{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Progress (
  progressCmd,
  TaskID(..)
  )
where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad.Extra (liftM2, unless, when)

import Formatting

#if !MIN_VERSION_http_directory(0,1,5)
import Network.HTTP.Client (Manager)
#endif
import Network.HTTP.Directory

import Control.Concurrent (threadDelay)

import Data.Fixed
import Data.Int (Int64)
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Time (diffUTCTime, getCurrentTime, getCurrentTimeZone,
                  localTimeOfDay, NominalDiffTime, nominalDiffTimeToSeconds,
                  TimeZone, utcToLocalTime)
import Data.RPM.NVR
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Koji
import SimpleCmd
import System.FilePath ((</>))

import Time
import Utils

-- FIXME if failure and no more, then stop
-- FIXME catch HTTP exception for connection timeout
progressCmd :: Bool -> Bool -> [TaskID] -> IO ()
progressCmd debug modules tids = do
  when (modules && not (null tids)) $ error' "cannot combine --modules with tasks"
  tasks <-
    if null tids
    then kojiListBuildTasks $ if modules then Just "mbs/mbs.fedoraproject.org" else Nothing
    else return tids
  when (null tasks) $ error' "no build tasks found"
  btasks <- mapM initialTaskinfo tasks
  tz <- getCurrentTimeZone
  loopBuildTasks debug tz btasks

data TaskStatus = TaskStatus
                     { tstSize :: Int,
                       tstTime :: UTCTime,
                       tstState :: TaskState
                     }
  deriving Show

mkTaskStatus :: Maybe Int -> Maybe UTCTime -> Maybe TaskState -> Maybe TaskStatus
mkTaskStatus Nothing _ _ = Nothing
mkTaskStatus _ Nothing _ = Nothing
mkTaskStatus _ _ Nothing = Nothing
mkTaskStatus (Just size) (Just time) (Just state) =
  Just (TaskStatus size time state)

-- FIXME change to (TaskID,Struct,Size,Time,State)
data TaskInfoStatus = TaskInfoStatus
                      { taskInfo :: Struct,
                        taskStatus :: Maybe TaskStatus}

data BuildTask =
  BuildTask TaskID UTCTime (Maybe UTCTime) (Maybe Int) [TaskInfoStatus]

initialTaskinfo :: TaskID -> IO BuildTask
initialTaskinfo tid = do
  mtaskinfo <- kojiGetTaskInfo fedoraKojiHub tid
  case mtaskinfo of
    Nothing -> error' $ "taskinfo not found for " ++ displayID tid
    Just taskinfo -> do
      let parent =
            case lookupStruct "method" taskinfo :: Maybe String of
              Nothing -> error' $ "no method found for " ++ displayID tid
              Just method ->
                case method of
                  "build" -> tid
                  "buildArch" ->
                    case lookupStruct "parent" taskinfo of
                      Nothing -> error' $ "no parent found for " ++ displayID tid
                      Just par -> TaskId par
                  _ -> error' $ "unsupported method: " ++ method
      children <- sortOn (\t -> lookupStruct "arch" t :: Maybe String) <$>
                          kojiGetTaskChildren fedoraKojiHub parent True
      let start =
            case lookupTime False taskinfo of
              Nothing ->
                error' $ "task " ++ displayID tid ++ " has no start time"
              Just t -> t
          mend = lookupTime True taskinfo
      return $
        BuildTask tid start mend Nothing $
        map (`TaskInfoStatus` Nothing) children

type TaskInfoStatuses = (Struct,
                         (Maybe Int, Maybe UTCTime),
                         Maybe TaskStatus)

loopBuildTasks :: Bool -> TimeZone -> [BuildTask] -> IO ()
loopBuildTasks _ _ [] = return ()
loopBuildTasks debug tz bts = do
  curs <- filter tasksOpen <$> mapM runProgress bts
  unless (null curs) $ do
    news <- mapM updateBuildTask curs
    loopBuildTasks debug tz news
  where
    tasksOpen :: BuildTask -> Bool
    tasksOpen (BuildTask _ _ _ _ ts) = not (null ts)

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask (BuildTask tid start mend msize ts) = do
      news <- mapM updateTask ts
      return (BuildTask tid start mend msize news)

    updateTask :: TaskInfoStatus -> IO TaskInfoStatus
    updateTask (TaskInfoStatus task mstatus) = do
      let tid = fromJust (readID task)
      mnew <- kojiGetTaskInfo fedoraKojiHub tid
      case mnew of
        Nothing -> error' $ "TaskInfo not found for " ++ displayID tid
        Just new -> return (TaskInfoStatus new mstatus)

    -- FIXME use last-modified to predict next update
    runProgress :: BuildTask -> IO BuildTask
    runProgress (BuildTask tid start mend msize tasks) =
      case tasks of
        [] -> do
          state <- kojiGetTaskState fedoraKojiHub tid
          if state `elem` map Just openTaskStates
            then do
            threadDelaySeconds 61
            initialTaskinfo tid
            else return $ BuildTask tid start Nothing msize []
        ((TaskInfoStatus task _):_) -> do
          when debug $ print task
          statuses <- mapM (buildlogSize debug 0) tasks
          end <- maybe getCurrentTime return mend
          let header =
                let epkgnvr = kojiTaskRequestPkgNVR task
                    duration = diffUTCTime end start
                in
                  -- FIXME mostly redundant for a single build task
                  logMsg $ either id showNVR epkgnvr ++ " (" ++ displayID tid ++ ")" +-+ maybe "" (\s -> show (s `div` 1000) ++ "kB,") msize +-+ renderDuration True duration
          printLogStatuses header tz statuses
          let news = map (\(task',(s,t),_) -> TaskInfoStatus task' (mkTaskStatus s t (getTaskState task'))) statuses
              (open,closed) = partition (\tis -> getTaskState (taskInfo tis) `elem` map Just openTaskStates) news
              mlargest = if not (any (\tis -> lookupStruct "method" (taskInfo tis) /= Just ("buildSRPMFromSCM" :: String)) closed)
                         then Nothing
                         else Just $ maximum $ mapMaybe (fmap tstSize . taskStatus) closed
              mbiggest = case (mlargest,msize) of
                           (Just large, Just size) ->
                             Just $ max large size
                           (Just large, Nothing) -> Just large
                           (Nothing,_) -> msize
          if null open
            then runProgress (BuildTask tid start mend mbiggest [])
            else return $ BuildTask tid start mend mbiggest open

    -- getTaskState' task =
    --   case getTaskState task of
    --     Nothing -> error' $ "undefined task state: " ++ show task
    --     Just st -> st

buildlogSize :: Bool -> Int -> TaskInfoStatus -> IO TaskInfoStatuses
buildlogSize debug n (TaskInfoStatus task oldstatus) = do
  exists <- if isJust oldstatus
            then return True
            else httpExists' buildlog
  when (debug && n>0) $ putChar '.'
  waitDelay
  (msize,mtime) <- if exists
                   then httpFileSizeTime' buildlog
                   else return (Nothing,Nothing)
  when debug $ print (mtime,oldstatus)
  if (mtime == fmap tstTime oldstatus || isNothing mtime) && n < 6
    then buildlogSize debug (n+1) (TaskInfoStatus task oldstatus)
    else
    return (task,
            (fromInteger <$> msize, mtime),
            oldstatus)
  where
    tid = show $ fromJust (readID' task)
    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"
    lastFew =
      let few = dropWhile (== '0') $ takeEnd 4 tid in
        if null few then "0" else few

    waitDelay ::  IO ()
    waitDelay = do
      case tstTime <$> oldstatus of
        Nothing -> when (n>0) $ threadDelaySeconds n
        Just ot -> do
          cur <- getCurrentTime
          let delay = delayTime (diffUTCTime cur ot)
          when debug $ print delay
          threadDelaySeconds delay

    delayTime :: NominalDiffTime -> Int
    delayTime dt =
      -- FIXME vary by small amounts
      let expect = 132 :: Pico
          lag = nominalDiffTimeToSeconds dt
      in if lag > expect
         then n
         else fromEnum (expect - lag ) `div` trillion

million, trillion :: Int
million = 1000000
trillion = million * million

secDuration :: NominalDiffTime -> Int
secDuration dt =
  let lag = nominalDiffTimeToSeconds dt
  in fromEnum lag `div` trillion

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds m =
  -- convert seconds to microseconds
  threadDelay (fromEnum (fromIntegral m :: Micro))


data TaskOutput = TaskOut {_outArch :: Text,
                           moutSize :: Maybe Int,
                           moutSizeStep :: Maybe Int,
                           _moutSizeChanged :: Bool,
                           _moutTime :: Maybe UTCTime,
                           _moutTimeStep :: Maybe Int,
                           _moutTimeChanged :: Bool,
                           _outState :: Text,
                           _stateChange :: Bool,
                           _method :: Text,
                           _mduration :: Maybe NominalDiffTime}

printLogStatuses :: IO () -> TimeZone -> [TaskInfoStatuses] -> IO ()
printLogStatuses header tz tss =
  let (mxsi, mxsp, taskoutputs) = (formatSize . mapMaybe taskOutput) tss
  in
    unless (null taskoutputs) $ do
    header
    mapM_ (printTaskOut mxsi mxsp) taskoutputs
    putChar '\n'
  where
    printTaskOut :: Int64 -> Int64 -> TaskOutput -> IO ()
    printTaskOut maxsize maxspd (TaskOut arch msize msizediff sizechanged mtime mtimediff timechanged state statechanged mthd mduration) =
      when (timechanged || sizechanged || statechanged) $
      fprintLn (rpadded 7 ' ' stext %
                lpadded maxsize ' ' (optioned commas) % "kB" % " " %
                parenthesised (optioned string) % " " %
                optioned ("[" % lpadded maxspd ' ' commas % " B/s]") %
                optioned (" (" % shown % "s) ") %
                stext % " " %
                optioned string % " " %
                stext)
      arch
      ((`div` 1000) <$> msize)
      (show . localTimeOfDay . utcToLocalTime tz <$> mtime)
      (liftM2 div msizediff mtimediff)
      mtimediff
      state
      (renderDuration True <$> mduration)
      (abridgeMethod mthd)

    formatSize :: [TaskOutput] -> (Int64, Int64,[TaskOutput])
    formatSize ts =
      let maxsi = maximum $ 0 : mapMaybe moutSize ts
          maxsp = maximum $ 0 : mapMaybe moutSizeStep ts
      in (decimalLength maxsi, decimalLength maxsp, ts)
      where
        decimalLength = fromIntegral . length . show

    abridgeMethod :: Text -> Text
    abridgeMethod mth =
      case mth of
        "buildArch" -> ""
        "buildSRPMFromSCM" -> "SRPM"
        _ -> mth

    taskOutput :: TaskInfoStatuses -> Maybe TaskOutput
    taskOutput (task, (size,time), oldstatus) =
      let oldtime = tstTime <$> oldstatus
          oldsize = tstSize <$> oldstatus
          oldstate = tstState <$> oldstatus
          mstate = getTaskState task
      in
      if time == oldtime && size == oldsize && mstate == oldstate
      then Nothing
      else
        let method = maybeVal "method not found" (lookupStruct "method") task :: Text
            arch = maybeVal "arch not found" (lookupStruct "arch") task :: Text
            sizediff = liftM2 (-) size oldsize
            timediff = if time == oldtime
                       then Nothing
                       else secDuration <$> liftM2 diffUTCTime time oldtime
            state' =
              case mstate of
                Nothing -> error' "No state found"
                Just s ->
                  if s == TaskOpen
                  then ""
                  else T.pack $ show s
        in Just $
           TaskOut arch size sizediff (size /= oldsize) time timediff (time /= oldtime) state' (mstate /= oldstate) method (durationOfTask task)

kojiListBuildTasks :: Maybe String -> IO [TaskID]
kojiListBuildTasks muser = do
  user <- case muser of
            Just user -> return user
            Nothing -> do
              mfasid <- (removeSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
              case mfasid of
                Just fas -> return fas
                Nothing -> error' "Could not determine FAS id from klist"
  mowner <- kojiGetUserID fedoraKojiHub user
  case mowner of
    Nothing -> error "No owner found"
    Just owner ->
      kojiListTaskIDs fedoraKojiHub
      [("method", ValueString "build"), ("owner", ValueInt (getID owner)), ("state", openTaskValues)]
      [("limit", ValueInt 10)]

maybeVal :: Show a => String -> (a -> Maybe b) -> a -> b
maybeVal err f v = fromMaybe (error (err ++ ": " ++ show v)) $ f v
