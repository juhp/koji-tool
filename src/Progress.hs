{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards #-}

module Progress (
  progressCmd,
  TaskID(..)
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (forM, liftM2, unless, when)

import Formatting

#if !MIN_VERSION_http_directory(0,1,5)
import Network.HTTP.Client (Manager)
#endif
import Network.HTTP.Directory

import Control.Concurrent (threadDelay)

import Data.Fixed
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR
import Data.Time (diffUTCTime, getCurrentTime, getCurrentTimeZone,
                  localTimeOfDay, NominalDiffTime, nominalDiffTimeToSeconds,
                  TimeZone, utcToLocalTime)
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Koji
import Safe (maximumMay)
import SimpleCmd

import Common (lookupArch)
import Tasks
import Time
import Utils

-- FIXME check parent status (not children) to drop
-- FIXME if failure and no more, then stop
-- FIXME catch HTTP exception for connection timeout
-- FIXME pick up new user builds (if none specified)
progressCmd :: Bool -> QueryOpts -> TaskReq -> IO ()
progressCmd modules queryopts@QueryOpts{..} taskreq = do
  tasks <- do
    tz <- getCurrentTimeZone
    let states =
          case taskreq of
            TaskQuery ->
              if null qStates
              then openTaskStates
              else qStates
            _ -> []
        mmethod =
          case taskreq of
            TaskQuery -> qmMethod <|> Just "build"
            _ -> Nothing
        muser =
          case taskreq of
            TaskQuery -> qmUserOpt
            _ -> Nothing
    ts <- getTasks tz fedoraKojiHub queryopts {qStates = states, qmMethod = mmethod, qmUserOpt = muser} taskreq
    if null ts
      then do
      tids <- kojiListBuildTasks $
              if modules
              then Just "mbs/mbs.fedoraproject.org"
              else Nothing
      forM tids $ \tid -> do
        mtaskinfo <- kojiGetTaskInfo fedoraKojiHub tid
        case mtaskinfo of
          Nothing -> error' $ "taskinfo not found for " ++ displayID tid
          Just taskinfo -> return taskinfo
      else do
      when modules $ error' "cannot combine --modules with tasks"
      return ts
  when (null tasks) $ error' "no build tasks found"
  btasks <- mapM initialBuildTask tasks
  tz <- getCurrentTimeZone
  loopBuildTasks qDebug tz btasks

data LogStatus = LogStatus
                 { logSize :: Int,
                   logTime :: UTCTime
                 }
  deriving Show

data TaskStatus = TaskStatus
                  { tstLog :: Maybe LogStatus,
                    tstState :: TaskState
                  }
  deriving Show

mkTaskStatus :: Maybe Int -> Maybe UTCTime -> Maybe TaskState -> Maybe TaskStatus
mkTaskStatus Nothing _ _ = Nothing
mkTaskStatus _ Nothing _ = Nothing
mkTaskStatus _ _ Nothing = Nothing
mkTaskStatus (Just size) (Just time) (Just state) =
  Just (TaskStatus (Just (LogStatus size time)) state)

-- FIXME change to (TaskID,Struct,Size,Time,State)
data TaskInfoStatus = TaskInfoStatus
                      { taskInfo :: Struct,
                        taskStatus :: Maybe TaskStatus}

data BuildTask =
  BuildTask TaskID UTCTime (Maybe UTCTime) (Maybe Int) [TaskInfoStatus]

initialBuildTask :: Struct -> IO BuildTask
initialBuildTask taskinfo = do
  let tid =
        case lookupStruct "id" taskinfo of
          Just tid' -> TaskId tid'
          Nothing -> error' $ "no taskid found for:" ++ show taskinfo
      parent =
        case lookupStruct "method" taskinfo :: Maybe String of
          Nothing -> error' $ "no method found for " ++ displayID tid
          Just method ->
            case method of
              "build" -> tid
              "buildArch" ->
                case lookupStruct "parent" taskinfo of
                  Nothing -> error' $ "no parent found for " ++ displayID tid
                  Just par -> TaskId par
              -- FIXME support newrepo
              -- https://koji.fedoraproject.org/koji/taskinfo?taskID=104446426
              -- https://kojipkgs.fedoraproject.org//work/tasks/6426/104446426/
              _ -> error' $ "unsupported method: " ++ method
  children <- sortOn lookupArch <$>
                      kojiGetTaskChildren fedoraKojiHub parent True
  let start =
        case lookupTime CreateEvent taskinfo of
          Nothing ->
            error' $ "task " ++ displayID tid ++ " has no create time"
          Just t -> t
      mend = lookupTime CompletionEvent taskinfo
  return $
    BuildTask parent start mend Nothing $
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
            mtaskinfo <- kojiGetTaskInfo fedoraKojiHub tid
            case mtaskinfo of
              Nothing -> error' $ "no taskinfo for " ++ show tid
              Just taskinfo -> initialBuildTask taskinfo
            else return $ BuildTask tid start Nothing msize []
        ((TaskInfoStatus task _):_) -> do
          when debug $ print task
          statuses <- mapM (buildlogSize debug 0) tasks
          end <- maybe getCurrentTime return mend
          let header =
                let pkg = either id showNVR $ kojiTaskRequestNVR task
                    duration = diffUTCTime end start
                in
                  -- FIXME mostly redundant for a single build task
                  logMsg $ pkg +-+ "(" ++ displayID tid ++ ")" +-+ maybe "" (\s -> show (s `div` 1000) ++ "kB,") msize +-+ renderDuration True duration
          printLogStatuses header tz statuses
          let news = map (\(task',(s,t),_) -> TaskInfoStatus task' (mkTaskStatus s t (getTaskState task'))) statuses
              (open,closed) = partition (\tis -> getTaskState (taskInfo tis) `elem` map Just openTaskStates) news
              mlargest = if not (any (\tis -> lookupStruct "method" (taskInfo tis) /= Just ("buildSRPMFromSCM" :: String)) closed)
                         then Nothing
                         else maximumMay $ mapMaybe (\t -> taskStatus t >>= tstLog <&> logSize) closed
              mbiggest = max mlargest msize
          if null open
            then runProgress (BuildTask tid start mend mbiggest [])
            else return $ BuildTask tid start mend mbiggest open

    -- getTaskState' task =
    --   case getTaskState task of
    --     Nothing -> error' $ "undefined task state: " ++ show task
    --     Just st -> st

buildlogSize :: Bool -> Int -> TaskInfoStatus -> IO TaskInfoStatuses
buildlogSize debug n (TaskInfoStatus task moldstatus) = do
  let tid = fromJust (lookupStruct "id" task)
      buildlog = buildlogUrlfromTaskId tid
  when debug $ putStrLn buildlog
  exists <- if isJust moldstatus
            then return True
            else httpExists' buildlog
  when (debug && n>0) $ putChar '.'
  waitDelay
  (msize,mtime) <- if exists
                   then httpFileSizeTime' buildlog
                   else return (Nothing,Nothing)
  when debug $ print (msize,mtime,moldstatus)
  if (mtime == fmap logTime moldlog || isNothing mtime) && n < 5
    then buildlogSize debug (n+1) (TaskInfoStatus task moldstatus)
    else
    return (task,
            (fromInteger <$> msize, mtime),
            moldstatus)
  where
    moldlog = moldstatus >>= tstLog :: Maybe LogStatus

    waitDelay ::  IO ()
    waitDelay = do
      case logTime <$> moldlog of
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


data TaskOutput = TaskOut {_outArch :: String,
                           moutSize :: Maybe Int,
                           moutSizeStep :: Maybe Int,
                           outSizeChanged :: Bool,
                           _moutTime :: Maybe UTCTime,
                           _moutTimeStep :: Maybe Int,
                           outTimeChanged :: Bool,
                           _outState :: Text,
                           outStateChanged :: Bool,
                           _method :: Text,
                           _mduration :: Maybe NominalDiffTime}

printLogStatuses :: IO () -> TimeZone -> [TaskInfoStatuses] -> IO ()
printLogStatuses header tz tss =
  let (mxsi, mxsp, taskoutputs) = (formatSize . map taskOutput) tss
  in
    unless (null taskoutputs) $
    when (any (\t -> outTimeChanged t || outSizeChanged t || outStateChanged t) taskoutputs) $ do
    header
    mapM_ (printTaskOut mxsi mxsp) taskoutputs
    putChar '\n'
  where
    printTaskOut :: Int64 -> Int64 -> TaskOutput -> IO ()
    printTaskOut maxsize maxspd (TaskOut arch msize msizediff _sizechanged mtime mtimediff _timechanged state _statechanged mthd mduration) =
      fprintLn (rpadded 8 ' ' string %
                lpadded (max 6 (maxsize+2)) ' ' (optioned commas) % "kB" %
                " " %
                optioned (parenthesised string % " ") %
                optioned ("[" % lpadded maxspd ' ' commas % " B/s] ") %
                optioned (parenthesised (shown % "s") % " ") %
                lpadded 8 ' ' (optioned (string % " ")) %
                stext % " " %
                stext)
      arch
      ((`div` 1000) <$> msize)
      (show . localTimeOfDay . utcToLocalTime tz <$> mtime)
      (if msizediff == Just 0
       then Just 0
       else liftM2 div msizediff mtimediff)
      mtimediff
      (renderDuration True <$> mduration)
      state
      (abridgeMethod mthd)

    formatSize :: [TaskOutput] -> (Int64, Int64,[TaskOutput])
    formatSize ts =
      let maxsi = maximum $ 0 : mapMaybe (fmap (`div` 1000) . moutSize) ts
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

    taskOutput :: TaskInfoStatuses -> TaskOutput
    taskOutput (task, (size,time), moldstatus) =
      let moldlog = moldstatus >>= tstLog
          oldtime = logTime <$> moldlog
          oldsize = logSize <$> moldlog
          oldstate = tstState <$> moldstatus
          mstate = getTaskState task
      in
        let method = maybeVal "method not found" (lookupStruct "method") task :: Text
            arch = maybeVal "arch not found" lookupArch task
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
        in TaskOut arch size sizediff (size /= oldsize) time timediff (time /= oldtime) state' (mstate /= oldstate) method (durationOfTask task)

kojiListBuildTasks :: Maybe String -> IO [TaskID]
kojiListBuildTasks muser = do
  user <- case muser of
            Just user -> return user
            Nothing -> do
              -- FIXME test for klist
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
