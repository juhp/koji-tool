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
import Control.Monad.Extra (unless, when)

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
                  localTimeOfDay, NominalDiffTime, TimeZone, utcToLocalTime)
import Data.Tuple.Extra (uncurry3)
import Data.RPM.NVR
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Koji
import SimpleCmd
import System.FilePath ((</>))

import Time
import Utils

progressCmd :: Bool -> Int -> Bool -> [TaskID] -> IO ()
progressCmd debug waitdelay modules tids = do
  when (waitdelay < 1) $ error' "minimum interval is 1 min"
  when (modules && not (null tids)) $ error' "cannot combine --modules with tasks"
  tasks <-
    if null tids
    then kojiListBuildTasks $ if modules then Just "mbs/mbs.fedoraproject.org" else Nothing
    else return tids
  when (null tasks) $ error' "no build tasks found"
  btasks <- mapM kojiTaskinfoRecursive tasks
  tz <- getCurrentTimeZone
  loopBuildTasks debug tz waitdelay btasks

-- FIXME change to (TaskID,Struct,Size,Time)
data TaskInfoSizeTime =
  TaskInfoSizeTime { tistTask :: Struct,
                     tistSize :: Maybe Int,
                     _tistTime :: Maybe UTCTime
                   }

data BuildTask =
  BuildTask TaskID UTCTime (Maybe UTCTime) (Maybe Int) [TaskInfoSizeTime]

kojiTaskinfoRecursive :: TaskID -> IO BuildTask
kojiTaskinfoRecursive tid = do
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
        map (uncurry3 TaskInfoSizeTime) $
        zip3 children (repeat Nothing) (repeat Nothing)

type TaskInfoSizeTimes =
  (Struct,(Maybe Int, Maybe UTCTime),(Maybe Int, Maybe UTCTime))

loopBuildTasks :: Bool -> TimeZone -> Int -> [BuildTask] -> IO ()
loopBuildTasks _ _  _ [] = return ()
loopBuildTasks debug tz waitdelay bts = do
  curs <- filter tasksOpen <$> mapM runProgress bts
  unless (null curs) $ do
    threadDelayMinutes waitdelay
    news <- mapM updateBuildTask curs
    loopBuildTasks debug tz waitdelay news
  where
    threadDelayMinutes :: Int -> IO ()
    threadDelayMinutes m =
      -- convert minutes to microseconds
      threadDelay (fromEnum (fromIntegral (m * 60) :: Micro))

    -- FIXME use last-modified to predict next update
    runProgress :: BuildTask -> IO BuildTask
    runProgress (BuildTask tid start mend msize tasks) =
      case tasks of
        [] -> do
          state <- kojiGetTaskState fedoraKojiHub tid
          if state `elem` map Just openTaskStates
            then do
            threadDelayMinutes waitdelay
            kojiTaskinfoRecursive tid
            else return $ BuildTask tid start Nothing msize []
        (tist:_) -> do
          let task = tistTask tist
          when debug $ print task
          let epkgnvr = kojiTaskRequestPkgNVR task
          end <- maybe getCurrentTime return mend
          let duration = diffUTCTime end start
          logMsg $ either id showNVR epkgnvr ++ " (" ++ displayID tid ++ ")" +-+ maybe "" (\s -> show (s `div` 1000) ++ "kB,") msize +-+ renderDuration True duration
          sizes <- mapM buildlogSize tasks
          printLogSizes tz waitdelay sizes
          putStrLn ""
          let news = map (\(t,(s,ti),_) -> (TaskInfoSizeTime t s ti)) sizes
              (open,closed) = partition (\t -> getTaskState (tistTask t) `elem` map Just openTaskStates) news
              mlargest = if not (any (\t -> lookupStruct "method" (tistTask t) /= Just ("buildSRPMFromSCM" :: String)) closed)
                         then Nothing
                         else maximum $ map tistSize closed
              mbiggest = case (mlargest,msize) of
                           (Just large, Just size) ->
                             Just $ max large size
                           (Just large, Nothing) -> Just large
                           (Nothing,_) -> msize
          if null open
            then runProgress (BuildTask tid start mend mbiggest [])
            else return $ BuildTask tid start mend mbiggest open

    tasksOpen :: BuildTask -> Bool
    tasksOpen (BuildTask _ _ _ _ ts) = not (null ts)

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask (BuildTask tid start mend msize ts) = do
      news <- mapM updateTask ts
      return (BuildTask tid start mend msize news)

    updateTask :: TaskInfoSizeTime -> IO TaskInfoSizeTime
    updateTask (TaskInfoSizeTime task size time) = do
      let tid = fromJust (readID task)
      mnew <- kojiGetTaskInfo fedoraKojiHub tid
      case mnew of
        Nothing -> error' $ "TaskInfo not found for " ++ displayID tid
        Just new -> return (TaskInfoSizeTime new size time)

buildlogSize :: TaskInfoSizeTime -> IO TaskInfoSizeTimes
buildlogSize (TaskInfoSizeTime task oldsize oldtime) = do
  exists <- if isJust oldsize then return True
            else httpExists' buildlog
  (msize,mtime) <- if exists
                   then httpFileSizeTime' buildlog
                   else return (Nothing,Nothing)
  return (task,(fromInteger <$> msize,mtime),(oldsize,oldtime))
  where
    tid = show $ fromJust (readID' task)
    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"
    lastFew =
      let few = dropWhile (== '0') $ takeEnd 4 tid in
        if null few then "0" else few

data TaskOutput = TaskOut {_outArch :: Text,
                           moutSize :: Maybe Int,
                           _moutTime :: Maybe UTCTime,
                           moutSpeed :: Maybe Int,
                           _outState :: Text,
                           _method :: Text,
                           _mduration :: Maybe NominalDiffTime}

printLogSizes :: TimeZone -> Int -> [TaskInfoSizeTimes] -> IO ()
printLogSizes tz waitdelay tss =
  let (mxsi, mxsp, taskoutputs) = (formatSize . map taskOutput) tss
  in mapM_ (printTaskOut mxsi mxsp) taskoutputs
  where
    printTaskOut :: Int64 -> Int64 -> TaskOutput -> IO ()
    printTaskOut mxsi mxsp (TaskOut a msi mti msp st mth mdur) =
      fprintLn (rpadded 8 ' ' stext %
                lpadded mxsi ' ' (optioned commas) % "kB" % " " %
                parenthesised (optioned string) % " " %
                optioned ("[" % lpadded mxsp ' ' commas % " B/min]") % " " %
                stext % " " %
                optioned string % " " %
                stext)
      a
      ((`div` 1000) <$> msi)
      (show . localTimeOfDay . utcToLocalTime tz <$> mti)
      ((`div` waitdelay) <$> msp)
      st
      (renderDuration True <$> mdur)
      (abridgeMethod mth)

    formatSize :: [TaskOutput] -> (Int64, Int64,[TaskOutput])
    formatSize ts =
      let maxsi = maximum $ 0 : mapMaybe moutSize ts
                  -- was "198,689"
          maxsp = maximum $ 0 : mapMaybe moutSpeed ts
      in (decimalLength maxsi, decimalLength maxsp, ts)
      where
        decimalLength = fromIntegral . length . show

    abridgeMethod :: Text -> Text
    abridgeMethod mth =
      case mth of
        "buildArch" -> ""
        "buildSRPMFromSCM" -> "SRPM"
        _ -> mth

    taskOutput :: TaskInfoSizeTimes -> TaskOutput
    taskOutput (task, (size,time), (oldsize,_oldtime)) =
      let method = maybeVal "method not found" (lookupStruct "method") task :: Text
          arch = maybeVal "arch not found" (lookupStruct "arch") task :: Text
          diff = (-) <$> size <*> oldsize
          state = maybeVal "No state found" getTaskState task
          state' =
            if state == TaskOpen
            then ""
            else T.pack $ show state
        in TaskOut arch size time diff state' method (durationOfTask task)

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
