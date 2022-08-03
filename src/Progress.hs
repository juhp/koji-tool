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
import Data.Time (diffUTCTime, getCurrentTime, NominalDiffTime)
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
  loopBuildTasks debug waitdelay btasks

type BuildTask = (TaskID, UTCTime, Maybe UTCTime, Maybe Int, [TaskInfoSize])

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
      return (tid, start, mend, Nothing, zip children (repeat Nothing))

-- FIXME change to (TaskID,Struct,Size)
type TaskInfoSize = (Struct,Maybe Int)
type TaskInfoSizes = (Struct,(Maybe Int,Maybe Int))

loopBuildTasks :: Bool -> Int -> [BuildTask] -> IO ()
loopBuildTasks _ _ [] = return ()
loopBuildTasks debug waitdelay bts = do
  curs <- filter tasksOpen <$> mapM runProgress bts
  unless (null curs) $ do
    threadDelayMinutes waitdelay
    news <- mapM updateBuildTask curs
    loopBuildTasks debug waitdelay news
  where
    threadDelayMinutes :: Int -> IO ()
    threadDelayMinutes m =
      -- convert minutes to microseconds
      threadDelay (fromEnum (fromIntegral (m * 60) :: Micro))

    runProgress :: BuildTask -> IO BuildTask
    runProgress (tid,start,mend,msize,tasks) =
      case tasks of
        [] -> do
          state <- kojiGetTaskState fedoraKojiHub tid
          if state `elem` map Just openTaskStates
            then do
            threadDelayMinutes waitdelay
            kojiTaskinfoRecursive tid
            else return (tid, start, Nothing, msize, [])
        ((task,_):_) -> do
          when debug $ print task
          let epkgnvr = kojiTaskRequestPkgNVR task
          end <- maybe getCurrentTime return mend
          let duration = diffUTCTime end start
          logMsg $ either id showNVR epkgnvr ++ " (" ++ displayID tid ++ ")" +-+ maybe "" (\s -> show (s `div` 1000) ++ "kB,") msize +-+ renderDuration True duration
          sizes <- mapM buildlogSize tasks
          printLogSizes waitdelay sizes
          putStrLn ""
          let news = map (\(t,(s,_)) -> (t,s)) sizes
              (open,closed) = partition (\ (t,_) -> getTaskState t `elem` map Just openTaskStates) news
              mlargest = if null (filter (\(t,_) -> lookupStruct "method" t /= Just ("buildSRPMFromSCM" :: String)) closed)
                         then Nothing
                         else maximum $ map snd closed
              mbiggest = case (mlargest,msize) of
                           (Just large, Just size) -> Just $ max large size
                           (Just large, Nothing) -> Just large
                           (Nothing,_) -> msize
          if null open
            then runProgress (tid,start,mend,mbiggest,[])
            else return (tid, start, mend, mbiggest, open)

    tasksOpen :: BuildTask -> Bool
    tasksOpen (_,_,_,_,ts) = not (null ts)

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask (tid,start,mend,msize,ts) = do
      news <- mapM updateTask ts
      return (tid, start, mend, msize, news)

    updateTask :: TaskInfoSize -> IO TaskInfoSize
    updateTask (task,size) = do
      let tid = fromJust (readID task)
      mnew <- kojiGetTaskInfo fedoraKojiHub tid
      case mnew of
        Nothing -> error' $ "TaskInfo not found for " ++ displayID tid
        Just new -> return (new,size)

buildlogSize :: TaskInfoSize -> IO TaskInfoSizes
buildlogSize (task, old) = do
  exists <- if isJust old then return True
            else httpExists' buildlog
  size <- if exists then httpFileSize' buildlog else return Nothing
  return (task,(fromInteger <$> size,old))
  where
    tid = show $ fromJust (readID' task)
    buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> "build.log"
    lastFew =
      let few = dropWhile (== '0') $ takeEnd 4 tid in
        if null few then "0" else few

data TaskOutput = TaskOut {_outArch :: Text,
                           moutSize :: Maybe Int,
                           moutSpeed :: Maybe Int,
                           _outState :: Text,
                           _method :: Text,
                           _mduration :: Maybe NominalDiffTime}

printLogSizes :: Int -> [TaskInfoSizes] -> IO ()
printLogSizes waitdelay tss =
  let (mxsi, mxsp, taskoutputs) = (formatSize . map taskOutput) tss
  in mapM_ (printTaskOut mxsi mxsp) taskoutputs
  where
    printTaskOut :: Int64 -> Int64 -> TaskOutput -> IO ()
    printTaskOut mxsi mxsp (TaskOut a msi msp st mth mdur) =
      fprintLn (rpadded 8 ' ' stext %
                lpadded mxsi ' ' (optioned commas) % "kB" % " " %
                optioned ("[" % lpadded mxsp ' ' commas % " B/min]") % " " %
                stext % " " %
                optioned string % " " % stext)
      a
      ((`div` 1000) <$> msi)
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

    taskOutput :: TaskInfoSizes -> TaskOutput
    taskOutput (task, (size,old)) =
      let method = maybeVal "method not found" (lookupStruct "method") task :: Text
          arch = maybeVal "arch not found" (lookupStruct "arch") task :: Text
          diff = (-) <$> size <*> old
          state = maybeVal "No state found" getTaskState task
          state' =
            if state == TaskOpen
            then ""
            else T.pack $ show state
        in TaskOut arch size diff state' method (durationOfTask task)

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
