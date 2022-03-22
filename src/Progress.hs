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
import Control.Monad

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
import Data.Text (Text)
import qualified Data.Text as T

import Distribution.Koji

import SimpleCmd

import System.FilePath (takeBaseName, (</>))

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

kojiTaskinfoRecursive :: TaskID -> IO BuildTask
kojiTaskinfoRecursive tid = do
  mtaskinfo <- kojiGetTaskInfo fedoraKojiHub tid
  case mtaskinfo of
    Nothing -> error' $ "taskinfo not found for " ++ displayID tid
    Just taskinfo -> do
      parent <-
        case lookupStruct "method" taskinfo :: Maybe String of
          Nothing -> error' $ "no method found for " ++ displayID tid
          Just method ->
            case method of
              "build" -> return tid
              "buildArch" ->
                case lookupStruct "parent" taskinfo of
                  Nothing -> error' $ "no parent found for " ++ displayID tid
                  Just parent -> return (TaskId parent)
              _ -> error' $ "unsupport method: " ++ method
      children <- sortOn (\t -> lookupStruct "arch" t :: Maybe String) <$>
                          kojiGetTaskChildren fedoraKojiHub parent True
      return (tid, zip children (repeat Nothing))

type BuildTask = (TaskID, [TaskInfoSize])

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
    runProgress (tid,tasks) =
      case tasks of
        [] -> do
          state <- kojiGetTaskState fedoraKojiHub tid
          if state `elem` map Just openTaskStates then do
            threadDelayMinutes waitdelay
            kojiTaskinfoRecursive tid
            else return (tid,[])
        ((task,_):_) -> do
          putStrLn ""
          when debug $ print task
          let request = lookupStruct "request" task :: Maybe [Value]
          when debug $ print request
          let mnvr = case request of
                       Just (srpm:_) ->
                         takeBaseName . takeBaseName <$> getString srpm
                       _ -> Nothing
          logMsg $ fromMaybe "<unknown>" mnvr ++ " (" ++ displayID tid ++ ")"
          sizes <- mapM buildlogSize tasks
          printLogSizes waitdelay sizes
          let news = map (\(t,(s,_)) -> (t,s)) sizes
              open = filter (\ (t,_) -> getTaskState t `elem` map Just openTaskStates) news
          return (tid, open)

    tasksOpen :: BuildTask -> Bool
    tasksOpen (_,ts) = not (null ts)

    updateBuildTask :: BuildTask -> IO BuildTask
    updateBuildTask (tid, ts) = do
      news <- mapM updateTask ts
      return (tid, news)

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

data TaskOutput = TaskOut {_outArch :: Text, moutSize :: Maybe Int, moutSpeed :: Maybe Int, _outState :: Text, _method :: Text}

printLogSizes :: Int -> [TaskInfoSizes] -> IO ()
printLogSizes waitdelay tss =
  let (mxsi, mxsp, taskoutputs) = (formatSize . map taskOutput) tss
  in mapM_ (printTaskOut mxsi mxsp) taskoutputs
  where
    printTaskOut :: Int64 -> Int64 -> TaskOutput -> IO ()
    printTaskOut mxsi mxsp (TaskOut a msi msp st mth) =
      fprintLn (rpadded 8 ' ' stext % lpadded mxsi ' ' (optioned commas) % "kB" % optioned ("[" % lpadded mxsp ' ' commas % " B/min]") % " " % stext % " " % stext) a ((`div` 1000) <$> msi) ((`div` waitdelay) <$> msp) st (abridgeMethod mth)

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
          state' = if state == TaskOpen then "" else T.pack (show state)
        in TaskOut arch size diff state' method

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
