{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Tasks (
  TaskFilter(..),
  TaskReq(..),
  BeforeAfter(..),
  tasksCmd,
  parseTaskState,
  kojiMethods,
  fedoraKojiHub
  )
where

import Control.Monad.Extra
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (isDigit, toUpper)
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Format.Numbers
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji
import Distribution.Koji.API
import Network.HTTP.Directory
import Network.HTTP.Simple
import SimpleCmd
import System.FilePath
import Text.Pretty.Simple

import Common
import User

data TaskReq = Task Int | Parent Int | Build String | Package String
             | TaskQuery | Pattern String

data TaskFilter = TaskPackage String | TaskNVR String

data BeforeAfter = Before String | After String

instance Show BeforeAfter where
  show (Before _) = "before"
  show (After _) = "after"

getTimedate :: BeforeAfter -> String
getTimedate (Before s) = s
getTimedate (After s) = s

capitalize :: String -> String
capitalize "" = ""
capitalize (h:t) = toUpper h : t

-- FIXME short output summary
tasksCmd :: Maybe String -> Maybe UserOpt -> Int -> [TaskState]
         -> [String] -> Maybe BeforeAfter -> Maybe String -> Bool -> Bool
         -> Maybe TaskFilter -> Bool -> TaskReq -> IO ()
tasksCmd mhub museropt limit states archs mdate mmethod details debug mfilter' tail' taskreq = do
  let server = maybe fedoraKojiHub hubURL mhub
  when (server /= fedoraKojiHub && museropt == Just UserSelf) $
    error' "--mine currently only works with Fedora Koji"
  tz <- getCurrentTimeZone
  case taskreq of
    Task taskid -> do
      when (isJust museropt || isJust mdate || isJust mfilter') $
        error' "cannot use --task together with --user, timedate, or filter"
      mtask <- kojiGetTaskInfo server (TaskId taskid)
      whenJust mtask$ \task -> do
        when debug $ pPrintCompact task
        whenJust (maybeTaskResult task) $ printTask True tz
    Build bld -> do
      when (isJust mdate || isJust mfilter') $
        error' "cannot use --build together with timedate or filter"
      mtaskid <- if all isDigit bld
                then ((fmap TaskId . lookupStruct "task_id") =<<) <$> getBuild server (InfoID (read bld))
                else kojiGetBuildTaskID server bld
      whenJust mtaskid $ \(TaskId taskid) ->
        tasksCmd (Just server) museropt limit states archs mdate mmethod details debug mfilter' tail' (Parent taskid)
    Package pkg -> do
      when (head pkg == '-') $
        error' $ "bad combination: not a package " ++ pkg
      when (isJust mdate || isJust mfilter') $
        error' "cannot use --package together with timedate or filter"
      mpkgid <- getPackageID server pkg
      case mpkgid of
        Nothing -> error' $ "no package id found for " ++ pkg
        Just pkgid -> do
          builds <- listBuilds server
                    [("packageID", ValueInt pkgid),
                     commonBuildQueryOptions limit]
          forM_ builds $ \bld -> do
            let mtaskid = (fmap TaskId . lookupStruct "task_id") bld
            whenJust mtaskid $ \(TaskId taskid) ->
              tasksCmd (Just server) museropt 10 states archs mdate mmethod details debug mfilter' tail' (Parent taskid)
    Pattern pat -> do
      let buildquery = [("pattern", ValueString pat),
                        commonBuildQueryOptions limit]
      when debug $ print buildquery
      builds <- listBuilds server buildquery
      when debug $ print builds
      forM_ builds $ \bld -> do
        let mtaskid = (fmap TaskId . lookupStruct "task_id") bld
        whenJust mtaskid $ \(TaskId taskid) ->
          tasksCmd (Just server) museropt 10 states archs mdate mmethod details debug mfilter' tail' (Parent taskid)
    _ -> do
      query <- setupQuery server
      let queryopts = commonQueryOptions limit "-id"
      when debug $ print $ query ++ queryopts
      tasks <- listTasks server query queryopts
      when debug $ mapM_ pPrintCompact tasks
      let detailed = details || length tasks == 1
      (mapM_ (printTask detailed tz) . filterResults . mapMaybe maybeTaskResult) tasks
  where
    setupQuery server = do
      case taskreq of
        Parent parent ->
          return $ ("parent", ValueInt parent) : commonParams
        _ -> do
          mdatestring <-
            case mdate of
              Nothing -> return Nothing
              Just date -> Just <$> cmd "date" ["+%F %T%z", "--date=" ++ dateString date]
          when (isNothing mmethod) $
            warning "buildArch tasks"
          whenJust mdatestring $ \date ->
            warning $ maybe "" show mdate +-+ date
          mowner <- maybeGetKojiUser server museropt
          return $
            [("owner", ValueInt (getID owner)) | Just owner <- [mowner]] ++
            [("complete" ++ (capitalize . show) date, ValueString datestring) | Just date <- [mdate], Just datestring <- [mdatestring]] ++
            commonParams
        where
          commonParams =
            [("decode", ValueBool True)]
            ++ [("state", ValueArray (map taskStateToValue states)) | notNull states]
            ++ [("arch", ValueArray (map (ValueString . kojiArch) archs)) | notNull archs]
            ++ [("method", ValueString method) | let method = fromMaybe "buildArch" mmethod]

          kojiArch :: String -> String
          kojiArch "i686" = "i386"
          kojiArch "armv7hl" = "armhfp"
          kojiArch a = a

    dateString :: BeforeAfter -> String
    -- make time refer to past not future
    dateString beforeAfter =
      let timedate = getTimedate beforeAfter
      in case words timedate of
           [t] | t `elem` ["hour", "day", "week", "month", "year"] ->
                 "last " ++ t
           [t] | t `elem` ["today", "yesterday"] ->
                 t ++ " 00:00"
           [t] | any (lower t `isPrefixOf`) ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"] ->
                 "last " ++ t ++ " 00:00"
           [n,_unit] | all isDigit n -> timedate ++ " ago"
           _ -> timedate

    maybeTaskResult :: Struct -> Maybe TaskResult
    maybeTaskResult st = do
      arch <- lookupStruct "arch" st
      let mstart_time = readTime' <$> lookupStruct "start_ts" st
          mend_time = readTime' <$> lookupStruct "completion_ts" st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      state <- getTaskState st
      request <- lookupStruct "request" st
      let package =
            case (getString . head) request of
              Nothing -> Left $ unwords $ map showValue $ take 2 request
              Just req ->
                let file = takeFileName req
                in if ".src.rpm" `isSuffixOf` file
                   then Right $ readNVR $ removeSuffix ".src.rpm" file
                   else Left $ takeBaseName file
          mparent' = lookupStruct "parent" st :: Maybe Int
      return $
        TaskResult package arch method state mparent' taskid mstart_time mend_time
      where
        showValue :: Value -> String
        showValue (ValueString cs) = cs
        showValue (ValueInt i) = show i
        showValue val = show val

    filterResults :: [TaskResult] -> [TaskResult]
    filterResults ts =
      case mfilter' of
        Nothing -> ts
        Just (TaskPackage pkg) ->
          filter (isPackage pkg . taskPackage) ts
        Just (TaskNVR nvr) ->
          filter (isNVR nvr . taskPackage) ts
      where
        isPackage pkg (Left p) = takeBaseName p == pkg
        isPackage pkg (Right (NVR n _)) = n == pkg

        isNVR _ (Left _) = False
        isNVR nvr (Right nvr') = nvr `isPrefixOf` showNVR nvr'

    printTask :: Bool -> TimeZone -> TaskResult -> IO ()
    printTask detailed tz task = do
      let mendtime = mtaskEndTime task
      mtime <- if isNothing  mendtime
                 then Just <$> getCurrentTime
                 else return Nothing
      if detailed
        then do
        putStrLn ""
        (mapM_ putStrLn . formatTaskResult mtime tz) task
        buildlogSize tail' $ taskId task
        else
        (putStrLn . compactTaskResult tz) task

    pPrintCompact =
#if MIN_VERSION_pretty_simple(4,0,0)
      pPrintOpt CheckColorTty
      (defaultOutputOptionsDarkBg {outputOptionsCompact = True})
#else
      pPrint
#endif

-- FIXME option to hide url (take terminal width into consideration?)
compactTaskResult :: TimeZone -> TaskResult -> String
compactTaskResult tz (TaskResult pkg arch method state _mparent taskid mstart mend) =
  let time =
        case mend of
          Just end -> compactZonedTime tz end
          Nothing -> maybe "" (compactZonedTime tz) mstart
  in
    showPackage pkg ++ (if method == "buildArch" then '.' : arch ++ replicate (8 - length arch) ' ' else ' ' : method) +-+
    show state +-+ time +-+
    "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show taskid

-- FIXME show task owner
formatTaskResult :: Maybe UTCTime -> TimeZone -> TaskResult -> [String]
formatTaskResult mtime tz (TaskResult pkg arch method state mparent taskid mstart mend) =
  [ showPackage pkg ++ (if method == "buildArch" then '.' : arch else ' ' : method) +-+ show state
  , "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show taskid +-+ maybe "" (\p -> "(parent: " ++ show p ++ ")") mparent] ++
  [formatTime defaultTimeLocale "Start: %c" (utcToZonedTime tz start) | Just start <- [mstart]] ++
  [formatTime defaultTimeLocale "End:   %c" (utcToZonedTime tz end) | Just end <- [mend]]
#if MIN_VERSION_time(1,9,1)
      ++
    case mtime of
      Just now ->
        ["current duration: " ++ formatTime defaultTimeLocale "%Hh %Mm %Ss" dur | Just start <- [mstart],  let dur = diffUTCTime now start]
      Nothing ->
        ["duration: " ++ formatTime defaultTimeLocale "%Hh %Mm %Ss" dur | Just start <- [mstart],  Just end <- [mend], let dur = diffUTCTime end start]

#endif

showPackage :: Either String NVR -> String
showPackage (Left p) = p
showPackage (Right nvr) = showNVR nvr

data TaskResult =
  TaskResult {taskPackage :: Either String NVR,
              _taskArch :: String,
              _taskMethod :: String,
              _taskState :: TaskState,
              _mtaskParent :: Maybe Int,
              taskId :: Int,
              _mtaskStartTime :: Maybe UTCTime,
              mtaskEndTime :: Maybe UTCTime
             }

#if !MIN_VERSION_koji(0,0,3)
taskStateToValue :: TaskState -> Value
taskStateToValue = ValueInt . fromEnum

parseTaskState :: String -> TaskState
parseTaskState s =
  case lower s of
    "free" -> TaskFree
    "open" -> TaskOpen
    "close" -> TaskClosed
    "closed" -> TaskClosed
    "cancel" -> TaskCanceled
    "canceled" -> TaskCanceled
    "assigned" -> TaskAssigned
    "fail" -> TaskFailed
    "failed" -> TaskFailed
    _ -> error' $! "unknown task state: " ++ s
#endif

data LastLog = WholeBuild | BuildTail | RootLog
  deriving Eq

logUrl :: Int -> LastLog -> String
logUrl taskid lastlog =
  if lastlog == BuildTail
  then "https://koji.fedoraproject.org/koji/getfile?taskID=" ++ tid ++ "&name=build.log&offset=-4000"
  else "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid </> logName <.> "log"
  where
    tid = show taskid

    lastFew =
      let few = dropWhile (== '0') $ takeEnd 4 tid
      in if null few then "0" else few

    logName = if lastlog == RootLog then "root" else "build"

buildlogSize :: Bool -> Int -> IO ()
buildlogSize tail' taskid = do
  let buildlog = logUrl taskid WholeBuild
  exists <- httpExists' buildlog
  when exists $ do
    putStr $ buildlog ++ " "
    msize <- httpFileSize' buildlog
    whenJust msize $ \size -> do
      putChar '('
      (T.putStr . kiloBytes) size
      putStrLn ")"
      -- FIXME check if short build.log ends with srpm
      lastlog <-
        if size < 1500
        then do
          putStrLn $ logUrl taskid RootLog
          return RootLog
        else return BuildTail
      when tail' $
        displayLog taskid lastlog
  where
    kiloBytes s = prettyI (Just ',') (fromInteger s `div` 1000) <> T.pack "kB"

kojiMethods :: [String]
kojiMethods =
  nub . sort $
  -- www/kojiweb/index.py _TASKS
  (["build",
    "buildSRPMFromSCM",
    "rebuildSRPM",
    "buildArch",
    "chainbuild",
    "maven",
    "buildMaven",
    "chainmaven",
    "wrapperRPM",
    "winbuild",
    "vmExec",
    "waitrepo",
    "tagBuild",
    "newRepo",
    "createrepo",
    "distRepo",
    "createdistrepo",
    "buildNotification",
    "tagNotification",
    "dependantTask",
    "livecd",
    "createLiveCD",
    "appliance",
    "createAppliance",
    "image",
    "indirectionimage",
    "createImage",
    "livemedia",
    "createLiveMedia",
    "buildContainer"]
   ++
   -- https://koji.fedoraproject.org/koji/tasks
    ["all",
     "appliance",
     "build",
     "buildArch",
     "buildContainer",
     "buildNotification",
     "buildSRPMFromSCM",
     "chainbuild",
     "chainmaven",
     "createAppliance",
     "createContainer",
     "createImage",
     "createLiveCD",
     "createLiveMedia",
     "createdistrepo",
     "createrepo",
     "dependantTask",
     "distRepo",
     "image",
     "indirectionimage",
     "livecd",
     "livemedia",
     "newRepo",
     "rebuildSRPM",
     "runroot",
     "tagBuild",
     "tagNotification",
     "waitrepo"])

displayLog :: Int -> LastLog -> IO ()
displayLog tid lastlog = do
  req <- parseRequest $ logUrl tid lastlog
  resp <- httpLBS req
  let out = U.toString $ getResponseBody resp
      ls = lines out
  putStrLn ""
  if lastlog == RootLog
    then
    let excluded = ["Executing command:", "Child return code was: 0",
                    "child environment: None", "ensuring that dir exists:",
                    "touching file:", "creating dir:", "kill orphans"]
    in putStr $ unlines $ map (dropPrefix "DEBUG ") $ takeEnd 30 $
       filter (\l -> not (any (`isInfixOf` l) excluded)) ls
    else
    if last ls == "Child return code was: 0"
    then putStr out
    else do
      let err = "Child return code was: 1"
      putStr $ unlines $ takeWhile (err /=) ls
      putStrLn err
