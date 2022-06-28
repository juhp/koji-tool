{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Tasks (
  TaskFilter(..),
  TaskReq(..),
  BeforeAfter(..),
  tasksCmd,
  parseTaskState,
  kojiMethods,
  fedoraKojiHub,
  taskinfoUrl,
  Select(PkgsReq)
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
import Data.Time.Clock
import Data.Time.LocalTime
import Distribution.Koji
import Distribution.Koji.API
import Formatting hiding (now)
import Network.HTTP.Directory
import Network.HTTP.Simple
import SimpleCmd
import System.FilePath
import Text.Pretty.Simple

import Common
import Install
import Time
import User
import Utils

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

data TaskResult =
  TaskResult {taskPackage :: Either String NVR,
              taskArch :: String,
              _taskMethod :: String,
              _taskState :: TaskState,
              mtaskParent :: Maybe Int,
              taskId :: Int,
              _mtaskStartTime :: Maybe UTCTime,
              mtaskEndTime :: Maybe UTCTime
             }

-- FIXME short output option
-- --sibling
-- FIXME --tail-size option (eg more that 4000B)
-- FIXME --output-fields
tasksCmd :: Maybe String -> Maybe UserOpt -> Int -> [TaskState]
         -> [String] -> Maybe BeforeAfter -> Maybe String -> Bool -> Bool
         -> Maybe TaskFilter -> Bool -> Maybe Select -> TaskReq -> IO ()
tasksCmd mhub museropt limit !states archs mdate mmethod details debug mfilter' tail' minstall taskreq = do
  when (hub /= fedoraKojiHub && museropt == Just UserSelf) $
    error' "--mine currently only works with Fedora Koji: use --user instead"
  tz <- getCurrentTimeZone
  case taskreq of
    Task taskid -> do
      when (isJust museropt || isJust mdate || isJust mfilter') $
        error' "cannot use --task together with --user, timedate, or filter"
      mtask <- kojiGetTaskInfo hub (TaskId taskid)
      whenJust mtask$ \task -> do
        when debug $ pPrintCompact task
        whenJust (maybeTaskResult task) $ \res -> do
          let hasparent = isJust $ mtaskParent res
          printTask hasparent tz res
          if hasparent
            then whenJust minstall $ \installopts ->
            installCmd False debug No mhub Nothing False False False False False Nothing installopts Nothing ReqName [show taskid]
            else tasksCmd (Just hub) museropt limit states archs mdate mmethod details debug mfilter' tail' minstall (Parent taskid)
    Build bld -> do
      when (isJust mdate || isJust mfilter') $
        error' "cannot use --build together with timedate or filter"
      mtaskid <- if all isDigit bld
                then ((fmap TaskId . lookupStruct "task_id") =<<) <$> getBuild hub (InfoID (read bld))
                else kojiGetBuildTaskID hub bld
      whenJust mtaskid $ \(TaskId taskid) ->
        tasksCmd (Just hub) museropt limit states archs mdate mmethod details debug mfilter' tail' minstall (Parent taskid)
    Package pkg -> do
      when (head pkg == '-') $
        error' $ "bad combination: not a package " ++ pkg
      when (isJust mdate || isJust mfilter') $
        error' "cannot use --package together with timedate or filter"
      mpkgid <- getPackageID hub pkg
      case mpkgid of
        Nothing -> error' $ "no package id found for " ++ pkg
        Just pkgid -> do
          builds <- listBuilds hub
                    [("packageID", ValueInt pkgid),
                     commonBuildQueryOptions limit]
          forM_ builds $ \bld -> do
            let mtaskid = (fmap TaskId . lookupStruct "task_id") bld
            whenJust mtaskid $ \(TaskId taskid) ->
              tasksCmd (Just hub) museropt 10 states archs mdate mmethod details debug mfilter' tail' minstall (Parent taskid)
    Pattern pat -> do
      let buildquery = [("pattern", ValueString pat),
                        commonBuildQueryOptions limit]
      when debug $ print buildquery
      builds <- listBuilds hub buildquery
      when debug $ print builds
      forM_ builds $ \bld -> do
        let mtaskid = (fmap TaskId . lookupStruct "task_id") bld
        whenJust mtaskid $ \(TaskId taskid) ->
          tasksCmd (Just hub) museropt 10 states archs mdate mmethod details debug mfilter' tail' minstall (Parent taskid)
    _ -> do
      query <- setupQuery
      let queryopts = commonQueryOptions limit "-id"
      when debug $ print $ query ++ queryopts
      tasks <- listTasks hub query queryopts
      when debug $ mapM_ pPrintCompact tasks
      let exact = length tasks == 1
          detailed = details || exact
      (mapM_ (printTask detailed tz) . filterResults . mapMaybe maybeTaskResult) tasks
      whenJust minstall $ \args ->
        if exact
        then installCmd False debug No mhub Nothing False False False False False Nothing args Nothing ReqName [show (i :: Int) | i <- mapMaybe (lookupStruct "id") tasks]
        else error' "cannot install more than one task"
  where
    hub = maybe fedoraKojiHub hubURL mhub

    setupQuery = do
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
          mowner <- maybeGetKojiUser hub museropt
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

          capitalize :: String -> String
          capitalize "" = ""
          capitalize (h:t) = toUpper h : t

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
      let mstart_time = lookupTime False st
          mend_time = lookupTime True st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      state <- getTaskState st
      let pkgnvr = kojiTaskRequestPkgNVR st
          mparent' = lookupStruct "parent" st :: Maybe Int
      return $
        TaskResult pkgnvr arch method state mparent' taskid mstart_time mend_time

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
        -- FIX for parent/build method show children (like we do with taskid)
        (mapM_ putStrLn . formatTaskResult hub mtime tz) task
        buildlogSize debug tail' hub task
        else
        (putStrLn . compactTaskResult hub tz) task

    pPrintCompact =
#if MIN_VERSION_pretty_simple(4,0,0)
      pPrintOpt CheckColorTty
      (defaultOutputOptionsDarkBg {outputOptionsCompact = True,
                                   outputOptionsCompactParens = True})
#else
      pPrint
#endif

taskinfoUrl :: String -> Int -> String
taskinfoUrl hub tid =
  webUrl hub +/+ "taskinfo?taskID=" ++ show tid

-- FIXME option to hide url (take terminal width into consideration?)
compactTaskResult :: String -> TimeZone -> TaskResult -> String
compactTaskResult hub tz (TaskResult pkg arch method state _mparent taskid mstart mend) =
  let time =
        case mend of
          Just end -> compactZonedTime tz end
          Nothing -> maybe "" (compactZonedTime tz) mstart
  in
    unwords $
    [showPackage pkg ++ if method == "buildArch" then '.' : arch ++ replicate (8 - length arch) ' ' else ' ' : method,
     show state,
     time] ++
    ["(" ++ renderDuration True dur ++ ")" | Just start <- [mstart],  Just end <- [mend], let dur = diffUTCTime end start] ++
    [taskinfoUrl hub taskid]

-- FIXME show task owner
formatTaskResult :: String -> Maybe UTCTime -> TimeZone -> TaskResult -> [String]
formatTaskResult hub
#if MIN_VERSION_time(1,9,1)
  mtime
#else
  _mtime
#endif
  tz (TaskResult pkg arch method state mparent taskid mstart mend) =
  [ showPackage pkg ++ (if method == "buildArch" then '.' : arch else ' ' : method) +-+ show state
  , taskinfoUrl hub taskid +-+ maybe "" (\p -> "(parent: " ++ show p ++ ")") mparent] ++
  [formatLocalTime True tz start | Just start <- [mstart]] ++
  [formatLocalTime False tz end | Just end <- [mend]]
#if MIN_VERSION_time(1,9,1)
      ++
    case mtime of
      Just now ->
        ["current duration: " ++ renderDuration False dur | Just start <- [mstart], let dur = diffUTCTime now start]
      Nothing ->
        ["duration: " ++ renderDuration False dur | Just start <- [mstart], Just end <- [mend], let dur = diffUTCTime end start]

#endif

showPackage :: Either String NVR -> String
showPackage (Left p) = p
showPackage (Right nvr) = showNVR nvr

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

data LogFile = BuildLog | RootLog
  deriving Eq

data OutputLocation = PackagesOutput | WorkOutput

outputUrl :: String -> TaskResult -> OutputLocation -> Maybe String
outputUrl hub task loc =
  case loc of
    WorkOutput -> Just $ taskOutputUrl task
    PackagesOutput ->
      case taskPackage task of
        Left _ -> Nothing
        Right nvr ->
          Just $ buildOutputURL hub nvr +/+ "data/logs" +/+ taskArch task

findOutputURL :: String -> TaskResult -> IO (Maybe String)
findOutputURL hub task =
  case outputUrl hub task PackagesOutput of
    Just burl -> urlExistsOr (taskOutputUrl task) $
                 urlExistsOr burl $ return Nothing
    Nothing -> urlExistsOr (taskOutputUrl task) $ return Nothing
  where
    urlExistsOr :: String -> IO (Maybe String) -> IO (Maybe String)
    urlExistsOr url alt = do
      exists <- httpExists' url
      if exists
        then return $ Just url
        else alt

taskOutputUrl :: TaskResult -> String
taskOutputUrl task =
  "https://kojipkgs.fedoraproject.org/work/tasks" </> lastFew </> tid
  where
    tid = show (taskId task)

    lastFew =
      let few = dropWhile (== '0') $ takeEnd 4 tid
      in if null few then "0" else few

tailLogUrl :: String -> Int -> LogFile -> String
tailLogUrl hub taskid file =
  webUrl hub +/+ "getfile?taskID=" ++ show taskid ++ "&name=" ++ logFile file ++ "&offset=-4000"

logFile :: LogFile -> String
logFile RootLog = "root.log"
logFile BuildLog = "build.log"

buildlogSize :: Bool -> Bool -> String -> TaskResult -> IO ()
buildlogSize debug tail' hub task = do
  murl <- findOutputURL hub task
  whenJust murl $ \ url -> do
    let buildlog = url +/+ logFile BuildLog
    putStr $ buildlog ++ " "
    msize <- httpFileSize' buildlog
    whenJust msize $ \size -> do
      fprintLn ("(" % commas % "kB)") (size `div` 1000)
      -- FIXME check if short build.log ends with srpm
      file <-
        if size < 1500
        then do
          putStrLn $ url +/+ logFile RootLog
          return RootLog
        else return BuildLog
      when tail' $ displayLog url file
    when debug $ putStrLn buildlog
  where
    displayLog :: String -> LogFile -> IO ()
    displayLog url file = do
      let logurl =
            case file of
              RootLog -> url +/+  logFile file
              BuildLog -> tailLogUrl hub (taskId task) file
      req <- parseRequest logurl
      resp <- httpLBS req
      let out = U.toString $ getResponseBody resp
          ls = lines out
      putStrLn ""
      if file == RootLog
        then
        let excluded = ["Executing command:", "Child return code was: 0",
                        "child environment: None", "ensuring that dir exists:",
                        "touching file:", "creating dir:", "kill orphans"]
        in putStr $ unlines $ map (dropPrefix "DEBUG ") $ takeEnd 30 $
           filter (\l -> not (any (`isInfixOf` l) excluded)) ls
        else
        if last ls == "Child return code was: 0"
        then putStr out
        else putStr . unlines $
          case breakOnEnd ["Child return code was: 1"] ls of
            ([],ls') -> ls'
            (ls',_) -> ls'
      putStrLn $ "\n" ++ logurl

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
