{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Tasks (
  TaskFilter(..),
  TaskReq(..),
  BeforeAfter(..),
  QueryOpts(..),
  Details(..),
  tasksCmd,
  getTasks,
  parseTaskState',
  kojiMethods,
  fedoraKojiHub,
  taskinfoUrl,
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

data TaskReq = Task Int | ChildrenOf Int | ParentOf Int
             | Build String | Package String
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
              taskMethod :: String,
              taskState :: TaskState,
              _mtaskParent :: Maybe Int,
              taskId :: Int,
              _taskCreateTime :: UTCTime,
              _mtaskStartTime :: Maybe UTCTime,
              mtaskEndTime :: Maybe UTCTime,
              _mtaskOwner :: Maybe String
             }

data QueryOpts = QueryOpts {
  qmUserOpt :: Maybe UserOpt,
  qLimit :: Maybe Int,
  qStates :: ![TaskState],
  qArchs :: ![String],
  qmDate :: Maybe BeforeAfter,
  qmMethod :: Maybe String,
  qDebug :: Bool,
  qmFilter :: Maybe TaskFilter}

data Details = Detailed | Concise
  deriving Eq

-- FIXME 'koji-tool tasks <taskid> -s fail -m buildarch' should not print build task
-- FIXME short output option
-- --sibling
-- FIXME --tail-size option (eg more that 4000B)
-- FIXME --output-fields
-- FIXME default to 'build' for install or try 'build' after 'buildarch'?
-- FIXME `-# 2` etc to select second result
tasksCmd :: Maybe String -> QueryOpts -> Maybe Details -> Bool -> Bool
         -> Maybe String -> Maybe Select -> TaskReq -> IO ()
tasksCmd mhub queryopts@QueryOpts{..} mdetails tail' hwinfo mgrep minstall taskreq = do
  when (hub /= fedoraKojiHub && qmUserOpt == Just UserSelf) $
    error' "--mine currently only works with Fedora Koji: use --user instead"
  tz <- getCurrentTimeZone
  tasks <- getTasks tz hub queryopts taskreq
  when qDebug $ mapM_ pPrintCompact tasks
  let details =
        case mdetails of
          Nothing ->
            if length tasks == 1 then Detailed else Concise
          Just detail -> detail
  (mapM_ (printTask details tz) . filterResults . mapMaybe maybeTaskResult) tasks
  where
    hub = maybe fedoraKojiHub hubURL mhub

    filterResults :: [TaskResult] -> [TaskResult]
    filterResults ts =
      case qmFilter of
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

    printTask :: Details -> TimeZone -> TaskResult -> IO ()
    printTask details tz task = do
      let mendtime = mtaskEndTime task
      mtime <- if isNothing  mendtime
                 then Just <$> getCurrentTime
                 else return Nothing
      if details == Detailed
        then do
        putStrLn ""
        -- FIX for parent/build method show children (like we do with taskid)
        (mapM_ putStrLn . formatTaskResult hub mtime tz) task
        if taskMethod task == "build"
          then do
          when (mdetails == Just Detailed) $
            getTasks tz hub queryopts (ChildrenOf $ taskId task) >>=
            mapM_ (printTask details tz) . mapMaybe maybeTaskResult
          else buildlogSize qDebug tz tail' hwinfo mgrep hub task
        else do
        (putStrLn . compactTaskResult hub tz) task
        when (tail' || hwinfo || isJust mgrep) $
          buildlogSize qDebug tz tail' hwinfo mgrep hub task
      whenJust minstall $ \installopts -> do
        putStrLn ""
        installCmd False qDebug No (Just hub) Nothing False False False Nothing [] Nothing Nothing installopts Nothing ReqName [show (taskId task)]

maybeTaskResult :: Struct -> Maybe TaskResult
maybeTaskResult st = do
  arch <- lookupArch st
  let (create,mstart,mend) = lookupTaskTimes st
  taskid <- lookupStruct "id" st
  method <- lookupStruct "method" st
  state <- getTaskState st
  let pkgnvr = kojiTaskRequestNVR st
      mparent' = lookupStruct "parent" st :: Maybe Int
  -- FIXME filter long names like
  -- "koschei/koschei-backend01.iad2.fedoraproject.org"
  -- "bpeck/jenkins-continuous-infra.apps.ci.centos.org"
      mowner = lookupStruct "owner_name" st
  return $
    TaskResult pkgnvr arch method state mparent' taskid create mstart mend mowner

pPrintCompact :: Struct -> IO ()
pPrintCompact =
#if MIN_VERSION_pretty_simple(4,0,0)
  pPrintOpt CheckColorTty
  (defaultOutputOptionsDarkBg {outputOptionsCompact = True,
                               outputOptionsCompactParens = True})
#else
  pPrint
#endif

defaultTaskMethod :: String
defaultTaskMethod = "buildArch"

-- FIXME more debug output
getTasks :: TimeZone -> String -> QueryOpts -> TaskReq -> IO [Struct]
getTasks tz hub queryopts@QueryOpts {..} req =
  case req of
    Task taskid -> do
      when (isJust qmUserOpt) $
        error' "cannot use taskid together with --user"
      when (isJust qmDate) $
        error' "cannot use taskid together with timedate"
      when (isJust qmFilter) $
        error' "cannot use taskid together with filter"
      mtask <- kojiGetTaskInfo hub (TaskId taskid)
      case mtask of
        Nothing -> error $ "taskid not found:" +-+ show taskid
        Just task -> do
          when qDebug $ pPrintCompact task
          case maybeTaskResult task of
            Nothing -> error' $ "failed to read task:" +-+ show task
            -- FIXME maybe should have way to list parent or children
            Just _res -> return [task]
    Build bld -> do
      when (isJust qmDate || isJust qmFilter) $
        error' "cannot use --build together with timedate or filter"
      mtaskid <- if all isDigit bld
                    -- FIXME use kojiGetBuildIdTaskID after next koji release
                 then ((fmap TaskId . lookupStruct "task_id") =<<) <$>
                      getBuild hub (InfoID (read bld))
                 else kojiGetBuildTaskID hub bld
      case mtaskid of
        Just (TaskId taskid) -> getTasks tz hub queryopts $ ChildrenOf taskid
        Nothing -> error' $ "no taskid found for build" +-+ bld
    Package pkg -> do
      when (head pkg == '-') $
        error' $ "bad combination: not a package" +-+ pkg
      when (isJust qmDate || isJust qmFilter) $
        -- FIXME why not?
        error' "cannot use package together with timedate or filter"
      mpkgid <- getPackageID hub pkg
      case mpkgid of
        Nothing -> error' $ "no package id found for" +-+ pkg
        Just pkgid -> do
          builds <- listBuilds hub
                    [("packageID", ValueInt pkgid),
                     commonBuildQueryOptions qLimit]
          fmap concat <$>
            forM builds $ \bld -> do
            let mtaskid = (fmap TaskId . lookupStruct "task_id") bld
            case mtaskid of
              -- FIXME gives too many tasks (parent builds):
              Just (TaskId taskid) -> getTasks tz hub queryopts {qLimit = Nothing} $ ChildrenOf taskid
              Nothing -> return []
    ParentOf taskid -> do
      mtask <- kojiGetTaskInfo hub (TaskId taskid)
      case mtask of
        Nothing -> error $ "taskid not found:" +-+ show taskid
        Just task -> do
          when qDebug $ pPrintCompact task
          case lookupStruct "parent" task of
            Nothing -> error' $ "no parent of" +-+ show taskid
            Just tid -> getTasks tz hub queryopts {qLimit = Nothing} $ Task tid
    Pattern pat -> do
      let buildquery = [("pattern", ValueString pat),
                        commonBuildQueryOptions qLimit]
      when qDebug $ print buildquery
      builds <- listBuilds hub buildquery
      when qDebug $ print builds
      fmap concat <$>
        forM builds $ \bld -> do
        let mtaskid = (fmap TaskId . lookupStruct "task_id") bld
        case mtaskid of
          Just (TaskId taskid) -> getTasks tz hub (queryopts {qLimit = Nothing}) $ ChildrenOf taskid
          Nothing -> return []
    _ -> do
      query <- setupQuery
      let qopts = commonQueryOptions qLimit "-id"
      when qDebug $ print $ query ++ qopts
      listTasks hub query qopts
  where
    setupQuery = do
      case req of
        ChildrenOf parent ->
          return $ ("parent", ValueInt parent) : commonParams
        _ -> do
          mdatestring <-
            case qmDate of
              Nothing -> return Nothing
              Just date -> Just <$> cmd "date" ["+%F %T%z", "--date=" ++ dateString date]
          when (isNothing qmMethod) $
            warning $ defaultTaskMethod +-+ "tasks"
          whenJust mdatestring $ \date ->
            warning $ maybe "" show qmDate +-+ date
          mowner <- maybeGetKojiUser hub qmUserOpt
          return $
            [("owner", ValueInt (getID owner)) | Just owner <- [mowner]] ++
            [("complete" ++ (capitalize . show) date, ValueString datestring) | Just date <- [qmDate], Just datestring <- [mdatestring]] ++
            commonParams
        where
          commonParams =
            [("decode", ValueBool True)]
            ++ [("state", ValueArray (map taskStateToValue qStates)) | notNull qStates]
            ++ [("arch", ValueArray (map (ValueString . kojiArch) qArchs)) | notNull qArchs]
            ++ [("method", ValueString method) | let method = fromMaybe defaultTaskMethod qmMethod]

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
                       "last" +-+ t
                 [t] | t `elem` ["today", "yesterday"] ->
                       t ++ " 00:00"
                 [t] | any (lower t `isPrefixOf`) ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"] ->
                       "last" +-+ t ++ " 00:00"
                 [n,_unit] | all isDigit n -> timedate ++ " ago"
                 _ -> timedate

taskinfoUrl :: String -> Int -> String
taskinfoUrl hub tid =
  webUrl hub +/+ "taskinfo?taskID=" ++ show tid

-- FIXME option to hide url (take terminal width into consideration?)
compactTaskResult :: String -> TimeZone -> TaskResult -> String
compactTaskResult hub tz (TaskResult pkg arch method state _mparent taskid _create mstart mend _mowner) =
  let time =
        case mend of
          Just end -> compactZonedTime tz end
          Nothing -> maybe "" (compactZonedTime tz) mstart
  in
    unwords $
    [time,
     taskinfoUrl hub taskid] ++
    ["(" ++ renderDuration True dur ++ ")" | Just start <- [mstart],  Just end <- [mend], let dur = diffUTCTime end start] ++
    [show state,
     showPackage pkg ++ if method == "buildArch" then '.' : arch ++ replicate (8 - length arch) ' ' else ' ' : method]

formatTaskResult :: String -> Maybe UTCTime -> TimeZone -> TaskResult -> [String]
formatTaskResult hub
#if MIN_VERSION_time(1,9,1)
  mtime
#else
  _mtime
#endif
  tz (TaskResult pkg arch method state mparent taskid create mstart mend mowner) =
  [ showPackage pkg ++ (if method == "buildArch" then '.' : arch else ' ' : method) +-+ show state +-+ maybe "" (\o -> '(' : o ++ ")") mowner
  , taskinfoUrl hub taskid +-+ maybe "" (\p -> "(parent:" +-+ show p ++ ")") mparent
  , formatLocalTime CreateEvent tz create] ++
  [formatLocalTime StartEvent tz start | Just start <- [mstart]] ++
  [formatLocalTime CompletionEvent tz end | Just end <- [mend]]
#if MIN_VERSION_time(1,9,1)
    ++
    ["delay:" +-+ renderDuration False dur | Just start <- [mstart], let dur = diffUTCTime start create]
    ++
    case mtime of
      Just now ->
        ["current duration:" +-+ renderDuration False dur | Just start <- [mstart], let dur = diffUTCTime now start]
      Nothing ->
        ["duration:" +-+ renderDuration False dur | Just start <- [mstart], Just end <- [mend], let dur = diffUTCTime end start]
#endif

showPackage :: Either String NVR -> String
showPackage (Left p) = p
showPackage (Right nvr) = showNVR nvr

#if !MIN_VERSION_koji(0,0,3)
taskStateToValue :: TaskState -> Value
taskStateToValue = ValueInt . fromEnum
#endif

parseTaskState' :: String -> TaskState
parseTaskState' s =
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
    _ -> error' $! "unknown task state:" +-+ s ++
         "\nknown states: free, open, closed, canceled, assigned, failed"

data LogFile = BuildLog | RootLog | HWInfo
  deriving Eq

data OutputLocation = PackagesOutput | WorkOutput

outputUrl :: String -> TaskResult -> OutputLocation -> Maybe String
outputUrl hub task loc =
  case loc of
    WorkOutput -> Just $ worktaskDirUrlfromTaskID $ taskId task
    PackagesOutput ->
      case taskPackage task of
        Left _ -> Nothing
        Right nvr ->
          Just $ buildOutputURL hub nvr +/+ "data/logs" +/+ taskArch task

findOutputURL :: String -> TaskResult -> IO (Maybe String)
findOutputURL hub task =
  case outputUrl hub task PackagesOutput of
    Just burl -> urlMayExist (worktaskDirUrlfromTaskID $ taskId task) <||>
                 urlMayExist burl
    Nothing -> urlMayExist (worktaskDirUrlfromTaskID $ taskId task)
  where
    urlMayExist :: String -> IO (Maybe String)
    urlMayExist url = do
      exists <- httpExists' url
      return $
        if exists
        then Just url
        else Nothing

    mact1 <||> mact2 = do
      ma1 <- mact1
      if isJust ma1
        then return ma1
        else mact2

tailLogUrl :: String -> Int -> LogFile -> String
tailLogUrl hub taskid file =
  webUrl hub +/+ "getfile?taskID=" ++ show taskid ++ "&name=" ++ logFile file ++ "&offset=-6000"

logFile :: LogFile -> String
logFile RootLog = "root.log"
logFile BuildLog = "build.log"
logFile HWInfo = "hw_info.log"

buildlogSize :: Bool -> TimeZone -> Bool -> Bool -> Maybe String -> String
             -> TaskResult -> IO ()
buildlogSize _debug tz tail' hwinfo mgrep hub task = do
  murl <- findOutputURL hub task
  whenJust murl $ \ url -> do
    let buildlog = url +/+ logFile BuildLog
    exists <- httpExists' buildlog
    if exists
      then do
      putStr buildlog
      (msize,mtime) <- httpFileSizeTime' buildlog
      whenJust msize $ \size -> do
        fprint (" (" % commas % "kB)") (size `div` 1000)
        when (taskState task == TaskOpen) $
          whenJust mtime $ \time ->
          putStr $ " (" ++ compactZonedTime tz time ++ ")"
        putChar '\n'
        -- FIXME check if short build.log ends with srpm
        file <-
          if hwinfo
          then do
            putStr $ url +/+ logFile HWInfo
            return HWInfo
          else
            -- for buildroot failure build.log could be ~3082 bytes
            if size < 4000
            then do
              putStr $ url +/+ logFile RootLog
              return RootLog
            else return BuildLog
        when (tail' || hwinfo || isJust mgrep) $ displayLog url file
      else do
      let rootlog = url +/+ logFile RootLog
      whenM (httpExists' rootlog) $
        putStrLn rootlog
  where
    displayLog :: String -> LogFile -> IO ()
    displayLog url file = do
      let logurl =
            case file of
              BuildLog -> tailLogUrl hub (taskId task) file
              _ -> url +/+  logFile file
      req <- parseRequest logurl
      resp <- httpLBS req
      let out = U.toString $ getResponseBody resp
          ls = lines out
      putStrLn ""
      let output
            | file == RootLog =
              let excluded = ["Executing command:",
                              "Child return code was: 0",
                              "child environment: None",
                              "ensuring that dir exists:",
                              "touching file:",
                              "creating dir:",
                              "kill orphans"]
              in
                map (dropPrefix "DEBUG ") $ takeEnd 30 $
                filter (\l -> not (any (`isInfixOf` l) excluded)) ls
            | last ls == "Child return code was: 0" = ls
            | otherwise =
                case breakOnEnd ["Child return code was: 1"] ls of
                  ([],ls') -> ls'
                  (ls',_) -> ls'
      putStr $ unlines $
        case mgrep of
          Nothing -> output
          Just needle ->
            filter (match needle) ls
      putStrLn $ "\n" ++ logurl
      where
        match :: String -> String -> Bool
        match "" _ = error' "empty grep string not allowed"
        match _ "" = False
        match ('^':needle) ls =
          if last needle == '$'
          then needle == ls
          else needle `isPrefixOf` ls
        match needle ls =
          if last needle == '$'
          then needle `isSuffixOf` ls
          else needle `isInfixOf` ls

-- FIXME turn into a type?
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
