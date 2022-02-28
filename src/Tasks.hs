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
import System.Directory (findExecutable)
import System.FilePath
import Text.Pretty.Simple

data TaskReq = Task Int | Parent Int | Build String | Package String
             | TaskQuery

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

tasksCmd :: String -> Maybe String -> Int -> TaskReq -> [TaskState]
         -> [String] -> Maybe BeforeAfter -> Maybe String -> Bool
         -> Maybe TaskFilter -> Bool -> IO ()
tasksCmd server muser limit taskreq states archs mdate mmethod debug mfilter' tail' = do
  tz <- getCurrentTimeZone
  mgr <- httpManager
  case taskreq of
    Task taskid -> do
      when (isJust muser || isJust mdate || isJust mfilter') $
        error' "cannot use --task together with --user, timedate, or filter"
      mtask <- kojiGetTaskInfo server (TaskId taskid)
      whenJust mtask$ \task -> do
        when debug $ pPrintCompact task
        whenJust (maybeTaskResult task) $ printTask mgr tz
    Build bld -> do
      when (isJust mdate || isJust mfilter') $
        error' "cannot use --build together with timedate or filter"
      mtaskid <- if all isDigit bld
                then ((fmap TaskId . lookupStruct "task_id") =<<) <$> getBuild server (InfoID (read bld))
                else kojiGetBuildTaskID server bld
      whenJust mtaskid $ \(TaskId taskid) ->
        tasksCmd server muser limit (Parent taskid) states archs mdate mmethod debug mfilter' tail'
    Package pkg -> do
      when (head pkg == '-') $
        error' $ "bad combination: not a package " ++ pkg
      when (isJust mdate || isJust mfilter') $
        error' "cannot use --package together with timedate or filter"
      mpkgid <- getPackageID server pkg
      case mpkgid of
        Nothing -> error' $ "no package id found for " ++ pkg
        Just pkgid -> do
          options <- setupQuery
          blds <- listBuilds server $
                  ("packageID", ValueInt pkgid):options
          case blds of
            [bld] -> do
              case lookupStruct "task_id" bld of
                Just taskid ->
                  tasksCmd server muser 10 (Parent taskid) states archs mdate mmethod debug mfilter' tail'
                Nothing -> error' "task id not found"
            _ -> do
              mapM_ putStrLn $ mapMaybe buildResult blds
              where
                buildResult :: Struct -> Maybe String
                buildResult bld = do
                  nvr <- lookupStruct "nvr" bld
                  state <- readBuildState <$> lookupStruct "state" bld
                  date <- lookupStruct "completion_time" bld
                  return $ nvr ++
                    " (" ++ takeWhile (/= '.') date ++ ")" ++
                    (if state == BuildComplete then "" else " " ++ show state)
    _ -> do
      query <- setupQuery
      results <- listTasks server query
                 [("limit",ValueInt limit), ("order", ValueString "-id")]
      when debug $ mapM_ pPrintCompact results
      (mapM_ (printTask mgr tz) . filterResults . mapMaybe maybeTaskResult) results
  where
    setupQuery = do
      case taskreq of
        -- FIXME!
        Task _ -> error' "unreachable task request"
        Build _ -> error' "unreachable build request"
        Package _ ->
          return [("queryOpts",ValueStruct [("limit",ValueInt limit),
                                            ("order",ValueString "-build_id")])]
        Parent parent -> do
          when (isJust muser || isJust mdate || isJust mfilter') $
            error' "cannot use --parent together with --user, timedate, or filter"
          return $
            ("parent", ValueInt parent) : commonParams
        TaskQuery -> do
          date <- cmd "date" ["+%F %T%z", "--date=" ++ dateString mdate]
          when (isJust mdate) $
            putStrLn $ maybe "" (++ " tasks ") mmethod ++ maybe "before" show mdate ++ " " ++ date
          user <-
            case muser of
              Just user -> return user
              Nothing -> do
                mKlist <- findExecutable "klist"
                if isJust mKlist
                  then do
                  mkls <- fmap words <$> cmdMaybe "klist" ["-l"]
                  case mkls of
                    Nothing -> error "klist failed"
                    Just kls ->
                      case find ("@FEDORAPROJECT.ORG" `isSuffixOf`) kls of
                        Nothing -> error' "Could not determine FAS id from klist"
                        Just principal ->
                          return $ dropSuffix "@FEDORAPROJECT.ORG" principal
                  else error' "Please specify koji user"
          mowner <- kojiGetUserID fedoraKojiHub user
          case mowner of
            Nothing -> error "No owner found"
            Just owner ->
              return $
                [("owner", ValueInt (getID owner)),
                 ("complete" ++ maybe "Before" (capitalize . show) mdate, ValueString date)]
                ++ commonParams
        where
          commonParams =
            [("decode", ValueBool True)]
            ++ [("state", ValueArray (map taskStateToValue states)) | notNull states]
            ++ [("arch", ValueArray (map (ValueString . kojiArch) archs)) | notNull archs]
            ++ [("method", ValueString method) | Just method <- [mmethod]]

          kojiArch :: String -> String
          kojiArch "i686" = "i386"
          kojiArch "armv7hl" = "armhfp"
          kojiArch a = a

    dateString :: Maybe BeforeAfter -> String
    dateString Nothing = "now"
    -- make time refer to past not future
    dateString (Just beforeAfter) =
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
      start_time <- readTime' <$> lookupStruct "start_time" st
      let mend_time = readTime' <$> lookupStruct "completion_time" st
      taskid <- lookupStruct "id" st
      method <- lookupStruct "method" st
      hostid <- lookupStruct "host_id" st
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
        TaskResult package arch method hostid state mparent' taskid start_time mend_time
      where
        readTime' :: String -> UTCTime
        readTime' = read . replace "+00:00" "Z"

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

    printTask :: Manager -> TimeZone -> TaskResult -> IO ()
    printTask mgr tz task = do
      putStrLn ""
      let mendtime = mtaskEndTime task
      time <- maybe getCurrentTime return mendtime
      (mapM_ putStrLn . formatTaskResult (isJust mendtime) tz) (task {mtaskEndTime = Just time})
      buildlogSize tail' mgr (taskId task)

    pPrintCompact =
#if MIN_VERSION_pretty_simple(4,0,0)
      pPrintOpt CheckColorTty
      (defaultOutputOptionsDarkBg {outputOptionsCompact = True})
#else
      pPrint
#endif

formatTaskResult :: Bool -> TimeZone -> TaskResult -> [String]
formatTaskResult ended tz (TaskResult pkg arch method _hostid state mparent taskid start mendtime) =
  [ showPackage pkg +-+ (if method == "buildArch" then arch else method) +-+ show state
  , "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show taskid +-+ maybe "" (\p -> "(parent: " ++ show p ++ ")") mparent
  , formatTime defaultTimeLocale "Start: %c" (utcToLocalTime tz start)
  ]
  ++
  case mendtime of
    Nothing -> []
    Just end ->
      [formatTime defaultTimeLocale "End:   %c" (utcToLocalTime tz end) | ended]
#if MIN_VERSION_time(1,9,1)
      ++
      let dur = diffUTCTime end start
      in [(if not ended then "current " else "") ++ "duration: " ++ formatTime defaultTimeLocale "%Hh %Mm %Ss" dur]
#endif
  where
    showPackage :: Either String NVR -> String
    showPackage (Left p) = p
    showPackage (Right nvr) = showNVR nvr

data TaskResult =
  TaskResult {taskPackage :: Either String NVR,
              _taskArch :: String,
              _taskMethod :: String,
              _taskHostId :: Int,
              _taskState :: TaskState,
              _mtaskParent :: Maybe Int,
              taskId :: Int,
              _taskStartTime :: UTCTime,
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

buildlogSize :: Bool -> Manager -> Int -> IO ()
buildlogSize tail' mgr taskid = do
  let buildlog = logUrl taskid WholeBuild
  exists <- httpExists mgr buildlog
  when exists $ do
    putStr $ buildlog ++ " "
    msize <- httpFileSize mgr buildlog
    whenJust msize $ \size -> do
      putStr "("
      (T.putStr . kiloBytes) size
      putStrLn ")"
      -- FIXME maybe timestamp better
      lastlog <-
        if size < 1500
        then do
          putStrLn $ logUrl taskid RootLog
          return RootLog
        else return BuildTail
      if tail'
        then displayLog taskid lastlog
        else putStrLn $ logUrl taskid lastlog
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
