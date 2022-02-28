{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Builds (
  BuildReq(..),
  buildsCmd,
  parseBuildState,
  fedoraKojiHub,
  kojiBuildTypes
  )
where

import Control.Monad.Extra

import Data.Char (isDigit, toUpper)
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji
import Distribution.Koji.API
import SimpleCmd
import System.Directory (findExecutable)
import Text.Pretty.Simple

import qualified Tasks

data BuildReq = BuildBuild String | BuildPackage String
              | BuildQuery
  deriving Eq

getTimedate :: Tasks.BeforeAfter -> String
getTimedate (Tasks.Before s) = s
getTimedate (Tasks.After s) = s

capitalize :: String -> String
capitalize "" = ""
capitalize (h:t) = toUpper h : t

buildsCmd :: String -> Maybe String -> Int -> BuildReq -> [BuildState]
          -> Maybe Tasks.BeforeAfter -> Maybe String -> Bool -> Bool
          -> Maybe String -> IO ()
buildsCmd server muser limit buildreq states mdate mtype details debug mpat = do
  when (isJust mpat && buildreq /= BuildQuery) $
    error' "cannot use pattern with --build or --package"
  tz <- getCurrentTimeZone
  case buildreq of
    BuildBuild bld -> do
      when (isJust mdate) $
        error' "cannot use buildinfo together with timedate"
      let bldinfo = if all isDigit bld
                    then InfoID (read bld)
                    else InfoString bld
      mbld <- getBuild server bldinfo
      whenJust (mbld >>= maybeBuildResult) $ printBuild tz
    BuildPackage pkg -> do
      when (head pkg == '-') $
        error' $ "bad combination: not a package: " ++ pkg
      when (isJust mdate) $
        error' "cannot use --package together with timedate"
      mpkgid <- getPackageID server pkg
      case mpkgid of
        Nothing -> error' $ "no package id found for " ++ pkg
        Just pkgid -> do
          options <- setupQuery
          let fullquery = ("packageID", ValueInt pkgid):options
          when debug $ print fullquery
          blds <- listBuilds server fullquery
          when debug $ mapM_ pPrintCompact blds
          if details
            then mapM_ (printBuild tz) $ mapMaybe maybeBuildResult blds
            else
            mapM_ putStrLn $ mapMaybe shortBuildResult blds
    _ -> do
      query <- setupQuery
      let fullquery =
            query <>
            [("queryOpts", ValueStruct [("limit",ValueInt limit),
                                        ("order", ValueString "-build_id")])]
      when debug $ print fullquery
      blds <- listBuilds server fullquery
      when debug $ mapM_ pPrintCompact blds
      (mapM_ (printBuild tz) . mapMaybe maybeBuildResult) blds
  where
    shortBuildResult :: Struct -> Maybe String
    shortBuildResult bld = do
      nvr <- lookupStruct "nvr" bld
      state <- readBuildState <$> lookupStruct "state" bld
      date <- lookupStruct "completion_time" bld
      return $ nvr ++
        " (" ++ takeWhile (/= '.') date ++ ")" ++
        (if state == BuildComplete then "" else " " ++ show state)

    setupQuery = do
      case buildreq of
        -- FIXME dummy cases!
        BuildBuild _ -> error' "unreachable build request"
        BuildPackage _ ->
          return [("queryOpts",ValueStruct [("limit",ValueInt limit),
                                            ("order",ValueString "-build_id")])]
        BuildQuery -> do
          date <- cmd "date" ["+%F %T%z", "--date=" ++ dateString mdate]
          when (isJust mdate) $
            putStrLn $ maybe "" (++ " builds ") mtype ++ maybe "before" show mdate ++ " " ++ date
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
                [("userID", ValueInt (getID owner)),
                 ("complete" ++ maybe "Before" (capitalize . show) mdate, ValueString date)]
                ++ [("state", ValueArray (map buildStateToValue states)) | notNull states]
                ++ [("type", ValueString typ) | Just typ <- [mtype]]
                ++ [("pattern", ValueString pat) | Just pat <- [mpat]]

    dateString :: Maybe Tasks.BeforeAfter -> String
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

    maybeBuildResult :: Struct -> Maybe BuildResult
    maybeBuildResult st = do
      start_time <- readTime' <$> lookupStruct "start_time" st
      let mend_time = readTime' <$> lookupStruct "completion_time" st
      buildid <- lookupStruct "build_id" st
      taskid <- lookupStruct "task_id" st
      state <- getBuildState st
      nvr <- lookupStruct "nvr" st >>= maybeNVR
      return $
        BuildResult nvr state buildid taskid start_time mend_time
      where
        readTime' :: String -> UTCTime
        readTime' = read . replace "+00:00" "Z"

    printBuild :: TimeZone -> BuildResult -> IO ()
    printBuild tz task = do
      putStrLn ""
      let mendtime = mbuildEndTime task
      time <- maybe getCurrentTime return mendtime
      (mapM_ putStrLn . formatBuildResult (isJust mendtime) tz) (task {mbuildEndTime = Just time})

    pPrintCompact =
#if MIN_VERSION_pretty_simple(4,0,0)
      pPrintOpt CheckColorTty
      (defaultOutputOptionsDarkBg {outputOptionsCompact = True})
#else
      pPrint
#endif

-- FIXME server
formatBuildResult :: Bool -> TimeZone -> BuildResult -> [String]
formatBuildResult ended tz (BuildResult nvr state buildid taskid start mendtime) =
  [ showNVR nvr +-+ show state
  , "https://koji.fedoraproject.org/koji/buildinfo?buildID=" ++ show buildid
  , "https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show taskid
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

-- FIXME
data BuildResult =
  BuildResult {_buildNVR :: NVR,
               _buildState :: BuildState,
               _buildId :: Int,
               _taskId :: Int,
               _buildStartTime :: UTCTime,
               mbuildEndTime :: Maybe UTCTime
              }

#if !MIN_VERSION_koji(0,0,3)
buildStateToValue :: BuildState -> Value
buildStateToValue = ValueInt . fromEnum

parseBuildState :: String -> BuildState
parseBuildState s =
  case lower s of
    "building" -> BuildBuilding
    "complete" -> BuildComplete
    "deleted" -> BuildDeleted
    "fail" -> BuildFailed
    "failed" -> BuildFailed
    "cancel" -> BuildCanceled
    "canceled" -> BuildCanceled
    _ -> error' $! "unknown task state: " ++ s
#endif

data LastLog = WholeBuild | BuildTail | RootLog
  deriving Eq

getBuildState :: Struct -> Maybe BuildState
getBuildState st = readBuildState <$> lookup "state" st

kojiBuildTypes :: [String]
kojiBuildTypes = ["all", "image", "maven", "module", "rpm", "win"]
