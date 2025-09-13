{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Install (
  Select(..),
  selectDefault,
  Request(..),
  installCmd,
  ExistingStrategy,
  PkgMgr(..),
  knownHubs,
  Yes(..),
  installArgs
  )
where

import Control.Monad.Extra
import Data.Functor ((<&>))
import Data.List.Extra
import Data.Maybe
import Data.RPM.NV hiding (name)
import Data.RPM.NVR
import Data.RPM.NVRA
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import Network.HTTP.Directory (httpFileSize', httpLastModified', (+/+))
import SelectRPMs (Yes(..), PkgMgr(..), ExistingStrategy, Select(..),
                   ExistNVRA, decideRPMs, installArgs, installRPMsAllowErasing,
                   nvraToRPM, rpmsToNVRAs, selectDefault)
import SimpleCmd
import System.Directory
import System.FilePath
import System.IO
import Text.Read (readMaybe)

import Common
import DownloadDir
import Time
import Utils

data Request = ReqName | ReqNV | ReqNVR
  deriving Eq

-- FIXME autodetect NVR, NV, etc
-- FIXME support buildid
-- FIXME specify tag or task
-- FIXME support --latest (last build)
-- FIXME support enterprise builds
-- FIXME --debuginfo
-- FIXME --delete after installing
-- FIXME way to install selected packages using default dnf repo instead
-- FIXME offer to download subpackage deps
-- FIXME is --check-remote-time really needed?
installCmd :: Bool -> Bool -> Yes -> Maybe String -> Maybe String -> Bool
           -> Bool -> Bool -> Maybe PkgMgr -> Bool -> [String]
           -> Maybe ExistingStrategy -> Maybe String -> Select -> Maybe String
           -> Either () Request -> [String] -> IO ()
installCmd dryrun debug yes mhuburl mpkgsurl listmode latest checkremotetime mmgr allowerasing archs mstrategy mprefix select mdisttag erequest pkgbldtsktag = do
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (hubToPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  printDlDir <- setDownloadDir dryrun "koji-tool"
  when debug printDlDir
  setNoBuffering
  (request,pkgbldtsks) <-
    case erequest of
      -- tagged
      Left () ->
        case pkgbldtsktag of
          [tag] -> do
            nvrs <- map kbNvr <$> kojiListTaggedBuilds huburl True tag
            return (ReqNVR,nvrs)
          _ -> error' "only one tag can be specified"
      Right req ->
        return (req,pkgbldtsktag)
  buildrpms <- mapM (kojiRPMs huburl pkgsurl request) $
               nubOrd pkgbldtsks
  unless listmode printDlDir
  installRPMsAllowErasing dryrun debug mmgr allowerasing yes buildrpms
  where
    kojiRPMs :: String -> String -> Request -> String
             -> IO (FilePath,[ExistNVRA])
    kojiRPMs huburl pkgsurl request bldtask =
      case readMaybe bldtask of
        Just taskid -> kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode archs mstrategy mprefix select checkremotetime taskid
        Nothing -> kojiBuildRPMs huburl pkgsurl request bldtask

    kojiBuildRPMs :: String -> String -> Request -> String
                  -> IO (FilePath,[ExistNVRA])
    kojiBuildRPMs huburl pkgsurl request pkgbld = do
      disttag <-
        case mdisttag of
          Just dt -> return dt
          Nothing -> do
            dist <- cmd "rpm" ["--eval", "%{dist}"]
            return $ if dist == "%{dist}" then "" else dist
      nvrs <- map readNVR <$> kojiBuildOSBuilds debug huburl listmode latest disttag request pkgbld
      case nvrs of
        [] -> error' $ pkgbld ++ " not found for " ++ disttag
        [nvr] -> do
          putStrLn $ showNVR nvr ++ ":"
          bid <- kojiGetBuildID' huburl (showNVR nvr)
          -- FIXME should we try kojiTaskRPMs first?
          nvras <- rpmsToNVRAs <$> kojiGetBuildRPMs huburl nvr archs bid
          results <-
            if null nvras
              then do
              mtid <- kojiGetBuildTaskID huburl (showNVR nvr)
              case mtid of
                Just (TaskId tid) ->
                  kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode archs mstrategy mprefix select checkremotetime tid
                Nothing -> error' $ "task id not found for" +-+ showNVR nvr
              else do
              when debug $ mapM_ (putStrLn . showNVRA) nvras
              let prefix = fromMaybe (nvrName nvr) mprefix
              dlRpms <- decideRPMs yes listmode mstrategy select prefix nvras
              let subdir = showNVR nvr
              unless listmode $ do
                unless (dryrun || null dlRpms) $ do
                  bld <- kojiGetBuild' huburl nvr
                  -- FIXME should be NVRA ideally
                  downloadRpms debug checkremotetime (lookupStartEndTimes' bld) subdir (buildURL nvr) dlRpms
              return (subdir,dlRpms)
          return $
            if listmode
            then ("",[])
            else results
        _ ->
          if listmode
          then do
            mapM_ (putStrLn . showNVR) nvrs
            return ("",[])
          else error $ "multiple build founds for " ++ pkgbld ++ ": " ++
               unwords (map showNVR nvrs)
        where
          buildURL :: NVR -> String -> String
          buildURL (NVR n (VerRel v r)) rpm =
             let arch = rpmArch (readNVRA rpm)
             in pkgsurl +/+ n  +/+ v +/+ r +/+ arch +/+ rpm

kojiTaskRPMs :: Bool -> Bool -> Yes -> String -> String -> Bool -> [String]
             -> Maybe ExistingStrategy -> Maybe String -> Select -> Bool
             -> Int -> IO (FilePath,[ExistNVRA])
kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode archs mstrategy mprefix select checkremotetime taskid = do
  mtaskinfo <- Koji.getTaskInfo huburl taskid True
  tasks <- case mtaskinfo of
            Nothing -> error' "failed to get taskinfo"
            Just taskinfo ->
              case lookupStruct "method" taskinfo :: Maybe String of
                Nothing -> error' $ "no method found for " ++ show taskid
                Just method ->
                  case method of
                    "build" -> do
                      when debug $ mapM_ print taskinfo >> putStrLn ""
                      Koji.getTaskChildren huburl taskid True
                    "buildArch" -> do
                      when debug $ print taskinfo >> putStrLn ""
                      return [taskinfo]
                    _ -> error' $ "unsupport method: " ++ method
  totalarchs <- map kojiTaskArch <$> getArchs archs
  let (archtid,archtask) =
        case find (selectBuildArch totalarchs) tasks of
          Nothing -> error' $ "no task found for" +-+ unwords totalarchs
          Just task' ->
            case lookupStruct "id" task' of
              Nothing -> error' "task id not found"
              Just tid -> (tid,task')
  when debug $ mapM_ print archtask
  nvras <- getTaskNVRAs archtid
  when (null nvras) $
    error' $ "no rpms found for" +-+ show archtid
  prefix <- case mprefix of
              Just pref -> return pref
              Nothing ->
                case find ((== "src") . rpmArch) nvras of
                  Just src -> return $ rpmName src
                  Nothing ->
                    return $ either id nvrName $ kojiTaskRequestNVR archtask
  if listmode
    then do
    rpms <- decideRPMs yes listmode mstrategy select prefix nvras
    return ("",rpms)
    else do
      when debug $ print $ map showNVRA nvras
      let subdir = show archtid
      dlRpms <- decideRPMs yes listmode mstrategy select prefix $
                filter ((/= "src") . rpmArch) nvras
      unless (dryrun || null dlRpms) $
        downloadRpms debug checkremotetime (lookupStartEndTimes' archtask) subdir (taskRPMURL archtid) dlRpms
      return (subdir,dlRpms)
  where
    selectBuildArch :: [String] -> Struct -> Bool
    selectBuildArch archs' t =
      case lookupStruct "arch" t of
        Just arch -> arch `elem` "noarch" : archs' &&
                     lookupStruct "method" t == Just "buildArch"
        Nothing -> False

    getTaskNVRAs :: Int -> IO [NVRA]
    getTaskNVRAs taskid' =
      -- FIXME get stats to show size
      Koji.listTaskOutput huburl taskid' False True False <&>
      rpmsToNVRAs . filter (".rpm" `isExtensionOf`) . map fst

    taskRPMURL :: Int -> String -> String
    taskRPMURL taskid' rpm =
      let lastFew =
            let few = dropWhile (== '0') $ takeEnd 4 (show taskid') in
              if null few then "0" else few
      in dropSuffix "packages" pkgsurl +/+ "work/tasks/" ++ lastFew +/+ show taskid' +/+ rpm

getArchs :: [String] -> IO [String]
getArchs archs =
  case archs of
    [] -> ("noarch" :) <$> cmdLines "rpm" ["--eval", "%{_arch}"]
    ars -> return ars

kojiBuildOSBuilds :: Bool -> String -> Bool -> Bool -> String -> Request
                  -> String -> IO [String]
kojiBuildOSBuilds debug hub listmode latest disttag request pkgpat = do
  when debug $ putStrLn pkgpat
  let (pkg,full) = packageOfPattern request pkgpat
      -- FIXME remove after moving to newer koji-hs
      -- rpmfusion koji still doesn't support patterns (2024-09-20)
      oldkoji = "rpmfusion" `isInfixOf` hub
  when debug $ print (pkg,full)
  when debug $ putStrLn pkg
  when (latest && request == ReqNVR) $
    error' "cannot use --latest with --nvr"
  when (latest && not listmode) $
    putStrLn "--latest is implied when not using --list"
  when (oldkoji && ("*" `isInfixOf` pkgpat || request /= ReqName)) $
    error' "cannot use pattern with this kojihub"
  mpkgid <- Koji.getPackageID hub pkg
  case mpkgid of
    Nothing -> error' $ "package not found: " ++ pkg
    Just pkgid -> do
      -- strictly should getAPIVersion
      let opts = (if oldkoji
                  then id
                  else (("pattern", ValueString (if full then pkgpat else dropSuffix "*" pkgpat ++ "*" ++ disttag ++ "*")) :))
                 [("packageID", ValueInt pkgid),
                  commonBuildQueryOptions $
                  Just (if listmode && not latest || oldkoji then 20 else 1)]
      when debug $ print opts
      nvrs <- mapMaybe (lookupStruct "nvr") <$> Koji.listBuilds hub opts
      if null nvrs
        then error' $ "no builds found for " ++ disttag
        else
        return $
        if oldkoji
        then case filter (disttag `isInfixOf`) nvrs of
               [] -> error' $ "no builds found for " ++ disttag
               [res] -> [res]
               rs@(r:_) ->
                 if listmode then rs else [r]
        else nvrs

packageOfPattern :: Request -> String -> (String, Bool)
packageOfPattern request pat =
  case request of
    ReqName -> (dropSuffix "-" $ takeWhile (/= '*') pat, False)
    ReqNV ->
      case readNV pat of
        NV n _ -> (n, False)
    ReqNVR ->
      case readNVR pat of
        NVR n _ -> (n, True)

-- empty until build finishes
kojiGetBuildRPMs :: String -> NVR -> [String] -> BuildID -> IO [String]
kojiGetBuildRPMs huburl nvr archs (BuildId bid) = do
  rpms <- Koji.listBuildRPMs huburl bid
  totalarchs <- getArchs archs
  return $ map getNVRA $ filter (forArch totalarchs) rpms
  where
    forArch :: [String] -> Struct -> Bool
    forArch archs' st =
      case lookupStruct "arch" st of
        Just a -> a `elem` "noarch" : archs'
        Nothing -> error $ "No arch found for rpm for: " ++ showNVR nvr

    getNVRA :: Struct -> String
    getNVRA st =
      case lookupStruct "nvr" st of
        Nothing -> error' "NVR not found"
        Just pnvr ->
          case lookupStruct "arch" st of
            Nothing -> error "arch not found"
            Just arch ->
              pnvr <.> arch

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

downloadRpms :: Bool -> Bool -> (UTCTime, UTCTime) -> FilePath
             -> (String -> String) -> [ExistNVRA] -> IO ()
downloadRpms debug checkremotetime (taskstart,taskend) subdir urlOf rpms = do
  urls <- fmap catMaybes <$>
    forM rpms $ \(_,nvra) -> do
    let rpm = nvraToRPM nvra
        rpmfile = subdir </> rpm
    exists <- doesFileExist rpmfile
    let url = urlOf rpm
    notfile <-
      if exists
      then do
        if checkremotetime
          then do
          old <- outOfDate rpmfile url
          when old $ removeFile rpmfile
          return old
          else do
          localtime <- getModificationTime rpmfile
          return $ localtime < taskstart || localtime > taskend
      else return True
    -- FIXME is this still useful?
    when (notfile && debug) $ putStrLn url
    return $ if notfile then Just url else Nothing
  unless (null urls) $ do
    putStrLn "downloading..."
    cmd_ "curl" $ ["--remote-time", "--fail", "--fail-early", "-C-", "--show-error", "--create-dirs", "--output-dir", subdir, "--remote-name-all", "--write-out", "%{filename_effective}\n"] ++ ["--progress-bar" | not debug] ++ urls
  where
    outOfDate :: String -> String -> IO Bool
    outOfDate file url = do
      mremotetime <- httpLastModified' url
      case mremotetime of
        Just remotetime -> do
          localtime <- getModificationTime file
          if localtime < remotetime
            then return True
            else sizeOk file url
        Nothing -> sizeOk file url

    sizeOk :: String -> String -> IO Bool
    sizeOk file url = do
      remotesize <- httpFileSize' url
      localsize <- getFileSize file
      return $ remotesize /= Just localsize

-- showTask :: Struct -> Maybe String
-- showTask struct = do
--   state <- getTaskState struct
--   request <- lookupStruct "request" struct
--   method <- lookupStruct "method" struct
--   let mparent = lookupStruct "parent" struct :: Maybe Int
--       showreq = takeWhileEnd (/= '/') . unwords . mapMaybe getString . take 3
--   return $ showreq request +-+ method +-+ (if state == TaskClosed then "" else show state) +-+ maybe "" (\p -> "(" ++ show p ++ ")") mparent

-- showChildTask :: Struct -> Maybe String
-- showChildTask struct = do
--   arch <- lookupStruct "arch" struct
--   state <- getTaskState struct
--   method <- lookupStruct "method" struct
--   taskid <- lookupStruct "id" struct
--   return $ arch ++ replicate (8 - length arch) ' ' +-+ show (taskid :: Int) +-+ method +-+ show state

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
