{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Install (
  Select(..),
  Request(..),
  installCmd,
  ExistingStrategy(..),
  PkgMgr(..),
  knownHubs,
  Yes(..),
  installArgs
  )
where

import Control.Monad.Extra
import Data.List.Extra
import Data.Either (partitionEithers)
import Data.Maybe
import Data.RPM.NV hiding (name)
import Data.RPM.NVR
import Data.RPM.NVRA
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import Network.HTTP.Directory (httpFileSize', httpLastModified', (+/+))
import SimpleCmd
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.IO
import Text.Read (readMaybe)

import Common
import DownloadDir
import Time
import Utils

data Yes = No | Yes
  deriving Eq

data Select = All
            | Ask
            | PkgsReq [String] [String] [String] -- ^ include, add, exclude
  deriving Eq

installArgs :: String -> Select
installArgs cs =
  case words cs of
    ["-a"] -> All
    ["--all"] -> All
    ["-A"] -> Ask
    ["--ask"] -> Ask
    ws -> installPairs [] [] [] ws
  where
    installPairs :: [String] -> [String] -> [String] -> [String] -> Select
    installPairs inst add excl [] = PkgsReq inst add excl
    installPairs inst add excl (w:ws)
      | w `elem` ["-p","--package"] =
          case ws of
            [] -> error' "--install-opts --package missing value"
            (w':ws') -> installPairs (w':inst) add excl ws'
      | w == "--add" =
          case ws of
            [] -> error' "--install-opts --add missing value"
            (w':ws') -> installPairs inst (w':add) excl ws'
      | w `elem` ["-x","--exclude"] =
          case ws of
            [] -> error' "--install-opts --exclude missing value"
            (w':ws') -> installPairs inst add (w':excl) ws'
      | otherwise = error' "invalid --install-opts"

data Request = ReqName | ReqNV | ReqNVR
  deriving Eq

data PkgMgr = DNF | RPM | OSTREE
  deriving Eq

data ExistingStrategy = ExistingUpdate | ExistingNoReinstall | ExistingSkip

-- FIXME --include devel, --exclude *
-- FIXME specify tag or task
-- FIXME support enterprise builds
-- FIXME --arch (including src)
-- FIXME --debuginfo
-- FIXME --delete after installing
-- FIXME way to install selected packages using default dnf repo instead
-- FIXME offer to download subpackage deps
-- FIXME is --check-remote-time really needed?
installCmd :: Bool -> Bool -> Yes -> Maybe String -> Maybe String -> Bool
           -> Bool -> Bool -> Maybe PkgMgr -> ExistingStrategy -> Maybe String
           -> Select -> Maybe String -> Request -> [String] -> IO ()
installCmd dryrun debug yes mhuburl mpkgsurl listmode latest checkremotetime mmgr existingStrategy mprefix select mdisttag request pkgbldtsks = do
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (hubToPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  printDlDir <- setDownloadDir dryrun "rpms"
  when debug printDlDir
  setNoBuffering
  buildrpms <- mapM (kojiRPMs huburl pkgsurl printDlDir) pkgbldtsks
  installRPMs dryrun debug mmgr existingStrategy yes buildrpms
  where
    kojiRPMs :: String -> String -> IO () -> String
             -> IO (FilePath, [(Existence,NVRA)])
    kojiRPMs huburl pkgsurl printDlDir bldtask =
      case readMaybe bldtask of
        Just taskid -> kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode existingStrategy mprefix select checkremotetime printDlDir taskid
        Nothing -> kojiBuildRPMs huburl pkgsurl printDlDir bldtask

    kojiBuildRPMs :: String -> String -> IO () -> String
                  -> IO (FilePath, [(Existence,NVRA)])
    kojiBuildRPMs huburl pkgsurl printDlDir pkgbld = do
      disttag <-
        case mdisttag of
          Just dt -> return dt
          Nothing -> do
            dist <- cmd "rpm" ["--eval", "%{dist}"]
            return $ if dist == "%{dist}" then "" else dist
      nvrs <- map readNVR <$> kojiBuildOSBuilds debug huburl listmode latest disttag request pkgbld
      if listmode
        then do
        case nvrs of
          [nvr] -> do
            putStrLn (showNVR nvr)
            putStrLn ""
            bid <- kojiGetBuildID' huburl (showNVR nvr)
            nvras <- sort . map readNVRA . filter notDebugPkg <$> kojiGetBuildRPMs huburl nvr bid
            when debug $ mapM_ (putStrLn . showNVRA) nvras
            let prefix = fromMaybe (nvrName nvr) mprefix
            rpms <- decideRpms yes listmode existingStrategy select prefix nvras
            mapM_ printInstalled rpms
            -- kojiGetBuildRPMs huburl nvr bid >>=
            --   mapM_ putStrLn . sort . filter notDebugPkg
          _ -> mapM_ (putStrLn . showNVR) nvrs
        return ("",[])
        else
        case nvrs of
          [] -> error' $ pkgbld ++ " not found for " ++ disttag
          [nvr] -> do
            putStrLn $ showNVR nvr ++ "\n"
            bid <- kojiGetBuildID' huburl (showNVR nvr)
            nvras <- sort . map readNVRA . filter notDebugPkg <$> kojiGetBuildRPMs huburl nvr bid
            when debug $ mapM_ (putStrLn . showNVRA) nvras
            let prefix = fromMaybe (nvrName nvr) mprefix
            dlRpms <- decideRpms yes listmode existingStrategy select prefix nvras
            when debug $ mapM_ printInstalled dlRpms
            let subdir = showNVR nvr
            unless (dryrun || null dlRpms) $ do
              bld <- kojiGetBuild' huburl nvr
              -- FIXME should be NVRA ideally
              downloadRpms debug checkremotetime (lookupTimes' bld) subdir (buildURL nvr) dlRpms
              -- FIXME once we check file size - can skip if no downloads
              printDlDir
            return (subdir,dlRpms)
          _ -> error $ "multiple build founds for " ++ pkgbld ++ ": " ++
               unwords (map showNVR nvrs)
        where
          buildURL :: NVR -> String -> String
          buildURL (NVR n (VerRel v r)) rpm =
             let arch = rpmArch (readNVRA rpm)
             in pkgsurl +/+ n  +/+ v +/+ r +/+ arch +/+ rpm

notDebugPkg :: String -> Bool
notDebugPkg p =
  not ("-debuginfo-" `isInfixOf` p || "-debugsource-" `isInfixOf` p)

kojiTaskRPMs :: Bool -> Bool -> Yes -> String -> String -> Bool
             -> ExistingStrategy -> Maybe String -> Select -> Bool -> IO ()
             -> Int -> IO (FilePath, [(Existence,NVRA)])
kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode existingStrategy mprefix select checkremotetime printDlDir taskid = do
  mtaskinfo <- Koji.getTaskInfo huburl taskid True
  tasks <- case mtaskinfo of
            Nothing -> error' "failed to get taskinfo"
            Just taskinfo -> do
              when debug $ mapM_ print taskinfo
              case lookupStruct "method" taskinfo :: Maybe String of
                Nothing -> error' $ "no method found for " ++ show taskid
                Just method ->
                  case method of
                    "build" -> Koji.getTaskChildren huburl taskid True
                    "buildArch" -> return [taskinfo]
                    _ -> error' $ "unsupport method: " ++ method
  sysarch <- cmd "rpm" ["--eval", "%{_arch}"]
  let (archtid,archtask) =
        case find (selectBuildArch sysarch) tasks of
          Nothing -> error' $ "no " ++ sysarch ++ " task found"
          Just task' ->
            case lookupStruct "id" task' of
              Nothing -> error' "task id not found"
              Just tid -> (tid,task')
  when debug $ mapM_ print archtask
  nvras <- getTaskNVRAs archtid
  prefix <- case mprefix of
              Just pref -> return pref
              Nothing ->
                case find ((== "src") . rpmArch) nvras of
                  Just src -> return $ rpmName src
                  Nothing ->
                    return $ kojiTaskRequestPkg archtask
  if listmode
    then do
    drpms <- decideRpms yes listmode existingStrategy select prefix nvras
    return ("",drpms)
    else
    if null nvras
    then do
      (_, rpms) <- kojiTaskRPMs dryrun debug yes huburl pkgsurl True existingStrategy mprefix select checkremotetime printDlDir archtid
      mapM_ printInstalled rpms
      return ("",[])
    else do
      when debug $ print $ map showNVRA nvras
      dlRpms <- decideRpms yes listmode existingStrategy select prefix $
                filter ((/= "src") . rpmArch) nvras
      when debug $ mapM_ printInstalled dlRpms
      let subdir = show archtid
      unless (dryrun || null dlRpms) $ do
        downloadRpms debug checkremotetime (lookupTimes' archtask) subdir (taskRPMURL archtid) dlRpms
        printDlDir
      return (subdir,dlRpms)
  where
    selectBuildArch :: String -> Struct -> Bool
    selectBuildArch sysarch t =
      let march = lookupStruct "arch" t
          mmethod = lookupStruct "method" t
      in march `elem` [Just sysarch,Just "noarch"] &&
         mmethod == Just "buildArch"

    getTaskNVRAs :: Int -> IO [NVRA]
    getTaskNVRAs taskid' =
      sort . map readNVRA . filter notDebugPkg . filter (".rpm" `isExtensionOf`) . map fst <$>
      -- FIXME get stats to show size
      Koji.listTaskOutput huburl taskid' False True False

    taskRPMURL :: Int -> String -> String
    taskRPMURL taskid' rpm =
      let lastFew =
            let few = dropWhile (== '0') $ takeEnd 4 (show taskid') in
              if null few then "0" else few
      in dropSuffix "packages" pkgsurl +/+ "work/tasks/" ++ lastFew +/+ show taskid' +/+ rpm

data Existence = ExistingNVR | ChangedNVR | NotInstalled
  deriving (Eq, Ord, Show)

-- FIXME ExistingStrategy isn't used, so output doesn't reflect it
-- FIXME determine and add missing internal deps
decideRpms :: Yes -> Bool -> ExistingStrategy -> Select -> String -> [NVRA]
           -> IO [(Existence,NVRA)]
decideRpms yes listmode existingStrategy select prefix nvras = do
  classified <- mapM installExists (filter isBinaryRpm nvras)
  if listmode
    then do
    case select of
      PkgsReq subpkgs addpkgs exclpkgs -> do
        let install = selectRPMs False prefix (subpkgs,addpkgs,exclpkgs) classified
        mapM_ printInstalled install
      _ -> mapM_ printInstalled classified
    return []
    else
    case select of
      All -> do
        promptPkgs yes classified
      Ask -> mapMaybeM (rpmPrompt yes) classified
      PkgsReq [] addpkgs [] ->
        let add = selectRPMs False prefix ([],addpkgs,[]) classified
        in
          if all ((== NotInstalled) . fst) classified && yes /= Yes
          then (add ++) <$> decideRpms yes listmode existingStrategy Ask prefix nvras
          else do
            let install = add ++ filter ((/= NotInstalled) . fst) classified
            if yes == Yes
              then return install
              else promptPkgs yes install
      PkgsReq subpkgs addpkgs exclpkgs -> do
        let install = selectRPMs False prefix (subpkgs,addpkgs,exclpkgs) classified
        promptPkgs yes install
  where
    installExists :: NVRA -> IO (Existence, NVRA)
    installExists nvra = do
      minstalled <- cmdMaybe "rpm" ["-q", rpmName nvra]
      return
        (case minstalled of
           Nothing -> NotInstalled
           Just installed ->
             if installed == showNVRA nvra then ExistingNVR else ChangedNVR,
         nvra)

renderInstalled :: (Existence, NVRA) -> String
renderInstalled (exist, nvra) = showNVRA nvra ++ " (" ++ show exist ++ ")"

printInstalled :: (Existence, NVRA) -> IO ()
printInstalled = putStrLn . renderInstalled

selectRPMs :: Bool -> String -> ([String],[String],[String])
           -> [(Existence,NVRA)] -> [(Existence,NVRA)]
selectRPMs recurse prefix (subpkgs,[],[]) rpms =
  sort . mconcat $
  flip map subpkgs $ \ pkgpat ->
  case filter (match (compile pkgpat) . rpmName . snd) rpms of
    [] -> if head pkgpat /= '*' && not recurse
          then selectRPMs True prefix ([prefix ++ '-' : pkgpat],[],[]) rpms
          else error' $ "no subpackage match for " ++ pkgpat
    result -> result
selectRPMs _ prefix ([], [], subpkgs) rpms =
  -- FIXME somehow determine unused excludes
  foldl' (exclude subpkgs) [] rpms
  where
    rpmnames = map (rpmName . snd) rpms

    exclude :: [String] -> [(Existence,NVRA)] -> (Existence,NVRA)
            -> [(Existence,NVRA)]
    exclude [] acc rpm = acc ++ [rpm]
    exclude (pat:pats) acc rpm =
        if checkMatch (rpmName (snd rpm))
        then acc
        else exclude pats acc rpm
      where
        checkMatch :: String -> Bool
        checkMatch rpmname =
          let comppat = compile pat
          in if isLiteral comppat
             then pat == rpmname ||
                  pat `notElem` rpmnames &&
                  (prefix ++ '-' : pat) == rpmname
             else match comppat rpmname
selectRPMs recurse prefix (subpkgs,addpkgs,exclpkgs) rpms =
  let needed = selectRPMs recurse prefix (subpkgs,[],[]) rpms
      added = selectRPMs recurse prefix (addpkgs,[],[]) rpms
      excluded = selectRPMs recurse prefix (exclpkgs,[],[]) rpms
  in nub . sort $ added ++ (needed \\ excluded)

promptPkgs :: Yes -> [(Existence,NVRA)] -> IO [(Existence,NVRA)]
promptPkgs yes classified = do
  mapM_ printInstalled classified
  ok <- prompt yes "install above"
  return $ if ok then classified else []

prompt :: Yes -> String -> IO Bool
prompt yes str = do
  if yes == Yes
    then return True
    else do
    putStr $ str ++ " [Y/n]: "
    inp <- trim <$> getLine
    case lower inp of
      "" -> return True
      "y" -> return True
      "yes" -> return True
      "n" -> return False
      "no" -> return False
      _ -> prompt yes str

rpmPrompt :: Yes -> (Existence,NVRA) -> IO (Maybe (Existence,NVRA))
rpmPrompt yes (exist,nvra) = do
  ok <- prompt yes $ renderInstalled (exist,nvra)
  return $
    if ok
    then Just (exist,nvra)
    else Nothing

kojiBuildOSBuilds :: Bool -> String -> Bool -> Bool -> String -> Request
                  -> String -> IO [String]
kojiBuildOSBuilds debug hub listmode latest disttag request pkgpat = do
  when debug $ putStrLn pkgpat
  let (pkg,full) = packageOfPattern request pkgpat
      -- FIXME recheck koji version
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
                  ("state", ValueInt (fromEnum BuildComplete)),
                  commonBuildQueryOptions
                  (if listmode && not latest || oldkoji then 20 else 1)]
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

kojiGetBuildRPMs :: String -> NVR -> BuildID -> IO [String]
kojiGetBuildRPMs huburl nvr (BuildId bid) = do
  rpms <- Koji.listBuildRPMs huburl bid
  sysarch <- cmd "rpm" ["--eval", "%{_arch}"]
  return $ map getNVRA $ filter (forArch sysarch) rpms
  where
    forArch :: String -> Struct -> Bool
    forArch sysarch st =
      case lookupStruct "arch" st of
        Just arch -> arch `elem` [sysarch, "noarch"]
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

data InstallType = ReInstall | Install

-- FIXME ExistingStrategy should move to decideRpms
installRPMs :: Bool -> Bool -> Maybe PkgMgr -> ExistingStrategy -> Yes
            -> [(FilePath,[(Existence,NVRA)])] -> IO ()
installRPMs _ _ _ _ _ [] = return ()
installRPMs dryrun debug mmgr existingStrategy yes classified = do
  case installTypes classified of
    ([],is) -> doInstall Install is
    (ris,is) -> do
      doInstall ReInstall (ris ++ is)
      doInstall Install is
  where
    doInstall i dirpkgs =
      unless (null dirpkgs) $ do
      mgr <-
        case mmgr of
          Nothing -> do
            mostree <- findExecutable "rpm-ostree"
            return $ if isJust mostree then OSTREE else DNF
          Just m -> return m
      let pkgmgr =
            case mgr of
              DNF -> "dnf"
              RPM -> "rpm"
              OSTREE -> "rpm-ostree"
          mcom =
            case i of
              ReInstall ->
                case existingStrategy of
                  ExistingUpdate -> Just (reinstallCommand mgr)
                  _ -> Nothing
              Install ->
                case existingStrategy of
                  ExistingSkip -> Nothing
                  _ -> Just (installCommand mgr)
        in whenJust mcom $ \com ->
        if dryrun
        then mapM_ putStrLn $ ("would" +-+ unwords (pkgmgr : com) ++ ":") : map showRpmFile dirpkgs
        else do
          when debug $ mapM_ (putStrLn . showRpmFile) dirpkgs
          (case mgr of
            OSTREE -> cmd_
            _ -> sudo_) pkgmgr $
            com ++ map showRpmFile dirpkgs ++ ["--assumeyes" | yes == Yes && mgr == DNF]

    installTypes :: [(FilePath,[(Existence,NVRA)])]
                 -> ([(FilePath,NVRA)],[(FilePath,NVRA)])
    installTypes = partitionEithers  . concatMap mapDir
      where
        mapDir :: (FilePath,[(Existence,NVRA)])
               -> [Either (FilePath,NVRA) (FilePath,NVRA)]
        mapDir (dir,cls) =
          map (\(e,n) -> combineExist e (dir,n)) cls

        combineExist e = if e == ExistingNVR then Left else Right

    reinstallCommand :: PkgMgr -> [String]
    reinstallCommand mgr =
      case mgr of
        DNF -> ["reinstall"]
        RPM -> ["-Uvh","--replacepkgs"]
        OSTREE -> ["install"]

    installCommand :: PkgMgr -> [String]
    installCommand mgr =
      case mgr of
        DNF -> ["localinstall"]
        RPM -> ["-ivh"]
        OSTREE -> ["install"]

showRpm :: NVRA -> FilePath
showRpm nvra = showNVRA nvra <.> "rpm"

showRpmFile :: (FilePath,NVRA) -> FilePath
showRpmFile (dir,nvra) = dir </> showRpm nvra

downloadRpms :: Bool -> Bool -> (UTCTime, UTCTime) -> FilePath
             -> (String -> String) -> [(Existence,NVRA)] -> IO ()
downloadRpms debug checkremotetime (taskstart,taskend) subdir urlOf rpms = do
  urls <- fmap catMaybes <$>
    forM (map (showRpm . snd) rpms) $ \rpm -> do
    let rpmfile = subdir </> rpm
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
    cmd_ "curl" $ ["--remote-time", "--fail", "-C-", "--show-error", "--create-dirs", "--output-dir", subdir, "--remote-name-all", "--progress-bar", "--write-out", "%{filename_effective}\n"] ++ urls
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

isBinaryRpm :: NVRA -> Bool
isBinaryRpm = (/= "src") . rpmArch

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
