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
import Data.Functor ((<&>))
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
import SimplePrompt (yesNoDefault)
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
            | PkgsReq [String] [String] [String] [String] -- include, except, exclude, add
  deriving Eq

installArgs :: String -> Select
installArgs cs =
  case words cs of
    ["-a"] -> All
    ["--all"] -> All
    ["-A"] -> Ask
    ["--ask"] -> Ask
    ws -> installPairs [] [] [] [] ws
  where
    installPairs :: [String] -> [String] -> [String] -> [String]
                 -> [String] -> Select
    installPairs incl except excl add [] = PkgsReq incl except excl add
    installPairs incl except excl add (w:ws)
      | w `elem` ["-p","--package"] =
          case ws of
            [] -> error' "--install opts: --package missing value"
            (w':ws') -> checkPat w' $
                        installPairs (w':incl) except excl add ws'
      | w `elem` ["-e","--except"] =
          case ws of
            [] -> error' "--install opts: --except missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl (w':except) excl add ws'
      | w `elem` ["-x","--exclude"] =
          case ws of
            [] -> error' "--install opts: --exclude missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl except (w':excl) add ws'
      | w `elem` ["-i","--include"] =
          case ws of
            [] -> error' "--install opts: --include missing value"
            (w':ws') -> checkPat w' $
                        installPairs incl except excl (w':add) ws'
      | otherwise = error' "invalid --install opts"

    checkPat w' f =
      if null w'
      then error' "empty pattern!"
      else f

data Request = ReqName | ReqNV | ReqNVR
  deriving Eq

data PkgMgr = DNF3 | DNF5 | RPM | OSTREE
  deriving Eq

data ExistingStrategy = ExistingNoReinstall | ExistingSkip

-- FIXME autodetect NVR, NV, etc
-- FIXME support buildid
-- FIXME specify tag or task
-- FIXME support --latest
-- FIXME support enterprise builds
-- FIXME --debuginfo
-- FIXME --delete after installing
-- FIXME way to install selected packages using default dnf repo instead
-- FIXME offer to download subpackage deps
-- FIXME is --check-remote-time really needed?
installCmd :: Bool -> Bool -> Yes -> Maybe String -> Maybe String -> Bool
           -> Bool -> Bool -> Maybe PkgMgr -> [String]
           -> Maybe ExistingStrategy -> Maybe String -> Select -> Maybe String
           -> Request -> [String] -> IO ()
installCmd dryrun debug yes mhuburl mpkgsurl listmode latest checkremotetime mmgr archs mstrategy mprefix select mdisttag request pkgbldtsks = do
  checkSelection select
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (hubToPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  printDlDir <- setDownloadDir dryrun "koji-tool"
  when debug printDlDir
  setNoBuffering
  buildrpms <- mapM (kojiRPMs huburl pkgsurl printDlDir) $ nubOrd pkgbldtsks
  installRPMs dryrun debug mmgr yes buildrpms
  where
    checkSelection :: Monad m => Select -> m ()
    checkSelection (PkgsReq ps es xs is) =
      forM_ (ps ++ es ++ xs ++ is) $ \s ->
      when (null s) $ error' "empty package pattern not allowed"
    checkSelection _ = return ()

    kojiRPMs :: String -> String -> IO () -> String
             -> IO (FilePath, [(Existence,NVRA)])
    kojiRPMs huburl pkgsurl printDlDir bldtask =
      case readMaybe bldtask of
        Just taskid -> kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode archs mstrategy mprefix select checkremotetime printDlDir taskid
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
      case nvrs of
        [] -> error' $ pkgbld ++ " not found for " ++ disttag
        [nvr] -> do
          putStrLn $ showNVR nvr ++ "\n"
          bid <- kojiGetBuildID' huburl (showNVR nvr)
          -- FIXME should we try kojiTaskRPMs first?
          nvras <- sort . map readNVRA . filter notDebugPkg <$> kojiGetBuildRPMs huburl nvr archs bid
          results <-
            if null nvras
              then do
              mtid <- kojiGetBuildTaskID huburl (showNVR nvr)
              case mtid of
                Just (TaskId tid) ->
                  kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode archs mstrategy mprefix select checkremotetime printDlDir tid
                Nothing -> error' $ "task id not found for" +-+ showNVR nvr
              else do
              when debug $ mapM_ (putStrLn . showNVRA) nvras
              let prefix = fromMaybe (nvrName nvr) mprefix
              dlRpms <- decideRpms yes listmode mstrategy select prefix nvras
              when debug $ mapM_ printInstalled dlRpms
              let subdir = showNVR nvr
              unless listmode $ do
                unless (dryrun || null dlRpms) $ do
                  bld <- kojiGetBuild' huburl nvr
                  -- FIXME should be NVRA ideally
                  downloadRpms debug checkremotetime (strictLookupTimes lookupBuildTimes bld) subdir (buildURL nvr) dlRpms
                -- FIXME once we check file size - can skip if no downloads
                  printDlDir
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

notDebugPkg :: String -> Bool
notDebugPkg p =
  not ("-debuginfo-" `isInfixOf` p || "-debugsource-" `isInfixOf` p)

kojiTaskRPMs :: Bool -> Bool -> Yes -> String -> String -> Bool -> [String]
             -> Maybe ExistingStrategy -> Maybe String -> Select -> Bool
             -> IO () -> Int -> IO (FilePath, [(Existence,NVRA)])
kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode archs mstrategy mprefix select checkremotetime printDlDir taskid = do
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
                    "buildArch" -> return [taskinfo]
                    _ -> error' $ "unsupport method: " ++ method
  totalarchs <- getArchs archs
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
    drpms <- decideRpms yes listmode mstrategy select prefix nvras
    return ("",drpms)
    else do
      when debug $ print $ map showNVRA nvras
      dlRpms <- decideRpms yes listmode mstrategy select prefix $
                filter ((/= "src") . rpmArch) nvras
      when debug $ mapM_ printInstalled dlRpms
      let subdir = show archtid
      unless (dryrun || null dlRpms) $ do
        downloadRpms debug checkremotetime (strictLookupTimes lookupTaskTimes archtask) subdir (taskRPMURL archtid) dlRpms
        printDlDir
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
      sort . map readNVRA . filter notDebugPkg . filter (".rpm" `isExtensionOf`) . map fst

    taskRPMURL :: Int -> String -> String
    taskRPMURL taskid' rpm =
      let lastFew =
            let few = dropWhile (== '0') $ takeEnd 4 (show taskid') in
              if null few then "0" else few
      in dropSuffix "packages" pkgsurl +/+ "work/tasks/" ++ lastFew +/+ show taskid' +/+ rpm

getArchs :: [String] -> IO [String]
getArchs archs =
  case archs of
    [] -> cmdLines "rpm" ["--eval", "%{_arch}"]
    ars -> return ars

data Existence = ExistingNVR | ChangedNVR | NotInstalled
  deriving (Eq, Ord, Show)

-- FIXME determine and add missing internal deps
decideRpms :: Yes -> Bool -> Maybe ExistingStrategy -> Select -> String
           -> [NVRA] -> IO [(Existence,NVRA)]
decideRpms yes listmode mstrategy select prefix nvras = do
  classified <- mapMaybeM installExists (filter isBinaryRpm nvras)
  if listmode
    then do
    case select of
      PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
        mapM_ printInstalled $
        selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
      _ -> mapM_ printInstalled classified
    return []
    else
    case select of
      All -> promptPkgs yes classified
      Ask -> mapMaybeM (rpmPrompt yes) classified
      PkgsReq subpkgs exceptpkgs exclpkgs addpkgs ->
        promptPkgs yes $
        selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) classified
  where
    installExists :: NVRA -> IO (Maybe (Existence, NVRA))
    installExists nvra = do
      -- FIXME this will fail for noarch changes
      -- FIXME check kernel
      minstalled <- cmdMaybe "rpm" ["-q", rpmName nvra <.> rpmArch nvra]
      let existence =
            case minstalled of
              Nothing -> NotInstalled
              Just installed ->
                if showNVRA nvra `elem` lines installed
                then ExistingNVR
                else ChangedNVR
      return $
        case mstrategy of
          Just ExistingSkip | existence /= NotInstalled -> Nothing
          Just ExistingNoReinstall | existence == ExistingNVR -> Nothing
          _ -> Just (existence, nvra)

renderInstalled :: (Existence, NVRA) -> String
renderInstalled (exist, nvra) =
  case exist of
    ExistingNVR -> '='
    ChangedNVR -> '^'
    NotInstalled -> '+'
  : showNVRA nvra

printInstalled :: (Existence, NVRA) -> IO ()
printInstalled = putStrLn . renderInstalled

defaultRPMs :: [(Existence,NVRA)] -> [(Existence,NVRA)]
defaultRPMs rpms =
  let installed = filter ((/= NotInstalled) . fst) rpms
  in if null installed
     then rpms
     else installed

matchingRPMs :: String -> [String] -> [(Existence,NVRA)] -> [(Existence,NVRA)]
matchingRPMs prefix subpkgs rpms =
  nubSort . mconcat $
  flip map (nubOrd subpkgs) $ \ pkgpat ->
  case getMatches pkgpat of
    [] -> if head pkgpat /= '*'
          then
            case getMatches (prefix ++ '-' : pkgpat) of
              [] -> error' $ "no subpackage match for " ++ pkgpat
              result -> result
          else error' $ "no subpackage match for " ++ pkgpat
    result -> result
  where
    getMatches :: String -> [(Existence,NVRA)]
    getMatches pkgpat =
      filter (match (compile pkgpat) . rpmName . snd) rpms

nonMatchingRPMs :: String -> [String] -> [(Existence,NVRA)] -> [(Existence,NVRA)]
nonMatchingRPMs _ [] _ = []
nonMatchingRPMs prefix subpkgs rpms =
  -- FIXME somehow determine unused excludes
  nubSort $ foldl' (exclude (nubOrd subpkgs)) [] rpms
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

selectRPMs :: String
           -> ([String],[String],[String],[String]) -- (subpkgs,except,exclpkgs,addpkgs)
           -> [(Existence,NVRA)] -> [(Existence,NVRA)]
selectRPMs prefix (subpkgs,exceptpkgs,exclpkgs,addpkgs) rpms =
  let excluded = matchingRPMs prefix exclpkgs rpms
      included = matchingRPMs prefix addpkgs rpms
      matching =
        if null subpkgs && null exceptpkgs
        then defaultRPMs rpms
        else matchingRPMs prefix subpkgs rpms
      nonmatching = nonMatchingRPMs prefix exceptpkgs rpms
  in nubSort $ ((matching ++ nonmatching) \\ excluded) ++ included

promptPkgs :: Yes -> [(Existence,NVRA)] -> IO [(Existence,NVRA)]
promptPkgs _ [] = error' "no rpms found"
promptPkgs yes classified = do
  mapM_ printInstalled classified
  ok <- prompt yes "install above"
  return $ if ok then classified else []

prompt :: Yes -> String -> IO Bool
prompt yes str = do
  if yes == Yes
    then return True
    else yesNoDefault True str

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

data InstallType = ReInstall | Install

-- FIXME support options per build: install ibus imsettings -i plasma
-- (or don't error if multiple packages)
installRPMs :: Bool -> Bool -> Maybe PkgMgr -> Yes
            -> [(FilePath,[(Existence,NVRA)])] -> IO ()
installRPMs _ _ _ _ [] = return ()
installRPMs dryrun debug mmgr yes classified = do
  case installTypes classified of
    ([],is) -> doInstall Install is
    (ris,is) -> do
      doInstall ReInstall (ris ++ is) -- include any new deps
      doInstall Install is            -- install any non-deps
  where
    doInstall :: InstallType -> [(FilePath,NVRA)] -> IO ()
    doInstall inst dirpkgs =
      unless (null dirpkgs) $ do
      mgr <-
        case mmgr of
          Just m -> return m
          Nothing -> do
            ostree <- doesDirectoryExist "/sysroot/ostree"
            if ostree
              then return OSTREE
              else do
              mdnf5 <- findExecutable "dnf5"
              return $ maybe DNF3 (const DNF5) mdnf5
      let pkgmgr =
            case mgr of
              DNF3 -> "dnf-3"
              DNF5 -> "dnf5"
              RPM -> "rpm"
              OSTREE -> "rpm-ostree"
          com =
            case inst of
              ReInstall -> reinstallCommand mgr
              Install -> installCommand mgr
        in
        if dryrun
        then mapM_ putStrLn $ ("would" +-+ unwords (pkgmgr : com) ++ ":") : map showRpmFile dirpkgs
        else do
          when debug $ mapM_ (putStrLn . showRpmFile) dirpkgs
          (case mgr of
            OSTREE -> cmd_
            _ -> sudo_) pkgmgr $
            com ++ map showRpmFile dirpkgs ++ ["--assumeyes" | yes == Yes && mgr `elem` [DNF3,DNF5]]

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
        DNF3 -> ["reinstall"]
        DNF5 -> ["reinstall"]
        RPM -> ["-Uvh","--replacepkgs"]
        OSTREE -> ["install"]

    installCommand :: PkgMgr -> [String]
    installCommand mgr =
      case mgr of
        DNF3 -> ["localinstall"]
        DNF5 -> ["install"]
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
    cmd_ "curl" $ ["--remote-time", "--fail", "-C-", "--show-error", "--create-dirs", "--output-dir", subdir, "--remote-name-all", "--write-out", "%{filename_effective}\n"] ++ ["--progress-bar" | not debug] ++ urls
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
