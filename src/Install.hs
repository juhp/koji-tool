{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Install (
  Mode(..),
  Request(..),
  installCmd,
  knownHubs,
  Yes(..)
  )
where

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.RPM
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import Network.HTTP.Directory (httpFileSize', httpLastModified', (+/+))
import SimpleCmd
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.IO

import Common
import DownloadDir

defaultPkgsURL :: String -> String
defaultPkgsURL url =
  case dropSuffix "/" url of
    "https://koji.fedoraproject.org/kojihub" ->
      "https://kojipkgs.fedoraproject.org/packages"
    "https://kojihub.stream.centos.org/kojihub" ->
      "https://kojihub.stream.centos.org/kojifiles/packages"
    _ ->
      if "kojihub" `isSuffixOf` url
      then replace "kojihub" "kojifiles" url +/+ "packages"
      else error' $ "use --files-url to specify kojifiles url for " ++ url

data Yes = No | Yes
  deriving Eq

data Mode = All
          | Ask
          -- distinguish except and exclude
          | PkgsReq [String] [String] -- ^ include, except/exclude
  deriving Eq

data Request = ReqName | ReqNV | ReqNVR
  deriving Eq

-- FIXME --include devel, --exclude *
-- FIXME specify tag or task
-- FIXME support enterprise builds
-- FIXME --arch (including src)
-- FIXME --debuginfo
-- FIXME --delete after installing
installCmd :: Bool -> Bool -> Yes -> Maybe String -> Maybe String -> Bool
           -> Bool -> Bool -> Mode -> String -> Request -> [String] -> IO ()
installCmd dryrun debug yes mhuburl mpkgsurl listmode latest noreinstall mode disttag request pkgbldtsks = do
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (defaultPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  -- FIXME use this location?
  printDlDir <- setDownloadDir dryrun "rpms"
  when debug printDlDir
  setNoBuffering
  mapM (kojiRPMs huburl pkgsurl printDlDir) pkgbldtsks
    >>= installRPMs dryrun noreinstall yes . mconcat
  where
    kojiRPMs :: String -> String -> IO () -> String -> IO [(Existence,NVRA)]
    kojiRPMs huburl pkgsurl printDlDir bldtask =
      if all isDigit bldtask
      then kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode noreinstall mode printDlDir bldtask
      else kojiBuildRPMs huburl pkgsurl printDlDir bldtask

    kojiBuildRPMs :: String -> String -> IO () -> String
                  -> IO [(Existence,NVRA)]
    kojiBuildRPMs huburl pkgsurl printDlDir pkgbld = do
      nvrs <- map readNVR <$> kojiBuildOSBuilds debug huburl listmode latest disttag request pkgbld
      if listmode
        then do
        if mode /= PkgsReq [] []
          then error' "modes not supported for listing build"  -- FIXME
          else case nvrs of
                 [nvr] -> do
                   putStrLn (showNVR nvr)
                   putStrLn ""
                   kojiGetBuildRPMs huburl nvr >>=
                     mapM_ putStrLn . sort . filter (not . debugPkg)
                 _ -> mapM_ (putStrLn . showNVR) nvrs
        return []
        else
        case nvrs of
          [] -> error' $ pkgbld ++ " not found for " ++ disttag
          [nvr] -> do
            putStrLn $ showNVR nvr ++ "\n"
            nvras <- sort . map readNVRA . filter (not . debugPkg) <$> kojiGetBuildRPMs huburl nvr
            when debug $ mapM_ (putStrLn . showNVRA) nvras
            dlRpms <- decideRpms yes listmode noreinstall mode (nvrName nvr) nvras
            when debug $ mapM_ printInstalled dlRpms
            unless (dryrun || null dlRpms) $ do
              downloadRpms debug (buildURL nvr) dlRpms
              -- FIXME once we check file size - can skip if no downloads
              printDlDir
            return dlRpms
          _ -> error $ "multiple build founds for " ++ pkgbld ++ ": " ++
               unwords (map showNVR nvrs)
        where
          buildURL :: NVR -> String -> String
          buildURL (NVR n (VerRel v r)) rpm =
             let arch = rpmArch (readNVRA rpm)
             in pkgsurl +/+ n  +/+ v +/+ r +/+ arch +/+ rpm

          debugPkg :: String -> Bool
          debugPkg p = "-debuginfo-" `isInfixOf` p || "-debugsource-" `isInfixOf` p

kojiTaskRPMs :: Bool -> Bool -> Yes -> String -> String -> Bool -> Bool -> Mode
             -> IO () -> String -> IO [(Existence,NVRA)]
kojiTaskRPMs dryrun debug yes huburl pkgsurl listmode noreinstall mode printDlDir task = do
  let taskid = read task
  mtaskinfo <- Koji.getTaskInfo huburl taskid True
  tasks <- case mtaskinfo of
            Nothing -> error' "failed to get taskinfo"
            Just taskinfo -> do
              when debug $ mapM_ print taskinfo
              case lookupStruct "method" taskinfo :: Maybe String of
                Nothing -> error' $ "no method found for " ++ task
                Just method ->
                  case method of
                    "build" -> Koji.getTaskChildren huburl taskid False
                    "buildArch" -> return [taskinfo]
                    _ -> error' $ "unsupport method: " ++ method
  sysarch <- cmd "rpm" ["--eval", "%{_arch}"]
  let archtid =
        case find (\t -> lookupStruct "arch" t == Just sysarch) tasks of
          Nothing -> error' $ "no " ++ sysarch ++ " task found"
          Just task' ->
            case lookupStruct "id" task' of
              Nothing -> error' "task id not found"
              Just tid -> tid
  nvras <- map readNVRA <$> getTaskRPMs archtid
  let srpm =
        case filter ((== "src") . rpmArch) nvras of
          [src] -> src
          _ -> error' "could not determine nvr from any srpm"
      nvr = dropArch srpm
  if listmode
    then decideRpms yes listmode noreinstall mode (nvrName nvr) nvras
    else
    if null nvras
    then do
      kojiTaskRPMs dryrun debug yes huburl pkgsurl True noreinstall mode printDlDir task >>= mapM_ printInstalled
      return []
    else do
      when debug $ print $ map showNVRA nvras
      dlRpms <- decideRpms yes listmode noreinstall mode (nvrName nvr) $ nvras \\ [srpm]
      when debug $ mapM_ printInstalled dlRpms
      unless (dryrun || null dlRpms) $ do
        downloadRpms debug (taskRPMURL task) dlRpms
        printDlDir
      return dlRpms
  where
    getTaskRPMs :: Int -> IO [String]
    getTaskRPMs taskid =
       sort . filter (".rpm" `isExtensionOf`) . map fst <$>
       Koji.listTaskOutput huburl taskid False True False

    taskRPMURL :: String -> String -> String
    taskRPMURL taskid rpm =
      let lastFew =
            let few = dropWhile (== '0') $ takeEnd 4 taskid in
              if null few then "0" else few
      in dropSuffix "packages" pkgsurl +/+ "work/tasks/" ++ lastFew +/+ taskid +/+ rpm

data Existence = NotInstalled | NVRInstalled | NVRChanged
  deriving (Eq, Ord, Show)

decideRpms :: Yes -> Bool -> Bool -> Mode -> String -> [NVRA]
           -> IO [(Existence,NVRA)]
decideRpms yes listmode noreinstall mode base nvras = do
  classified <- mapM installExists (filter isBinaryRpm nvras)
  if listmode
    then mapM_ printInstalled classified >> return []
    else
    case mode of
      All -> return classified
      Ask -> mapMaybeM rpmPrompt classified
      PkgsReq [] [] ->
        if all ((== NotInstalled) . fst) classified && yes /= Yes
        then decideRpms yes listmode noreinstall Ask base nvras
        else
          let install = filter ((/= NotInstalled) . fst) classified
          in if yes == Yes
          then return install
          else do
            mapM_ printInstalled install
            ok <- prompt "install above"
            return $ if ok then install else []
      PkgsReq subpkgs exclpkgs ->
        return $ selectRPMs base (subpkgs,exclpkgs) classified
  where
    installExists :: NVRA -> IO (Existence, NVRA)
    installExists nvra = do
      minstalled <- cmdMaybe "rpm" ["-q", rpmName nvra]
      return
        (case minstalled of
           Nothing -> NotInstalled
           Just installed ->
             if installed == showNVRA nvra then NVRInstalled else NVRChanged,
         nvra)

renderInstalled :: (Existence, NVRA) -> String
renderInstalled (exist, nvra) = showNVRA nvra ++ " (" ++ show exist ++ ")"

printInstalled :: (Existence, NVRA) -> IO ()
printInstalled = putStrLn . renderInstalled

selectRPMs :: String -> ([String],[String]) -> [(Existence,NVRA)]
           -> [(Existence,NVRA)]
selectRPMs base (subpkgs,[]) rpms =
  sort . mconcat $
  flip map subpkgs $ \ pkgpat ->
  case filter (match (compile pkgpat) . rpmName . snd) rpms of
    [] -> if head pkgpat /= '*'
          then selectRPMs base ([base ++ '-' : pkgpat],[]) rpms
          else error' $ "no subpackage match for " ++ pkgpat
    result -> result
selectRPMs base ([], subpkgs) rpms =
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
                  (base ++ '-' : pat) == rpmname
             else match comppat rpmname
selectRPMs base (subpkgs,exclpkgs) rpms =
  let needed = selectRPMs base (subpkgs,[]) rpms
      excluded = selectRPMs base ([], exclpkgs) rpms
  in nub . sort $ needed ++ excluded

prompt :: String -> IO Bool
prompt str = do
  putStr $ str ++ " [y/n]: "
  c <- getChar
  unless (c == '\n') $ putStrLn ""
  case toLower c of
    'y' -> return True
    'n' -> return False
    _ -> prompt str

rpmPrompt :: (Existence,NVRA) -> IO (Maybe (Existence,NVRA))
rpmPrompt (exist,nvra) = do
  ok <- prompt $ renderInstalled (exist,nvra)
  return $
    if ok
    then Just (exist,nvra)
    else Nothing

kojiBuildOSBuilds :: Bool -> String -> Bool -> Bool -> String -> Request
                  -> String -> IO [String]
kojiBuildOSBuilds debug hub listmode latest disttag request pkgpat = do
  when debug $ putStrLn pkgpat
  let (pkg,full) = packageOfPattern request pkgpat
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

kojiGetBuildRPMs :: String -> NVR -> IO [String]
kojiGetBuildRPMs huburl nvr = do
  mbid <- kojiGetBuildID huburl (showNVR nvr)
  case mbid of
    Nothing -> error $ "Build id not found for " ++ showNVR nvr
    Just (BuildId bid) -> do
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

-- data Classify = ChangeNVR | SameNVR
--   deriving (Eq, Ord)

installRPMs :: Bool -> Bool -> Yes -> [(Existence,NVRA)] -> IO ()
installRPMs _ _ _ [] = return ()
installRPMs dryrun noreinstall yes classified = do
  forM_ (groupSort classified) $ \(cl,pkgs) ->
    unless (null pkgs) $
    let mdnfcmd =
          case cl of
            NVRInstalled -> if noreinstall then Nothing else Just "reinstall"
            _ -> Just "localinstall"
    in whenJust mdnfcmd $ \dnfcmd ->
      if dryrun
      then mapM_ putStrLn $ ("would " ++ dnfcmd ++ ":") : map showNVRA pkgs
      else sudo_ "dnf" $ dnfcmd : map showNVRA pkgs ++ ["--assumeyes" | yes == Yes]
--  where
    -- classifyInstall :: String -> IO (Classify,String)
    -- classifyInstall rpm = do
    --   minstalled <- cmdMaybe "rpm" ["-q", nvraName rpm]
    --   case minstalled of
    --       Nothing -> return (ChangeNVR,rpm)
    --       Just installed -> return
    --         (if installed == rpm
    --          then SameNVR
    --          else ChangeNVR,
    --          rpm)

downloadRpms :: Bool -> (String -> String) -> [(Existence,NVRA)] -> IO ()
downloadRpms debug urlOf rpms = do
  urls <- fmap catMaybes <$>
    forM (map snd rpms) $ \nvra -> do
    let rpm = showNVRA nvra <.> "rpm"
    exists <- doesFileExist rpm
    let url = urlOf rpm
    notfile <-
      if exists
      then do
        old <- outOfDate rpm url
        when old $ removeFile rpm
        return old
      else return True
    when notfile $ putStrLn $ if debug then url else rpm
    return $ if notfile then Just url else Nothing
  unless (null urls) $ do
    mapM_ putStrLn urls
    putStrLn "downloading..."
    cmd_ "curl" $ ["--remote-time", "--fail", "-C-", "--show-error", "--remote-name-all", "--progress-bar"] ++ urls
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
