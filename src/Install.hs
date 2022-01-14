{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Install (
  Mode(..),
  Request(..),
  installCmd,
  knownHubs
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

import DownloadDir

-- mbox kojihub is locked
knownHubs :: [String]
knownHubs = ["fedora","stream","rpmfusion", "or URL"]

hubURL :: String -> String
hubURL "fedora" = fedoraKojiHub
-- later use centosKojiHub
hubURL "stream" = "https://kojihub.stream.centos.org/kojihub"
--hubURL "mbox" = "https://koji.mbox.centos.org/kojihub"
hubURL "rpmfusion" = "https://koji.rpmfusion.org/kojihub"
hubURL "fusion" = "https://koji.rpmfusion.org/kojihub"
hubURL hub =
  if "http" `isPrefixOf` hub
  then hub
  else error' $ "unknown hub: try " ++ show knownHubs

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
installCmd :: Bool -> Bool -> Maybe String -> Maybe String -> Bool -> Bool
           -> Mode -> String -> Request -> [String] -> IO ()
installCmd dryrun debug mhuburl mpkgsurl listmode latest mode disttag request pkgbldtsks = do
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (defaultPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  -- FIXME use this location?
  dlDir <- setDownloadDir dryrun "rpms"
  when debug $ putStrLn dlDir
  setNoBuffering
  mapM (kojiRPMs huburl pkgsurl dlDir) pkgbldtsks
    >>= if listmode
        then mapM_ putStrLn . mconcat
        else installRPMs dryrun . mconcat
  where
    kojiRPMs :: String -> String -> String -> String -> IO [String] -- ([String],String)
    kojiRPMs huburl pkgsurl dlDir bldtask =
      if all isDigit bldtask
      then kojiTaskRPMs dryrun debug huburl pkgsurl listmode mode dlDir bldtask
      else kojiBuildRPMs huburl pkgsurl dlDir bldtask

    kojiBuildRPMs :: String -> String -> String -> String -> IO [String]
    kojiBuildRPMs huburl pkgsurl dlDir pkgbld = do
      nvrs <- kojiBuildOSBuilds debug huburl listmode latest disttag request pkgbld
      if listmode
        then if mode /= PkgsReq [] []
                -- FIXME:
             then error' "modes not supported for listing build"
             else case nvrs of
                    [nvr] -> ([nvr,""] ++) . map (<.> "rpm") . sort . filter (not . debugPkg) <$> kojiGetBuildRPMs huburl nvr
                    _ -> return nvrs
        else
        case nvrs of
          [] -> error' $ pkgbld ++ " not found for " ++ disttag
          [nvr] -> do
            putStrLn $ nvr ++ "\n"
            allRpms <- map (<.> "rpm") . sort . filter (not . debugPkg) <$> kojiGetBuildRPMs huburl nvr
            when debug $ print allRpms
            dlRpms <- decideRpms listmode mode (maybeNVRName nvr) allRpms
            when debug $ print dlRpms
            unless (dryrun || null dlRpms) $ do
              downloadRpms debug (buildURL (readNVR nvr)) dlRpms
              -- FIXME once we check file size - can skip if no downloads
              putStrLn $ "Packages downloaded to " ++ dlDir
            return dlRpms
          _ -> error $ "multiple build founds for " ++ pkgbld ++ ": " ++
               unwords nvrs
        where
          buildURL :: NVR -> String -> String
          buildURL (NVR n (VerRel v r)) rpm =
             let arch = rpmArch (readNVRA rpm)
             in pkgsurl +/+ n  +/+ v +/+ r +/+ arch +/+ rpm

          debugPkg :: String -> Bool
          debugPkg p = "-debuginfo-" `isInfixOf` p || "-debugsource-" `isInfixOf` p

kojiTaskRPMs :: Bool -> Bool -> String -> String -> Bool -> Mode -> String
             -> String -> IO [String]
kojiTaskRPMs dryrun debug huburl pkgsurl listmode mode dlDir task = do
  let taskid = read task
  if listmode
    then do
    mtaskinfo <- Koji.getTaskInfo huburl taskid True
    case mtaskinfo of
      Just taskinfo -> do
        when debug $ mapM_ print taskinfo
        if isNothing (lookupStruct "parent" taskinfo :: Maybe Int)
          then do
          children <- Koji.getTaskChildren huburl taskid False
          return $ fromMaybe "" (showTask taskinfo) : mapMaybe showChildTask children
          else getTaskRPMs taskid >>= decideRpms listmode mode Nothing
      Nothing -> error' "failed to get taskinfo"
    else do
    rpms <- getTaskRPMs taskid
    if null rpms
      then do
      kojiTaskRPMs dryrun debug huburl pkgsurl True mode dlDir task >>= mapM_ putStrLn
      return []
      else do
      when debug $ print rpms
      let srpm =
            case filter (".src.rpm" `isExtensionOf`) rpms of
              [src] -> src
              _ -> error' "could not determine nvr from any srpm"
          nvr = dropSuffix ".src.rpm" srpm
      dlRpms <- decideRpms listmode mode (maybeNVRName nvr) $ rpms \\ [srpm]
      when debug $ print dlRpms
      unless (dryrun || null dlRpms) $ do
        downloadRpms debug (taskRPMURL task) dlRpms
        putStrLn $ "Packages downloaded to " ++ dlDir
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

maybeNVRName :: String -> Maybe String
maybeNVRName = fmap nvrName . maybeNVR

decideRpms :: Bool -> Mode -> Maybe String -> [String] -> IO [String]
decideRpms listmode mode mbase allRpms =
  case mode of
    All -> if listmode
           then error' "cannot use --list and --all together"
           else return allRpms
    Ask -> if listmode
           then error' "cannot use --list and --ask together"
           else mapMaybeM rpmPrompt allRpms
    PkgsReq [] [] ->
      if listmode
      then return allRpms
      else do
      rpms <- filterM (isInstalled . nvraName) $
              filter isBinaryRpm allRpms
      if null rpms
        then decideRpms listmode Ask mbase allRpms
        else return rpms
    PkgsReq subpkgs exclpkgs ->
      return $ selectRPMs mbase (subpkgs,exclpkgs) allRpms

isInstalled :: String -> IO Bool
isInstalled rpm = cmdBool "rpm" ["--quiet", "-q", rpm]

selectRPMs :: Maybe String -> ([String],[String])  -> [String] -> [String]
selectRPMs mbase (subpkgs,[]) rpms =
  sort . mconcat $
  flip map subpkgs $ \ pkgpat ->
  case filter (match (compile pkgpat) . nvraName) rpms of
    [] -> case mbase of
      Just base | head pkgpat /= '*' ->
                  selectRPMs Nothing ([base ++ '-' : pkgpat],[]) rpms
      _ -> error' $ "no subpackage match for " ++ pkgpat
    result -> result
selectRPMs mbase ([], subpkgs) rpms =
  -- FIXME somehow determine unused excludes
  foldl' (exclude subpkgs) [] rpms
  where
    rpmnames = map nvraName rpms

    exclude :: [String] -> [String] -> String -> [String]
    exclude [] acc rpm = acc ++ [rpm]
    exclude (pat:pats) acc rpm =
        if checkMatch (nvraName rpm)
        then acc
        else exclude pats acc rpm
      where
        checkMatch :: String -> Bool
        checkMatch rpmname =
          let comppat = compile pat
          in if isLiteral comppat
             then pat == rpmname ||
                  pat `notElem` rpmnames &&
                  maybe False (\b -> (b ++ '-' : pat) == rpmname) mbase
             else match comppat rpmname
selectRPMs mbase (subpkgs,exclpkgs) rpms =
  let needed = selectRPMs mbase (subpkgs,[]) rpms
      excluded = selectRPMs mbase ([], exclpkgs) rpms
  in nub . sort $ needed ++ excluded

nvraName :: String -> String
nvraName = rpmName . readNVRA

rpmPrompt :: String -> IO (Maybe String)
rpmPrompt rpm = do
  putStr $ rpm ++ " [y/n]: "
  c <- getChar
  putStrLn ""
  case toLower c of
    'y' -> return $ Just rpm
    'n' -> return Nothing
    _ -> rpmPrompt rpm

kojiBuildOSBuilds :: Bool -> String -> Bool -> Bool -> String -> Request
                  -> String -> IO [String]
kojiBuildOSBuilds debug hub listmode latest disttag request pkgpat = do
  when debug $ putStrLn pkgpat
  let (pkg,full) = packageOfPattern request pkgpat
      oldkoji = "rpmfusion" `isInfixOf` hub
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
                  ("queryOpts",ValueStruct
                    [("limit",ValueInt $ if listmode && not latest || oldkoji then 10 else 1),
                     ("order",ValueString "-build_id")])]
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

kojiGetBuildRPMs :: String -> String -> IO [String]
kojiGetBuildRPMs huburl nvr = do
  mbid <- kojiGetBuildID huburl nvr
  case mbid of
    Nothing -> error $ "Build id not found for " ++ nvr
    Just (BuildId bid) -> do
      rpms <- Koji.listBuildRPMs huburl bid
      sysarch <- cmd "rpm" ["--eval", "%{_arch}"]
      return $ map getNVRA $ filter (forArch sysarch) rpms
   where
     forArch :: String -> Struct -> Bool
     forArch sysarch st =
       case lookupStruct "arch" st of
         Just arch -> arch `elem` [sysarch, "noarch"]
         Nothing -> error $ "No arch found for rpm for: " ++ nvr

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

installRPMs :: Bool -> [FilePath] -> IO ()
installRPMs _ [] = return ()
installRPMs dryrun rpms = do
  installed <- filterM (isInstalled . dropExtension) rpms
  unless (null installed) $
    if dryrun
    then mapM_ putStrLn $ "would update:" : installed
    else sudo_ "dnf" ("reinstall" : installed)
  let rest = rpms \\ installed
  unless (null rest) $
    if dryrun
    then mapM_ putStrLn $ "would install:" : rest
    else sudo_ "dnf" ("localinstall" : rest)

downloadRpms :: Bool -> (String -> String) -> [String] -> IO ()
downloadRpms debug urlOf rpms = do
  urls <- fmap catMaybes <$>
    forM rpms $ \rpm -> do
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

showTask :: Struct -> Maybe String
showTask struct = do
  state <- getTaskState struct
  request <- lookupStruct "request" struct
  method <- lookupStruct "method" struct
  let mparent = lookupStruct "parent" struct :: Maybe Int
      showreq = takeWhileEnd (/= '/') . unwords . mapMaybe getString . take 3
  return $ showreq request +-+ method +-+ (if state == TaskClosed then "" else show state) +-+ maybe "" (\p -> "(" ++ show p ++ ")") mparent

showChildTask :: Struct -> Maybe String
showChildTask struct = do
  arch <- lookupStruct "arch" struct
  state <- getTaskState struct
  method <- lookupStruct "method" struct
  taskid <- lookupStruct "id" struct
  return $ arch ++ replicate (8 - length arch) ' ' +-+ show (taskid :: Int) +-+ method +-+ show state

isBinaryRpm :: FilePath -> Bool
isBinaryRpm file =
  ".rpm" `isExtensionOf` file && not (".src.rpm" `isExtensionOf` file)

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
