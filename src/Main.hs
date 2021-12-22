{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.RPM
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import Network.HTTP.Directory (httpFileSize', httpLastModified', (+/+))
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.IO

import DownloadDir
import Paths_koji_install (version)

data Mode = Update
          | All
          | Ask
          | PkgsReq [String] [String] -- ^ included and excluded
  deriving Eq

data Request = ReqName | ReqNV | ReqNVR
  deriving Eq

-- FIXME --include devel, --exclude *
-- FIXME specify tag or task
-- FIXME support enterprise builds
-- FIXME --arch (including src)
-- FIXME --debuginfo
-- FIXME --delete after installing

main :: IO ()
main = do
  sysdisttag <- do
    dist <- cmd "rpm" ["--eval", "%{dist}"]
    return $ if dist == "%{dist}" then "" else dist
  simpleCmdArgs (Just Paths_koji_install.version)
    "Download and install latest package build from Koji tag."
    ("HUB = " ++ intercalate ", " knownHubs) $
    program
    <$> switchWith 'n' "dry-run" "Don't actually download anything"
    <*> switchWith 'D' "debug" "More detailed output"
    <*> optional (strOptionWith 'H' "hub" "HUB"
                  "KojiHub shortname or url [default: fedora]")
    <*> optional (strOptionWith 'P' "packages-url" "URL"
                  "KojiFiles packages url [default: Fedora]")
    <*> switchWith 'l' "list" "List builds"
    <*> modeOpt
    <*> disttagOpt sysdisttag
    <*> (flagWith' ReqNVR 'R' "nvr" "Give an N-V-R instead of package name"
         <|> flagWith ReqName ReqNVR 'V' "nv" "Give an N-V instead of package name")
    <*> some (strArg "PACKAGE|TASKID...")
  where
    modeOpt :: Parser Mode
    modeOpt =
      flagWith' All 'a' "all" "all subpackages" <|>
      flagWith' Ask 'A' "ask" "ask for each subpackge [default if not installed]" <|>
      pkgsReqOpts <|>
      pure Update

    pkgsReqOpts = PkgsReq
      <$> many (strOptionWith 'p' "package" "SUBPKG" "Subpackage (glob) to install") <*> many (strOptionWith 'x' "exclude" "SUBPKG" "Subpackage (glob) not to install")

    disttagOpt :: String -> Parser String
    disttagOpt disttag = startingDot <$> strOptionalWith 'd' "disttag" "DISTTAG" ("Use a different disttag [default: " ++ disttag ++ "]") disttag

    startingDot cs =
      case cs of
        "" -> error' "empty disttag"
        (c:_) -> if c == '.' then cs else '.' : cs

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

program :: Bool -> Bool -> Maybe String -> Maybe String -> Bool -> Mode
        -> String -> Request -> [String] -> IO ()
program dryrun debug mhuburl mpkgsurl listmode mode disttag request pkgs = do
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (defaultPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  -- FIXME use this location?
  dlDir <- setDownloadDir dryrun "rpms"
  when debug $ putStrLn dlDir
  setNoBuffering
  mapM (kojiRPMs huburl pkgsurl dlDir) pkgs
    >>= if listmode
        then mapM_ putStrLn . mconcat
        else installRPMs dryrun . mconcat
  where
    kojiRPMs :: String -> String -> String -> String -> IO [String]
    kojiRPMs huburl pkgsurl dlDir pkg =
      if all isDigit pkg
      then kojiTaskRPMs dryrun debug huburl pkgsurl listmode mode dlDir pkg
      else kojiBuildRPMs huburl pkgsurl dlDir pkg

    kojiBuildRPMs :: String -> String -> String -> String -> IO [String]
    kojiBuildRPMs huburl pkgsurl dlDir pkg = do
      nvrs <- kojiBuildOSBuilds debug huburl listmode disttag request pkg
      if listmode
        then if mode /= Update
             then error' "modes not supported for listing build"
             else return nvrs
        else
        case nvrs of
          [] -> error' $ pkg ++ " not found for " ++ disttag
          [nvr] -> do
            putStrLn $ nvr ++ "\n"
            allRpms <- map (<.> "rpm") . sort . filter (not . debugPkg) <$> kojiGetBuildRPMs huburl nvr
            when debug $ print allRpms
            dlRpms <- decideRpms listmode mode (Just pkg) allRpms
            when debug $ print dlRpms
            unless (dryrun || null dlRpms) $ do
              mapM_ (downloadBuildRpm debug pkgsurl (readNVR nvr)) dlRpms
              -- FIXME once we check file size - can skip if no downloads
              putStrLn $ "Packages downloaded to " ++ dlDir
            return dlRpms
          _ -> error $ "multiple build founds for " ++ pkg ++ ": " ++
               unwords nvrs

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
      dlRpms <- decideRpms listmode mode Nothing rpms
      when debug $ print dlRpms
      unless (dryrun || null dlRpms) $ do
        mapM_ (downloadTaskRpm debug pkgsurl task) dlRpms
        putStrLn $ "Packages downloaded to " ++ dlDir
      return dlRpms
  where
    getTaskRPMs :: Int -> IO [String]
    getTaskRPMs taskid =
       sort . filter isBinaryRpm . map fst <$>
       Koji.listTaskOutput huburl taskid False True False

decideRpms :: Bool -> Mode -> Maybe String -> [String] -> IO [String]
decideRpms listmode mode mpkg allRpms =
  case mode of
    All -> if listmode
           then error' "cannot use --list and --all together"
           else return allRpms
    Ask -> if listmode
           then error' "cannot use --list and --ask together"
           else mapMaybeM rpmPrompt allRpms
    Update ->
      if listmode
      then return allRpms
      else do
      rpms <- filterM (isInstalled . rpmName . readNVRA) allRpms
      if null rpms
        then decideRpms listmode Ask mpkg allRpms
        else return rpms
    PkgsReq subpkgs exclpkgs ->
      return $ selectRPMs (subpkgs,exclpkgs) allRpms

isInstalled :: String -> IO Bool
isInstalled rpm = cmdBool "rpm" ["--quiet", "-q", rpm]

selectRPMs :: ([String],[String])  -> [String] -> [String]
selectRPMs (subpkgs,[]) allRpms =
  sort . mconcat $
  flip map subpkgs $ \ pkgpat ->
  case filter (match (compile pkgpat) . nvraName) allRpms of
    [] -> error' $ "no subpackage match for " ++ pkgpat
    result -> result
selectRPMs ([], subpkgs) allRpms =
  -- FIXME somehow determine unused excludes
  foldl' (exclude subpkgs) [] allRpms
  where
    exclude :: [String] -> [String] -> String -> [String]
    exclude [] acc pkg = acc ++ [pkg]
    exclude (p:ps) acc pkg =
      if match (compile p) (nvraName pkg)
      then acc
      else exclude ps acc pkg
selectRPMs (subpkgs,exclpkgs) allRpms =
  let needed = selectRPMs (subpkgs,[]) allRpms
      excluded = selectRPMs ([], exclpkgs) allRpms
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

kojiBuildOSBuilds :: Bool -> String -> Bool -> String -> Request -> String
                  -> IO [String]
kojiBuildOSBuilds debug hub listmode disttag request pkgpat = do
  let (pkg,full) = packageOfPattern pkgpat
      oldkoji = "rpmfusion" `isInfixOf` hub
  when (oldkoji && ("*" `isInfixOf` pkgpat || request /= ReqName)) $
    error' "cannot use pattern with this kojihub"
  mpkgid <- Koji.getPackageID hub pkg
  case mpkgid of
    Nothing -> error $ "package not found: " ++ pkg
    Just pkgid -> do
      -- strictly should getAPIVersion
      let opts = (if oldkoji
                  then id
                  else (("pattern", ValueString (if full then pkgpat else dropSuffix "*" pkgpat ++ "*" ++ disttag ++ "*")) :))
                 [("packageID", ValueInt pkgid),
                  ("state", ValueInt (fromEnum BuildComplete)),
                  ("queryOpts",ValueStruct
                    [("limit",ValueInt $ if listmode || oldkoji then 10 else 1),
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
  where
    packageOfPattern :: String -> (String, Bool)
    packageOfPattern pat =
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
installRPMs dryrun pkgs = do
  installed <- filterM (isInstalled . rpmName . readNVRA) pkgs
  unless (null installed) $
    if dryrun
    then mapM_ putStrLn $ "would update:" : installed
    else sudo_ "dnf" ("reinstall" : installed)
  let rest = pkgs \\ installed
  unless (null rest) $
    if dryrun
    then mapM_ putStrLn $ "would install:" : rest
    else sudo_ "dnf" ("localinstall" : rest)

downloadBuildRpm :: Bool -> String -> NVR -> String -> IO ()
downloadBuildRpm debug pkgsurl (NVR n (VerRel v r)) rpm = do
  let arch = rpmArch (readNVRA rpm)
      url = pkgsurl +/+ n  +/+ v +/+ r +/+ arch +/+ rpm
  downloadRPM debug url

downloadTaskRpm :: Bool -> String -> String -> String -> IO ()
downloadTaskRpm debug pkgsurl taskid rpm = do
  let url = dropSuffix "packages" pkgsurl +/+ "work/tasks/" ++ lastFew +/+ taskid +/+ rpm
  downloadRPM debug url
  where
    lastFew =
      let few = dropWhile (== '0') $ takeEnd 4 taskid in
        if null few then "0" else few

-- FIXME check file size
-- FIXME check timestamp
downloadRPM :: Bool -> String -> IO ()
downloadRPM debug url = do
  let rpm = takeFileName url
  exists <- doesFileExist rpm
  notfile <-
    if exists
    then do
      old <- outOfDate rpm
      when old $ removeFile rpm
      return old
    else return True
  when notfile $ do
    putStrLn $ "Downloading " ++ if debug then url else rpm
    cmd_ "curl" ["--remote-time", "--fail", "--silent", "-C-", "--show-error", "--remote-name", url]
  where
    outOfDate :: String -> IO Bool
    outOfDate file = do
      mremotetime <- httpLastModified' url
      case mremotetime of
        Just remotetime -> do
          localtime <- getModificationTime file
          return $ localtime < remotetime
        Nothing -> do
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
