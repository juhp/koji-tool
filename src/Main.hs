-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.RPM
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath ((<.>), isExtensionOf)
import System.IO

import DownloadDir
import Paths_koji_install (version)

-- FIXME use subtype for listing vs installation
data Mode = List | InstMode InstallMode
  deriving Eq

data InstallMode = Update | All | Ask | Base | NoDevel
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
    <*> modeOpt
    <*> disttagOpt sysdisttag
    <*> (flagWith' ReqNVR 'R' "nvr" "Give an N-V-R instead of package name"
         <|> flagWith ReqName ReqNVR 'V' "nv" "Give an N-V instead of package name")
    <*> some (strArg "PACKAGE|TASKID...")
  where
    modeOpt :: Parser Mode
    modeOpt =
      flagWith' List 'l' "list" "List builds" <|>
      InstMode <$>
      (flagWith' All 'a' "all" "all subpackages" <|>
      flagWith' Ask 'A' "ask" "ask for each subpackge" <|>
      flagWith' Base 'b' "base-only" "only base package" <|>
      flagWith' NoDevel 'D' "exclude-devel" "Skip devel packages" <|>
      pure Update)

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
      if "kojihub" `isExtensionOf` url
      then replace "kojihub" "kojifiles" url
      else error' $ "use --files-url to specify kojifiles url for " ++ url

program :: Bool -> Bool -> Maybe String -> Maybe String -> Mode
        -> String -> Request -> [String] -> IO ()
program dryrun debug mhuburl mpkgsurl mode disttag request pkgs = do
  let huburl = maybe fedoraKojiHub hubURL mhuburl
      pkgsurl = fromMaybe (defaultPkgsURL huburl) mpkgsurl
  when debug $ do
    putStrLn huburl
    putStrLn pkgsurl
  -- FIXME use this?
  dlDir <- setDownloadDir dryrun "rpms"
  when debug $ putStrLn dlDir
  setNoBuffering
  mapM (kojiRPMs huburl pkgsurl dlDir) pkgs
    >>= case mode of
          List -> mapM_ putStrLn . mconcat
          _ -> installRPMs dryrun . mconcat
  where
    kojiRPMs :: String -> String -> String -> String -> IO [String]
    kojiRPMs huburl pkgsurl dlDir pkg =
      if all isDigit pkg
      then kojiTaskRPMs huburl pkgsurl dlDir pkg
      else kojiBuildRPMs huburl pkgsurl dlDir pkg

    kojiTaskRPMs :: String -> String -> String -> String -> IO [String]
    kojiTaskRPMs huburl pkgsurl dlDir task = do
      let taskid = read task
      case mode of
        List -> do
          -- FIXME handle parent task
          taskreq <- Koji.getTaskRequest huburl taskid
          case taskreq of
                ValueArray req -> do
                  children <- Koji.getTaskChildren huburl taskid False
                  return $ showTaskReq req : mapMaybe showChildTask children
                _ -> error' "taskinfo request not found"
        InstMode instmode -> do
          rpms <- filter (".rpm" `isSuffixOf`) . map fst <$> Koji.listTaskOutput huburl taskid False True False
          dlRpms <- decideRpms instmode Nothing rpms
          unless (dryrun || null dlRpms) $ do
            mapM_ (downloadTaskRpm pkgsurl task) dlRpms
            putStrLn $ "Packages downloaded to " ++ dlDir
          return dlRpms

    kojiBuildRPMs :: String -> String -> String -> String -> IO [String]
    kojiBuildRPMs huburl pkgsurl dlDir pkg = do
      nvrs <- kojiBuildOSBuilds debug huburl (mode == List) disttag request pkg
      case mode of
        List -> return nvrs
        InstMode instmode ->
          case nvrs of
            [] -> error' $ pkg ++ " not found for " ++ disttag
            [nvr] -> do
              putStrLn $ nvr ++ "\n"
              allRpms <- map (<.> "rpm") . sort . filter (not . debugPkg) <$> kojiGetBuildRPMs huburl nvr
              dlRpms <- decideRpms instmode (Just pkg) allRpms
              unless (dryrun || null dlRpms) $ do
                mapM_ (downloadBuildRpm pkgsurl (readNVR nvr)) dlRpms
                -- FIXME once we check file size - can skip if no downloads
                putStrLn $ "Packages downloaded to " ++ dlDir
              return dlRpms
            _ -> error $ "multiple build founds for " ++ pkg ++ ": " ++
                 unwords nvrs

    decideRpms :: InstallMode -> Maybe String -> [String] -> IO [String]
    decideRpms mode' mpkg allRpms =
      case mode' of
        All -> return allRpms
        Ask -> mapMaybeM rpmPrompt allRpms
        Base ->
          let predicate = maybe (const True) isPrefixOf mpkg
          in return $ pure $ minimumOn length $ filter predicate allRpms
        Update -> do
          rpms <- filterM (isInstalled . rpmName . readNVRA) allRpms
          if null rpms
            then decideRpms Ask mpkg allRpms
            else return rpms
        NoDevel -> return $ filter (not . ("-devel-" `isInfixOf`)) allRpms

    rpmPrompt :: String -> IO (Maybe String)
    rpmPrompt rpm = do
      putStr $ rpm ++ " [y/n]: "
      c <- getChar
      putStrLn ""
      case toLower c of
        'y' -> return $ Just rpm
        'n' -> return Nothing
        _ -> rpmPrompt rpm

    isInstalled :: String -> IO Bool
    isInstalled rpm = cmdBool "rpm" ["--quiet", "-q", rpm]

    debugPkg :: String -> Bool
    debugPkg p = "-debuginfo-" `isInfixOf` p || "-debugsource-" `isInfixOf` p

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
      return $ map getNVRA $ filter (forArch "x86_64") rpms
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
installRPMs dryrun pkgs =
  unless dryrun $
  sudo_ "dnf" ("install" : map ("./" ++) pkgs)

-- FIXME check file size
downloadBuildRpm :: String -> NVR -> String -> IO ()
downloadBuildRpm pkgsurl (NVR n (VerRel v r)) rpm = do
  unlessM (doesFileExist rpm) $ do
    let arch = rpmArch (readNVRA rpm)
        url = pkgsurl +/+ n  +/+ v +/+ r +/+ arch +/+ rpm
    putStrLn $ "Downloading " ++ rpm
    cmd_ "curl" ["--fail", "--silent", "-C-", "--show-error", "--remote-name", url]

downloadTaskRpm :: String -> String -> String -> IO ()
downloadTaskRpm pkgsurl taskid rpm = do
  unlessM (doesFileExist rpm) $ do
    let url = pkgsurl +/+ "work/tasks/" ++ takeEnd 4 taskid +/+ taskid +/+ rpm
    putStrLn $ "Downloading " ++ rpm
    cmd_ "curl" ["--fail", "--silent", "-C-", "--show-error", "--remote-name", url]

showTaskReq :: [Value] -> String
showTaskReq = unwords . mapMaybe getString . take 2

showChildTask :: Struct -> Maybe String
showChildTask struct = do
  arch <- lookupStruct "arch" struct
  state <- getTaskState struct
  taskid <- lookupStruct "id" struct
  return $ arch ++ " " ++ show (taskid :: Int) ++ " " ++ show state

-- from next http-directory or http-query
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
