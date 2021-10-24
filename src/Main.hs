-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.RPM
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath ((<.>))
import System.IO

import DownloadDir
import Paths_koji_install (version)

data InstallMode = Update | All | Ask | Base | NoDevel

-- FIXME --include devel, --exclude *
-- FIXME specify tag or task
-- FIXME support enterprise builds
-- FIXME --arch (including src)
-- FIXME --debuginfo
-- FIXME --delete after installing
main :: IO ()
main = do
  sysdisttag <- cmd "rpm" ["--eval", "%{dist}"]
  simpleCmdArgs (Just Paths_koji_install.version) "Install latest build from Koji"
    "Download and install latest package build from Koji tag." $
    program
    <$> dryrunOpt
    <*> modeOpt
    <*> disttagOpt sysdisttag
    <*> some (strArg "PACKAGE")
  where
    dryrunOpt = switchWith 'n' "dry-run" "Don't actually download anything"

    modeOpt :: Parser InstallMode
    modeOpt =
      flagWith' All 'a' "all" "all subpackages" <|>
      flagWith' Ask 'A' "ask" "ask for each subpackge" <|>
      flagWith' Base 'b' "base-only" "only base package" <|>
      flagWith' NoDevel 'D' "exclude-devel" "Skip devel packages" <|>
      pure Update

    disttagOpt :: String -> Parser String
    disttagOpt disttag = startingDot <$> strOptionalWith 'd' "disttag" "DISTTAG" ("Use a different disttag [default: " ++ disttag ++ "]") disttag

    startingDot cs =
      case cs of
        "" -> error' "empty disttag"
        (c:_) -> if c == '.' then cs else '.' : cs


program :: Bool -> InstallMode -> String -> [String] -> IO ()
program dryrun mode disttag pkgs = do
  -- FIXME use this?
  dlDir <- setDownloadDir dryrun "rpms"
  setNoBuffering
  mapM (kojiLatestRPMs dlDir) pkgs >>= installRPMs dryrun . mconcat
  where
    kojiLatestRPMs :: String -> String -> IO [String]
    kojiLatestRPMs dlDir pkg = do
      mnvr <- kojiLatestOSBuild fedoraKojiHub disttag pkg
      case mnvr of
        Nothing -> error' $ "latest " ++ pkg ++ " not found"
        Just nvr -> do
          putStrLn $ nvr ++ "\n"
          allRpms <- map (<.> "rpm") . sort . filter (not . debugPkg) <$> kojiGetBuildRPMs nvr
          dlRpms <- decideRpms mode allRpms
          unless (dryrun || null dlRpms) $ do
            mapM_ (downloadRpm (readNVR nvr)) dlRpms
            -- FIXME once we check file size - can skip if no downloads
            putStrLn $ "Packages downloaded to " ++ dlDir
          return dlRpms
      where
        decideRpms :: InstallMode -> [String] -> IO [String]
        decideRpms mode' allRpms =
          case mode' of
            All -> return allRpms
            Ask -> mapMaybeM rpmPrompt allRpms
            Base -> return $ pure $ minimumOn length $ filter (pkg `isPrefixOf`) allRpms
            Update -> do
              rpms <- filterM (isInstalled . rpmName . readNVRA) allRpms
              if null rpms
                then decideRpms Ask allRpms
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

kojiLatestOSBuild :: String -> String -> String -> IO (Maybe String)
kojiLatestOSBuild hub disttag pkgpat = do
  let (pkg,full) = packageOfPattern pkgpat
  mpkgid <- Koji.getPackageID hub pkg
  case mpkgid of
    Nothing -> error $ "package not found: " ++ pkg
    Just pkgid -> do
      let opts = [("packageID", ValueInt pkgid),
                  ("queryOpts",ValueStruct [("limit",ValueInt 1),
                                            ("order",ValueString "-build_id")])]
      res <- Koji.listBuilds hub $
             ("pattern", ValueString (if full then pkgpat else dropSuffix "*" pkgpat ++ "*" ++ disttag)) : opts
      case res of
        [] -> return Nothing
        [bld] -> return $ lookupStruct "nvr" bld
        _ -> error $ "more than one latest build found for " ++ pkg
  where
    packageOfPattern :: String -> (String, Bool)
    packageOfPattern pat =
      case maybeNVR pat of
        Just (NVR n _) -> (n, True)
        Nothing ->
          case maybeNV pat of
            Just (NV n _) -> (n, False)
            Nothing -> (dropSuffix "-" $ takeWhile (/= '*') pat, False)


kojiGetBuildRPMs :: String -> IO [String]
kojiGetBuildRPMs nvr = do
  mbid <- kojiGetBuildID fedoraKojiHub nvr
  case mbid of
    Nothing -> error $ "Build id not found for " ++ nvr
    Just (BuildId bid) -> do
      rpms <- Koji.listBuildRPMs fedoraKojiHub bid
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
downloadRpm :: NVR -> String -> IO ()
downloadRpm (NVR n (VerRel v r)) rpm = do
  unlessM (doesFileExist rpm) $ do
    let arch = rpmArch (readNVRA rpm)
        url = "https://kojipkgs.fedoraproject.org/packages" +/+ n  +/+ v +/+ r +/+ arch +/+ rpm
    putStrLn $ "Downloading " ++ rpm
    cmd_ "curl" ["--silent", "-C-", "--show-error", "--remote-name", url]

-- from next http-directory or http-query
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
