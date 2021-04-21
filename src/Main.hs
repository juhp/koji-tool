-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Char
import Data.List
import Data.Maybe
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import SimpleCmd
import SimpleCmdArgs
import System.IO

-- FIXME install vs update
-- FIXME --exclude devel, etc
main :: IO ()
main =
  simpleCmdArgs Nothing "Install latest builds from Koji"
    "Download and install latest builds from Koji tag." $
    program <$> switchWith 'a' "all" "all subpackages" <*> some (strArg "PACKAGE")

program :: Bool -> [String] -> IO ()
program allsubpkgs pkgs = do
  disttag <- cmd "rpm" ["--eval", "%{dist}"]
  setNoBuffering
  mapM (kojiLatestRPMs disttag) pkgs >>= installRPMs . mconcat
  where
    kojiLatestRPMs :: String -> String -> IO [String]
    kojiLatestRPMs disttag pkg = do
      mnvr <- kojiLatestOSBuild fedoraKojiHub disttag pkg
      case mnvr of
        Nothing -> error' $ "latest " ++ pkg ++ " not found"
        Just nvr -> do
          putStrLn nvr
          rpms <- sort . mapMaybe (stripPrefix "Downloading: ") <$>
                  cmdLines "koji" ["download-build",
                                   "--noprogress",
                                   "--arch=x86_64",
                                   "--arch=noarch",
                                   nvr]
          if allsubpkgs
            then return rpms
            else mapMaybeM rpmPrompt rpms
    rpmPrompt :: String -> IO (Maybe String)
    rpmPrompt rpm = do
      putStr $ rpm ++ " [y/n]: "
      c <- getChar
      putStrLn ""
      case toLower c of
        'y' -> return $ Just rpm
        'n' -> return $ Nothing
        _ -> rpmPrompt rpm

kojiLatestOSBuild :: String -> String -> String -> IO (Maybe String)
kojiLatestOSBuild hub disttag pkg = do
  mpkgid <- Koji.getPackageID hub pkg
  case mpkgid of
    Nothing -> error $ "package not found: " ++ pkg
    Just pkgid -> do
      let opts = [("packageID", ValueInt pkgid),
                  ("queryOpts",ValueStruct [("limit",ValueInt 1),
                                            ("order",ValueString "-build_id")])]
      res <- Koji.listBuilds hub $
             -- centos stream koji 1.23 doesn't have pattern
             ("pattern", ValueString ("*" ++ disttag)) : opts
      case res of
        [] -> return Nothing
        [bld] -> return $ lookupStruct "nvr" bld
        _ -> error $ "more than one latest build found for " ++ pkg

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

installRPMs :: [FilePath] -> IO ()
installRPMs [] = return ()
installRPMs pkgs =
  sudo_ "dnf" ("install" : map ("./" ++) pkgs)
