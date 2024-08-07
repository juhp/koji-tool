{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

module BuildlogSizes (
  buildlogSizesCmd
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad.Extra

import Data.Char (isDigit)
import Data.RPM.NVR
import Data.List.Extra (sortOn, splitOn)
--import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Distribution.Koji (fedoraKojiHub)
import Distribution.Koji.API --(listBuilds)

import Formatting

#if !MIN_VERSION_http_directory(0,1,5)
import Network.HTTP.Client (Manager)
#endif
import Network.HTTP.Directory

import SimpleCmdArgs

import Common (commonBuildQueryOptions, getBuildState, lookupArch)
import Utils (buildlogUrlfromTaskId)

-- FIXME split off arch suffix
-- FIXME show build duration
-- FIXME allow buildid
buildlogSizesCmd :: String -> IO ()
buildlogSizesCmd nvrpat = do
  if all isDigit nvrpat -- taskid
    then buildlogSizes (read nvrpat)
    else do -- find builds
    let pat =
          if '*' `notElem` nvrpat && length (splitOn "-" nvrpat) < 3
          then nvrpat ++ "*"
          else nvrpat
    results <- listBuilds fedoraKojiHub
               [("pattern", ValueString pat),
                commonBuildQueryOptions (Just 5)]
    if null results
      then if '*' `notElem` pat
           then buildlogSizesCmd $ nvrpat ++ "*"
           else putStrLn $ "no NVRs found for pattern: " ++ pat
      else mapM_ getResult results
  where
    getResult :: Struct -> IO ()
    getResult bld = do
      putStrLn ""
      case lookupStruct "nvr" bld of
        Just nvr -> do
          putStrLn $ nvr ++
            maybe "" (\s -> " (" ++ show s ++ ")") (getBuildState bld)
          ok <- nvrBuildlogSizes nvr
          unless ok $ taskResult bld
        Nothing -> taskResult bld

    taskResult :: Struct -> IO ()
    taskResult bld = do
      let mtid =
            lookupStruct "task_id" bld <|>
            (lookupStruct "extra" bld >>= lookupStruct "task_id")
      case mtid :: Maybe Int of
        Nothing -> error "no taskid found!"
        Just tid -> buildlogSizes tid

nvrBuildlogSizes :: String -> IO Bool
nvrBuildlogSizes nvr = do
  let (NVR n (VerRel v r)) = readNVR nvr
      logsdir = "https://kojipkgs.fedoraproject.org/packages" +/+ n  +/+ v +/+ r +/+ "data/logs/"
  exists <- httpExists' logsdir
  if exists
    then do
    archs <- map (T.unpack . noTrailingSlash) <$> httpDirectory' logsdir
    forM_ archs $ \arch ->
      doGetBuildlogSize (logsdir +/+ arch +/+ "build.log") arch
    return True
    else
    return False

buildlogSizes :: Int -> IO ()
buildlogSizes tid = do
  children <- sortOn lookupArch <$>
              getTaskChildren fedoraKojiHub tid True
  mapM_ buildlogSize children

buildlogSize :: Struct -> IO ()
buildlogSize child = do
  case lookupStruct "id" child :: Maybe Int of
    Nothing -> error "child taskid not found"
    Just tid ->
      whenJust (lookupStruct "arch" child) $
        doGetBuildlogSize buildlog
      where
        buildlog = buildlogUrlfromTaskId tid

doGetBuildlogSize :: String -> String -> IO ()
doGetBuildlogSize buildlog arch = do
  exists <- httpExists' buildlog
  msize <- if exists then httpFileSize' buildlog else return Nothing
  whenJust msize $ \ size ->
    fprintLn (right 8 ' ' % lpadded 7 ' ' commas % "kB") arch (size `div` 1000)

#if !MIN_VERSION_http_directory(0,1,9)
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
#endif
