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
import Data.List (sortOn)
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

import Common (commonBuildQueryOptions)

-- FIXME split off arch suffix
buildlogSizesCmd :: String -> IO ()
buildlogSizesCmd nvrpat = do
  if all isDigit nvrpat -- check if taskid (not buildid)
    then do
    buildlogSizes (read nvrpat)
    else do -- find builds
    results <- listBuilds fedoraKojiHub
               [("pattern", ValueString nvrpat),
                commonBuildQueryOptions 5]
    mapM_ getResult results
  where
    getResult :: Struct -> IO ()
    getResult bld = do
      putStrLn ""
      case lookupStruct "nvr" bld of
        Just nvr -> do
          putStrLn nvr
          nvrBuildlogSizes nvr
        Nothing -> do
          let mextra = lookupStruct "extra" bld
              mtid =
                lookupStruct "task_id" bld <|>
                (mextra >>= lookupStruct "task_id")
          case mtid :: Maybe Int of
            Nothing -> error "no taskid found!"
            Just tid -> buildlogSizes tid

nvrBuildlogSizes :: String -> IO ()
nvrBuildlogSizes bld = do
  let (NVR n (VerRel v r)) = readNVR bld
      logsdir = "https://kojipkgs.fedoraproject.org/packages" +/+ n  +/+ v +/+ r +/+ "data/logs/"
  archs <- map (T.unpack . noTrailingSlash) <$> httpDirectory' logsdir
  forM_ archs $ \arch ->
    doGetBuildlogSize (logsdir +/+ arch +/+ "build.log") arch

buildlogSizes :: Int -> IO ()
buildlogSizes tid = do
  children <- sortOn (\t -> lookupStruct "arch" t :: Maybe String) <$>
              getTaskChildren fedoraKojiHub tid True
  mapM_ buildlogSize children

buildlogSize :: Struct -> IO ()
buildlogSize child = do
  case lookupStruct "id" child :: Maybe Int of
    Nothing -> error "child taskid not found"
    Just tid -> do
      whenJust (lookupStruct "arch" child) $
        doGetBuildlogSize buildlog
      where
        buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" +/+ lastFew +/+ show tid +/+ "build.log"
        lastFew =
          let few = dropWhile (== '0') $ drop 4 (show tid) in
            if null few then "0" else few

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
