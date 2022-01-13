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

#if !MIN_VERSION_http_directory(0,1,5)
import Network.HTTP.Client (Manager)
#endif
import Network.HTTP.Directory

import Data.Text.Format.Numbers

import SimpleCmdArgs

-- FIXME split off arch suffix
buildlogSizesCmd :: String -> IO ()
buildlogSizesCmd nvrpat = do
  if all isDigit nvrpat -- check if taskid (not buildid)
    then do
    mgr <- httpManager
    buildlogSizes mgr (read nvrpat)
    else do -- find builds
    results <- listBuilds fedoraKojiHub
               [("pattern", ValueString nvrpat),
                ("queryOpts",ValueStruct [("limit",ValueInt 5),
                                          ("order",ValueString "-build_id")])]
    mgr <- httpManager
    mapM_ (getResult mgr) results
  where
    getResult :: Manager -> Struct -> IO ()
    getResult mgr bld = do
      putStrLn ""
      case lookupStruct "nvr" bld of
        Just nvr -> do
          putStrLn nvr
          nvrBuildlogSizes mgr nvr
        Nothing -> do
          let mextra = lookupStruct "extra" bld
              mtid =
                lookupStruct "task_id" bld <|>
                (mextra >>= lookupStruct "task_id")
          case mtid :: Maybe Int of
            Nothing -> error "no taskid found!"
            Just tid -> buildlogSizes mgr tid

nvrBuildlogSizes :: Manager -> String -> IO ()
nvrBuildlogSizes mgr bld = do
  let (NVR n (VerRel v r)) = readNVR bld
      logsdir = "https://kojipkgs.fedoraproject.org/packages" +/+ n  +/+ v +/+ r +/+ "data/logs/"
  archs <- map (T.unpack . noTrailingSlash) <$> httpDirectory mgr logsdir
  forM_ archs $ \arch ->
    doGetBuildlogSize mgr (logsdir +/+ arch +/+ "build.log") arch

buildlogSizes :: Manager -> Int -> IO ()
buildlogSizes mgr tid = do
  children <- sortOn (\t -> lookupStruct "arch" t :: Maybe String) <$>
              getTaskChildren fedoraKojiHub tid True
  mapM_ (buildlogSize mgr) children

buildlogSize :: Manager -> Struct -> IO ()
buildlogSize mgr child = do
  case lookupStruct "id" child :: Maybe Int of
    Nothing -> error "child taskid not found"
    Just tid -> do
      whenJust (lookupStruct "arch" child) $
        doGetBuildlogSize mgr buildlog
      where
        buildlog = "https://kojipkgs.fedoraproject.org/work/tasks" +/+ lastFew +/+ show tid +/+ "build.log"
        lastFew =
          let few = dropWhile (== '0') $ drop 4 (show tid) in
            if null few then "0" else few

doGetBuildlogSize :: Manager -> String -> String -> IO ()
doGetBuildlogSize mgr buildlog arch = do
  exists <- httpExists mgr buildlog
  msize <- if exists then httpFileSize mgr buildlog else return Nothing
  whenJust msize $ \ size -> do
    let kb = kiloBytes size
    putStrLn $ arch ++ replicate (16 - length arch - length kb) ' ' ++ kb

kiloBytes :: Integer -> String
kiloBytes size =
  T.unpack $ prettyI (Just ',') (fromInteger size `div` 1000) <> "kB"

#if !MIN_VERSION_http_directory(0,1,9)
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = init s +/+ t
        | head t == '/' = s +/+ tail t
s +/+ t = s ++ "/" ++ t
#endif