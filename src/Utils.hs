module Utils (
  kojiTaskRequestNVR,
  kojiGetBuildID',
  kojiGetBuild',
  showValue,
  buildOutputURL,
  hubToPkgsURL
  )
where

import Data.List.Extra (dropSuffix, isInfixOf, isPrefixOf, isSuffixOf, replace,
                        takeWhileEnd)
import Data.RPM (dropArch)
import Data.RPM.NVR
import Data.RPM.NVRA
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import Network.HTTP.Directory ((+/+))
import SimpleCmd (error')
import System.FilePath (takeBaseName)

-- FIXME this should really be a triple
-- FIXME should not use on build
kojiTaskRequestNVR :: Struct -> Either String NVR
kojiTaskRequestNVR task =
  case lookupStruct "request" task of
    Just req@(source:_) ->
      case getString source of
        -- non-build task
        Nothing -> Left $ unwords $ map showValue $ take 2 req
        Just src ->
          case maybeNVRA src of
            Just nvra | not (".git#" `isInfixOf` src) ->
                        Right $ dropArch nvra
            _ -> let base = takeBaseName src in
                   Left $
                   -- fedora-ci_fc87dcd7db694cdfac703cf5af128a72_c1f53c859616fdbf40811058b60bebe2930e3271_0;forks:petersen:rpms:grub2.f38.src.rpm
                   if "fedora-ci_" `isPrefixOf` base
                   then takeWhileEnd (/= ':') $ tail $ dropWhile (/= ';') base
                   else base
    _ -> error' "could determine package from build request"

kojiGetBuildID' :: String -> String -> IO BuildID
kojiGetBuildID' hub nvr = do
  mbid <- kojiGetBuildID hub nvr
  case mbid of
    Nothing -> error' $ "build id not found for " ++ nvr
    Just bid -> return bid

kojiGetBuild' :: String -> NVR -> IO Koji.Struct
kojiGetBuild' hub nvr = do
  mbld <- Koji.getBuild hub (Koji.InfoString (showNVR nvr))
  case mbld of
    Nothing -> error' $ "build not found for " ++ showNVR nvr
    Just bld -> return bld

showValue :: Value -> String
showValue (ValueString cs) = cs
showValue (ValueInt i) = show i
showValue val = show val

buildOutputURL :: String -> NVR -> String
buildOutputURL hub nvr =
  let name = nvrName nvr
      verrel = nvrVerRel nvr
      ver = vrVersion verrel
      rel = vrRelease verrel
  in hubToPkgsURL hub +/+ name +/+ ver +/+ rel

hubToPkgsURL :: String -> String
hubToPkgsURL url =
  case dropSuffix "/" url of
    "https://koji.fedoraproject.org/kojihub" ->
      "https://kojipkgs.fedoraproject.org/packages"
    "https://kojihub.stream.centos.org/kojihub" ->
      "https://kojihub.stream.centos.org/kojifiles/packages"
    _ ->
      if "kojihub" `isSuffixOf` url
      then replace "kojihub" "kojifiles" url +/+ "packages"
      else error' $ "use --files-url to specify kojifiles url for " ++ url
