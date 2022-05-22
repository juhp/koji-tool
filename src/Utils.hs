module Utils (
  kojiTaskRequestNVR,
  kojiTaskRequestPkgNVR,
  showValue,
  buildOutputURL
  )
where

import Data.List (isInfixOf, isPrefixOf)
import Data.RPM (dropArch)
import Data.RPM.NVR
import Data.RPM.NVRA
import Distribution.Koji
import Network.HTTP.Directory ((+/+))
import SimpleCmd (error')
import System.FilePath (takeBaseName)

kojiTaskRequestNVR :: Struct -> Maybe NVR
kojiTaskRequestNVR  task =
  case lookupStruct "request" task of
    Just (srpm:_) ->
      getString srpm >>= fmap dropArch . maybeNVRA
    _ -> Nothing

-- FIXME this should really be a triple
kojiTaskRequestPkgNVR :: Struct -> Either String NVR
kojiTaskRequestPkgNVR task =
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
                   if "fedora-ci_" `isPrefixOf` base
                   then tail $ dropWhile (/= ';') base
                   else base
    _ -> error' "could determine package from build request"

showValue :: Value -> String
showValue (ValueString cs) = cs
showValue (ValueInt i) = show i
showValue val = show val

buildOutputURL :: NVR -> String
buildOutputURL nvr =
  let name = nvrName nvr
      verrel = nvrVerRel nvr
      ver = vrVersion verrel
      rel = vrRelease verrel
  in "https://kojipkgs.fedoraproject.org/packages" +/+ name +/+ ver +/+ rel
