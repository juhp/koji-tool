module Utils (
  kojiTaskRequestNVR,
  kojiTaskRequestPkgNVR,
  showValue
  )
where

import Data.RPM (dropArch)
import Data.RPM.NVR
import Data.RPM.NVRA
import Distribution.Koji
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
            Just nvra -> Right $ dropArch nvra
            Nothing -> Left $ takeBaseName src
    _ -> error' "could determine package from build request"

showValue :: Value -> String
showValue (ValueString cs) = cs
showValue (ValueInt i) = show i
showValue val = show val
