module Common (
  knownHubs,
  hubURL,
  commonQueryOptions,
  commonBuildQueryOptions,
  webUrl,
  getBuildState,
  lookupArch
  )
where

import Control.Applicative ((<|>))
import Data.List.Extra (dropSuffix, isPrefixOf)
import Distribution.Koji (fedoraKojiHub, Value(..), Struct, BuildState,
                          lookupStruct, readBuildState)
import SimpleCmd (error')

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

commonQueryOptions :: Int -> String -> [(String, Value)]
commonQueryOptions limit order =
  [("limit",ValueInt limit),
   ("order",ValueString order)]

commonBuildQueryOptions :: Int -> (String, Value)
commonBuildQueryOptions limit =
  ("queryOpts", ValueStruct (commonQueryOptions limit "-build_id"))

webUrl :: String -> String
webUrl = dropSuffix "hub"

getBuildState :: Struct -> Maybe BuildState
getBuildState st = readBuildState <$> lookup "state" st

-- noarch build tasks usually have arch
-- see also https://pagure.io/koji/issue/4098
lookupArch :: Struct -> Maybe String
lookupArch st = lookupStruct "label" st <|> lookupStruct "arch" st
