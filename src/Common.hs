module Common (
  knownHubs,
  hubURL,
  commonQueryOptions,
  commonBuildQueryOptions,
  webUrl
  )
where

import Data.List.Extra (dropSuffix, isPrefixOf)
import Distribution.Koji (fedoraKojiHub, Value(..))
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
