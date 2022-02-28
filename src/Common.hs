module Common (
  knownHubs,
  hubURL,
  readTime'
  )
where

import Data.List (isPrefixOf)
import Data.Time.Clock
import Data.Time.Clock.System
import Distribution.Koji (fedoraKojiHub)
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

readTime' :: Double -> UTCTime
readTime' =
  let mkSystemTime t = MkSystemTime t 0
  in systemToUTCTime . mkSystemTime .truncate
