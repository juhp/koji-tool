module Common (
  knownHubs,
  hubURL,
  readTime',
  compactZonedTime,
  commonQueryOptions,
  commonBuildQueryOptions
  )
where

import Data.List (isPrefixOf)
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Format
import Data.Time.LocalTime
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

readTime' :: Double -> UTCTime
readTime' =
  let mkSystemTime t = MkSystemTime t 0
  in systemToUTCTime . mkSystemTime .truncate

compactZonedTime :: TimeZone -> UTCTime -> String
compactZonedTime tz =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Z" . utcToZonedTime tz

commonQueryOptions :: Int -> String -> [(String, Value)]
commonQueryOptions limit order =
  [("limit",ValueInt limit),
   ("order",ValueString order)]

commonBuildQueryOptions :: Int -> (String, Value)
commonBuildQueryOptions limit =
  ("queryOpts", ValueStruct (commonQueryOptions limit "-build_id"))
