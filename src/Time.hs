module Time (
  compactZonedTime,
  lookupTime)
where

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji.API (Struct, lookupStruct)

readTime' :: Double -> UTCTime
readTime' =
  let mkSystemTime t = MkSystemTime t 0
  in systemToUTCTime . mkSystemTime . truncate

compactZonedTime :: TimeZone -> UTCTime -> String
compactZonedTime tz =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Z" . utcToZonedTime tz

lookupTime :: String -> Struct -> Maybe UTCTime
lookupTime prefix str = do
  case lookupStruct (prefix ++ "_ts") str of
    Just ts -> return $ readTime' ts
    Nothing ->
      lookupStruct (prefix ++ "_time") str >>=
      parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%EZ"
