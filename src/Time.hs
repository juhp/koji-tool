{-# LANGUAGE CPP #-}

module Time (
  compactZonedTime,
  lookupTime,
  lookupTimes,
  formatLocalTime,
  renderDuration)
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

lookupTime :: Bool -> Struct -> Maybe UTCTime
lookupTime completion str = do
  case lookupStruct (prefix ++ "_ts") str of
    Just ts -> return $ readTime' ts
    Nothing ->
      lookupStruct (prefix ++ "_time") str >>=
      parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%EZ"
  where
    prefix = if completion then "completion" else "start"

lookupTimes :: Struct -> Maybe (UTCTime, Maybe UTCTime)
lookupTimes str = do
  start <- lookupTime False str
  let mend = lookupTime True str
  return (start,mend)

formatLocalTime :: Bool -> TimeZone -> UTCTime -> String
formatLocalTime start tz t =
  formatTime defaultTimeLocale (if start then "Start: %c" else "End:   %c") $
  utcToZonedTime tz t

renderDuration :: Bool -> NominalDiffTime -> String
#if MIN_VERSION_time(1,9,0)
renderDuration short dur =
  let fmtstr
        | dur < 60 = "%s" ++ secs
        | dur < 3600 = "%m" ++ mins ++ " %S" ++ secs
        | otherwise = "%h" ++ hrs ++ " %M" ++ mins
  in formatTime defaultTimeLocale fmtstr dur
  where
    secs = if short then "s" else " sec"
    mins = if short then "m" else " min"
    hrs = if short then "h" else " hours"
#else
renderDuration short dur =
  show dur ++ if short then "" else "ec"
#endif
