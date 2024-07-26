{-# LANGUAGE CPP, OverloadedStrings #-}

module Time (
  compactZonedTime,
  TimeEvent(..),
  lookupTime,
  lookupStartEndTimes,
  lookupStartEndTimes',
  lookupTaskTimes,
  durationOfTask,
  formatLocalTime,
  renderDuration,
  UTCTime)
where

import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji.API (Struct, lookupStruct)
import Formatting

readTime' :: Double -> UTCTime
readTime' =
  let mkSystemTime t = MkSystemTime t 0
  in systemToUTCTime . mkSystemTime . truncate

compactZonedTime :: TimeZone -> UTCTime -> String
compactZonedTime tz =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" . utcToZonedTime tz

data TimeEvent = CreateEvent | StartEvent | CompletionEvent

showEvent :: TimeEvent -> String
showEvent CreateEvent = "create"
showEvent StartEvent = "start"
showEvent CompletionEvent = "completion"

lookupTime :: TimeEvent -> Struct -> Maybe UTCTime
lookupTime event str = do
  let ev = showEvent event
    in
    case lookupStruct (ev ++ "_ts") str of
      Just ts -> return $ readTime' ts
      Nothing ->
        lookupStruct (ev ++ "_time") str >>=
        parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%EZ"

lookupTime' :: TimeEvent -> Struct -> UTCTime
lookupTime' event str =
  case lookupTime event str of
    Nothing -> error $ "no " ++ showEvent event -- for build/task
    Just t -> t

lookupTaskTimes :: Struct -> (UTCTime, Maybe UTCTime, Maybe UTCTime)
lookupTaskTimes str =
  let create = lookupTime' CreateEvent str
      mstart = lookupTime StartEvent str
      mend = lookupTime CompletionEvent str
  in (create,mstart,mend)

lookupStartEndTimes :: Struct -> Maybe (UTCTime, Maybe UTCTime)
lookupStartEndTimes str = do
  start <- lookupTime StartEvent str
  let mend = lookupTime CompletionEvent str
  return (start,mend)

lookupStartEndTimes' :: Struct -> (UTCTime, UTCTime)
lookupStartEndTimes' st =
  (lookupTime' StartEvent st, lookupTime' CompletionEvent st)

durationOfTask :: Struct -> Maybe NominalDiffTime
durationOfTask str = do
  let (create,_mstart,mend) = lookupTaskTimes str
  end <- mend
  return $ diffUTCTime end create

formatLocalTime :: TimeEvent -> TimeZone -> UTCTime -> String
formatLocalTime event tz t =
  -- FIXME format time with formatting
  formatTime defaultTimeLocale
  (formatToString (rpadded 12 ' ' string % "%c") (showEvent event ++ ":")) $
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
  show dur ++ if short then "" else "sec"
#endif
