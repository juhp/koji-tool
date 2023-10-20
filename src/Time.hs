{-# LANGUAGE CPP, OverloadedStrings #-}

module Time (
  compactZonedTime,
  TimeEvent(..),
  lookupTime,
  lookupBuildTimes,
  lookupTaskTimes,
  strictLookupTimes,
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

lookupTaskTimes :: Struct -> Maybe (UTCTime, Maybe UTCTime)
lookupTaskTimes str = do
  start <- lookupTime CreateEvent str
  let mend = lookupTime CompletionEvent str
  return (start,mend)

lookupBuildTimes :: Struct -> Maybe (UTCTime, Maybe UTCTime)
lookupBuildTimes str = do
  start <- lookupTime StartEvent str
  let mend = lookupTime CompletionEvent str
  return (start,mend)

strictLookupTimes :: (Struct -> Maybe (UTCTime, Maybe UTCTime))
                  -> Struct -> (UTCTime, UTCTime)
strictLookupTimes lf st =
  case lf st of
    Nothing -> error "no start time" -- for build/task
    Just (start,mend) ->
      case mend of
        Nothing -> error "no end time" -- for build/task
        Just end -> (start,end)

durationOfTask :: Struct -> Maybe NominalDiffTime
durationOfTask str = do
  (start,mend) <- lookupTaskTimes str
  end <- mend
  return $ diffUTCTime end start

formatLocalTime :: Bool -> TimeZone -> UTCTime -> String
formatLocalTime start tz t =
  -- FIXME format time with formatting
  formatTime defaultTimeLocale
  (formatToString (rpadded 11 ' ' string % "%c") (if start then "Created:" else "Completed:")) $
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
