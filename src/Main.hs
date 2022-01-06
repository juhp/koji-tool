{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.List.Extra
import SimpleCmd
import SimpleCmdArgs

import BuildlogSizes
import Install
import qualified Paths_koji_tool
import Progress
import Query

main :: IO ()
main = do
  sysdisttag <- do
    dist <- cmd "rpm" ["--eval", "%{dist}"]
    return $ if dist == "%{dist}" then "" else dist
  simpleCmdArgs (Just Paths_koji_tool.version)
    "Query Koji tasks and install rpms from Koji."
    ("HUB = " ++ intercalate ", " knownHubs) $
    subcommands
    [ Subcommand "install"
      "Install rpm packages directly from a Koji  build task" $
      installCmd
      <$> switchWith 'n' "dry-run" "Don't actually download anything"
      <*> switchWith 'D' "debug" "More detailed output"
      <*> optional (strOptionWith 'H' "hub" "HUB"
                    "KojiHub shortname or url [default: fedora]")
      <*> optional (strOptionWith 'P' "packages-url" "URL"
                    "KojiFiles packages url [default: Fedora]")
      <*> switchWith 'l' "list" "List builds"
      <*> modeOpt
      <*> disttagOpt sysdisttag
      <*> (flagWith' ReqNVR 'R' "nvr" "Give an N-V-R instead of package name"
           <|> flagWith ReqName ReqNVR 'V' "nv" "Give an N-V instead of package name")
      <*> some (strArg "PKG|NVR|TASKID...")

    , Subcommand "query" "Query Koji tasks" $
      queryCmd
      <$> strOptionalWith 'S' "server" "URL" "Koji Hub [default: Fedora]" fedoraKojiHub
      <*> optional (strOptionWith 'u' "user" "USER" "Koji user [default: fasid]")
      <*> optionalWith auto 'l' "limit" "INT" "Maximum number of tasks to show [default: 10]" 10
      <*> (Task <$> optionWith auto 't' "task" "TASKID" "Show task"
           <|> Parent <$> optionWith auto 'c' "children" "TASKID" "List child tasks of parent"
           <|> Build <$> strOptionWith 'b' "build" "BUILD" "List child tasks of build"
           <|> pure TaskQuery)
      <*> many (parseTaskState <$> strOptionWith 's' "state" "STATE" "Filter tasks by state")
      <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
      <*> optional (strOptionWith 'd' "date" "DURATION" "Tasks started after date [default: today]")
      <*> (fmap normalizeMethod <$> optional (strOptionWith 'm' "method" "METHOD" "Select tasks by method: [build,buildarch,etc]"))
      <*> switchWith 'D' "debug" "Pretty-pretty raw XML result"
      -- FIXME error if integer (eg mistakenly taskid)
      <*> optional (TaskPackage <$> strOptionWith 'p' "package" "PKG" "Filter results to specified package"
                   <|> TaskNVR <$> strOptionWith 'n' "nvr" "PREFIX" "Filter results by NVR prefix")

    , Subcommand "progress" "Track running Koji tasks by buildlog size" $
      progressCmd
      <$> optionalWith auto 'i' "interval" "MINUTES" "Polling interval between updates (default 2 min)" 2
      <*> switchWith 'm' "modules" "Track module builds"
      <*> many (TaskId <$> argumentWith auto "TASKID")

    , Subcommand "buildlog-sizes" "Show buildlog sizes for nvr patterns" $
      buildlogSizesCmd <$> strArg "NVRPATTERN"
    ]
  where
    modeOpt :: Parser Mode
    modeOpt =
      flagWith' All 'a' "all" "all subpackages" <|>
      flagWith' Ask 'A' "ask" "ask for each subpackge [default if not installed]" <|>
      pkgsReqOpts

    pkgsReqOpts = PkgsReq
      <$> many (strOptionWith 'p' "package" "SUBPKG" "Subpackage (glob) to install") <*> many (strOptionWith 'x' "exclude" "SUBPKG" "Subpackage (glob) not to install")

    disttagOpt :: String -> Parser String
    disttagOpt disttag = startingDot <$> strOptionalWith 'd' "disttag" "DISTTAG" ("Use a different disttag [default: " ++ disttag ++ "]") disttag

    startingDot cs =
      case cs of
        "" -> error' "empty disttag"
        (c:_) -> if c == '.' then cs else '.' : cs

    normalizeMethod :: String -> String
    normalizeMethod m =
      case elemIndex (lower m) (map lower kojiMethods) of
        Just i -> kojiMethods !! i
        Nothing -> error' $! "unknown method: " ++ m
