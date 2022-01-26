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
    "Query and track Koji tasks, and install rpms from Koji."
    "see https://github.com/juhp/koji-tool#readme" $
    subcommands
    [ Subcommand "install"
      "Install rpm packages directly from a Koji build task" $
      installCmd
      <$> switchWith 'n' "dry-run" "Don't actually download anything"
      <*> switchWith 'D' "debug" "More detailed output"
      <*> optional (strOptionWith 'H' "hub" "HUB"
                    ("KojiHub shortname or url (HUB = " ++
                     intercalate ", " knownHubs ++
                     ") [default: fedora]"))
      <*> optional (strOptionWith 'P' "packages-url" "URL"
                    "KojiFiles packages url [default: Fedora]")
      <*> switchWith 'l' "list" "List builds"
      <*> switchWith 'L' "latest" "Latest build"
      <*> modeOpt
      <*> disttagOpt sysdisttag
      <*> (flagWith' ReqNVR 'R' "nvr" "Give an N-V-R instead of package name"
           <|> flagWith ReqName ReqNV 'V' "nv" "Give an N-V instead of package name")
      <*> some (strArg "PKG|NVR|TASKID...")

    , Subcommand "query"
      "Query Koji tasks (by default lists your tasks today)" $
      queryCmd
      <$> strOptionalWith 'S' "server" "URL" "Koji Hub [default: Fedora]" fedoraKojiHub
      <*> optional (strOptionWith 'u' "user" "USER" "Koji user [default: fasid]")
      <*> (flagWith' 1 'L' "latest" "Latest build or task" <|>
           optionalWith auto 'l' "limit" "INT" "Maximum number of tasks to show [default: 10]" 10)
      <*> (Task <$> optionWith auto 't' "task" "TASKID" "Show task"
           <|> Parent <$> optionWith auto 'c' "children" "TASKID" "List child tasks of parent"
           <|> Build <$> strOptionWith 'b' "build" "BUILD" "List child tasks of build"
           <|> Package <$> strOptionWith 'p' "package" "PKG" "Build tasks of package"
           <|> pure TaskQuery)
      <*> many (parseTaskState <$> strOptionWith 's' "state" "STATE" "Filter tasks by state (open, close(d), cancel(ed), fail(ed), assigned, free)")
      <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
      <*> optional (Before <$> strOptionWith 'B' "before" "TIMESTAMP" "Tasks completed before timedate [default: now]" <|>
                    After <$> strOptionWith 'F' "from" "TIMESTAMP" "Tasks completed after timedate")
      <*> (normalizeMethod <$> optional (strOptionWith 'm' "method" "METHOD" "Select tasks by method: [build,buildarch,etc] or 'any' (default 'buildArch')"))
      <*> switchWith 'D' "debug" "Pretty-print raw XML result"
      -- FIXME error if integer (eg mistakenly taskid)
      <*> optional (TaskPackage <$> strOptionWith 'P' "only-package" "PKG" "Filter task results to specified package"
                   <|> TaskNVR <$> strOptionWith 'N' "only-nvr" "PREFIX" "Filter task results by NVR prefix")
      <*> switchWith 'T' "tail" "Fetch the tail of build.log"

    , Subcommand "progress"
      "Track running Koji tasks by buildlog size" $
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

    normalizeMethod :: Maybe String -> Maybe String
    normalizeMethod Nothing = normalizeMethod (Just "buildarch")
    normalizeMethod (Just "any") = Nothing
    normalizeMethod (Just m) =
      case elemIndex (lower m) (map lower kojiMethods) of
        Just i -> Just $ kojiMethods !! i
        Nothing -> error' $! "unknown method: " ++ m
