{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Char (isDigit)
import Data.List.Extra
import SelectRPMs (existingStrategyOption, pkgMgrOpt, selectRpmsOptions)
import SimpleCmd
import SimpleCmdArgs

import Builds
import BuildlogSizes
import Common (Limit(..), Natural, defaultBuildsLimit, defaultTasksLimit)
import User
import Install
import qualified Paths_koji_tool
import Progress
import Find
import Tasks

main :: IO ()
main =
  simpleCmdArgs (Just Paths_koji_tool.version)
    "Query and track Koji tasks, and install rpms from Koji."
    "see https://github.com/juhp/koji-tool#readme" $
    subcommands
    [ Subcommand "builds"
      "Query Koji builds (by default lists the most recent builds)" $
      buildsCmd
      <$> hubOpt
      <*> optional (userOpt False)
      <*> optional (limitOpt "build" defaultBuildsLimit)
      <*> many (parseBuildState' <$> strOptionWith 's' "state" "STATE" "Filter builds by state (building,complete,deleted,fail(ed),cancel(ed)")
      <*> optional (Before <$> strOptionWith 'B' "before" "TIMESTAMP" "Builds completed before timedate [default: now]" <|>
                    After <$> strOptionWith 'F' "from" "TIMESTAMP" "Builds completed after timedate")
      <*> (fmap normalizeBuildType <$> optional (strOptionLongWith "type" "TYPE" ("Select builds by type: " ++ intercalate "," kojiBuildTypes)))
      <*> optional
      (flagWith' Builds.Detailed 'd' "details" "Show more build details" <|>
       flagWith' Builds.DetailedTasks 't' "tasks" "Show details and tasks")
      <*> tailOpt
      <*> optional (installArgs <$> strOptionWith 'i' "install" "INSTALLOPTS" "Install the package with 'install' options")
      <*> switchWith 'D' "debug" "Pretty-print raw XML result"
      <*> (BuildBuild <$> strOptionWith 'b' "build" "NVR/BUILDID" "Show build" <|>
           BuildPattern <$> strOptionWith 'p' "pattern" "NVRPAT" "Builds matching glob pattern" <|>
           BuildPackage <$> strArg "PACKAGE" <|>
           pure BuildQuery)

    , Subcommand "tasks"
      "Query Koji tasks (by default lists the most recent buildArch tasks)" $
      tasksCmd
      <$> hubOpt
      <*> queryOpts False "buildArch"
      <*> optional
      (flagWith' Tasks.Detailed 'd' "details" "Show more task details" <|>
       flagWith' Tasks.Concise 'c' "concise" "Compact task output")
      <*> tailOpt
      <*> (flagLongWith' HWInfo "hw-info" "Fetch hw_info.log" <|>
           flagLongWith BuildLog RootLog "rootlog" "Fetch root.log")
      <*> optional (strOptionWith 'g' "grep" "STRING" "Filter matching log lines")
      <*> optional (installArgs <$> strOptionWith 'i' "install" "INSTALLOPTS" "Install the package with 'install' options")
      <*> taskReqOpt

    , Subcommand "latest"
      "Query latest Koji build for tag" $
      latestCmd
      <$> hubOpt
      <*> switchWith 'D' "debug" "Pretty-print raw XML result"
      <*> strArg "TAG"
      <*> strArg "PKG"

    , Subcommand "install"
      "Install rpm packages directly from a Koji build task" $
      installCmd
      <$> switchWith 'n' "dry-run" "Don't actually download anything"
      <*> switchWith 'D' "debug" "More detailed output"
      -- FIXME add --no
      <*> flagWith No Yes 'y' "yes" "Assume yes to questions"
      <*> hubOpt
      <*> optional (strOptionWith 'P' "packages-url" "URL"
                    "KojiFiles packages url [default: Fedora]")
      <*> switchWith 'l' "list" "List RPMs"
      <*> switchWith 'L' "latest" "Latest build"
      <*> switchWith 't' "check-remote-time" "Check remote rpm timestamps"
      <*> optional pkgMgrOpt
      <*> switchLongWith "allow-erasing" "Use dnf --allowerasing"
      <*> many archOpt
      <*> optional existingStrategyOption
      <*> optional (strOptionWith 'b' "prefix" "SUBPKGPREFIX" "Prefix to use for subpackages [default: base package]")
      <*> selectRpmsOptions
      <*> optional disttagOpt
      <*> (flagLongWith' (Left ()) "tagged" "Give a tag instead of package name" <|>
           flagWith' (Right ReqNVR) 'R' "nvr" "Give an N-V-R instead of package name" <|>
           flagWith (Right ReqName) (Right ReqNV) 'V' "nv" "Give an N-V instead of (default) package name")
      <*> some (strArg "PKG|NVR|TASKID|TAG...")

    , Subcommand "progress"
      "Track running Koji tasks by buildlog size" $
      progressCmd
      <$> switchLongWith "modules" "Track module builds"
      <*> queryOpts True "build"
      <*> taskReqOpt

    , Subcommand "buildlog-sizes" "Show buildlog sizes for nvr patterns" $
      buildlogSizesCmd
      <$> optional (limitOpt "build" defaultBuildsLimit)
      <*> strArg "NVRPATTERN|PKG|TASKID"

    , Subcommand "find"
      ("Simple quick common queries using words like: [" ++
       intercalate ", " wordsListHeads ++ "]") $
      findCmd
      <$> hubOpt
      <*> many (strArg "PHRASE")

    , Subcommand "tagged"
      "List builds for tag" $
      taggedCmd
      <$> hubOpt
      <*> switchWith 'i' "install" "Install packages"
      <*> strArg "TAG"
    ]
  where
    hubOpt = optional (strOptionWith 'H' "hub" "HUB"
                       ("KojiHub shortname or url (HUB = " ++
                        intercalate ", " knownHubs ++
                        ") [default: fedora]"))

    userOpt :: Bool -> Parser UserOpt
    userOpt mine =
      User <$> strOptionWith 'u' "user" "USER" "Koji user"
      <|> if mine then pure UserSelf else flagWith' UserSelf 'M' "mine" "Your tasks (krb fasid)"

    disttagOpt :: Parser String
    disttagOpt = startingDot <$>
                 strOptionWith 'd' "disttag" "DISTTAG"
                 "Select a disttag different to system"

    startingDot cs =
      case cs of
        "" -> error' "empty disttag"
        (c:_) -> if c == '.' then cs else '.' : cs

    normalizeMethod :: String -> String
    normalizeMethod m =
      case elemIndex (lower m) (map lower kojiMethods) of
        Just i -> kojiMethods !! i
        Nothing -> error' $! "unknown method: " ++ m

    normalizeBuildType :: String -> String
    normalizeBuildType m =
      case elemIndex (lower m) (map lower kojiBuildTypes) of
        Just i -> kojiBuildTypes !! i
        Nothing -> error' $! "unknown build type: " ++ m

    readTaskReq :: String -> Maybe TaskReq
    readTaskReq cs =
      Just $ if all isDigit cs then Task (read cs) else Package cs

    -- FIXME check valid arch (eg i686 not i386)
    archOpt = strOptionWith 'a' "arch" "ARCH" "Task arch"

    queryOpts :: Bool -> String -> Parser QueryOpts
    queryOpts mine defaultMethod =
      QueryOpts
      <$> optional (userOpt mine)
      <*> optional (limitOpt "build/task" defaultTasksLimit)
      <*> many (fmap parseTaskState' $! strOptionWith 's' "state" "STATE" "Filter tasks by state (open,close(d),cancel(ed),fail(ed),assigned,free)")
      <*> many archOpt
      <*> optional (Before <$> strOptionWith 'B' "before" "TIMESTAMP" "Tasks completed before timedate [default: now]" <|>
                    After <$> strOptionWith 'F' "from" "TIMESTAMP" "Tasks completed after timedate")
      <*> (fmap normalizeMethod <$> optional (strOptionWith 'm' "method" "METHOD" ("Select tasks by method (default '" ++ defaultMethod ++ "'): " ++ intercalate "," kojiMethods)))
      <*> switchWith 'D' "debug" "Pretty-print raw XML result"
      <*> optional (TaskPackage <$> strOptionWith 'P' "only-package" "PKG" "Filter task results to specified package"
                   <|> TaskNVR <$> strOptionWith 'N' "only-nvr" "PREFIX" "Filter task results by NVR prefix")

    -- FIXME error if integer (eg mistakenly taskid) instead of string
    taskReqOpt =
      Build <$> strOptionWith 'b' "build" "BUILD" "List child tasks of build"
      <|> Pattern <$> strOptionWith 'p' "pattern" "NVRPAT" "Build tasks of matching pattern"
      <|> ChildrenOf <$> optionLongWith auto "children" "TASKID" "Children tasks of parent"
      <|> ParentOf <$> optionLongWith auto "parent" "TASKID" "Parent of task"
      <|> argumentWith (maybeReader readTaskReq) "PACKAGE|TASKID"
      <|> pure TaskQuery

    limitOpt :: String -> Natural -> Parser Limit
    limitOpt tgt dflt =
      flagWith' (Limit 1) 'L' "latest" ("Latest" +-+ tgt) <|>
      flagWith' Nolimit 'U' "unlimited" "No limit on number of results" <|>
      Limit <$> optionWith auto 'l' "limit" "INT" ("Maximum number of" +-+ tgt ++ "s to show [default:" +-+ show dflt ++ "]")

    tailOpt = switchWith 'T' "tail" "Fetch the tail of build.log"
