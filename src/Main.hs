{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Char (isDigit)
import Data.List.Extra
import SimpleCmd
import SimpleCmdArgs

import Builds
import BuildlogSizes
import User
import Install
import qualified Paths_koji_tool
import Progress
import Find
import Tasks

main :: IO ()
main = do
  simpleCmdArgs (Just Paths_koji_tool.version)
    "Query and track Koji tasks, and install rpms from Koji."
    "see https://github.com/juhp/koji-tool#readme" $
    subcommands
    [ Subcommand "builds"
      "Query Koji builds (by default lists the most recent builds)" $
      buildsCmd
      <$> hubOpt
      <*> optional userOpt
      <*> (flagWith' 1 'L' "latest" "Latest build" <|>
           optionalWith auto 'l' "limit" "INT" "Maximum number of builds to show [default: 10]" 10)
      <*> many (parseBuildState <$> strOptionWith 's' "state" "STATE" "Filter builds by state (building,complete,deleted,fail(ed),cancel(ed)")
      <*> optional (Before <$> strOptionWith 'B' "before" "TIMESTAMP" "Builds completed before timedate [default: now]" <|>
                    After <$> strOptionWith 'F' "from" "TIMESTAMP" "Builds completed after timedate")
      <*> (fmap normalizeBuildType <$> optional (strOptionWith 'T' "type" "TYPE" ("Select builds by type: " ++ intercalate "," kojiBuildTypes)))
      <*> (flagWith' Detailed 'd' "details" "Show more build details" <|>
           flagWith DetailDefault DetailedTasks 't' "tasks" "Show details and tasks")
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
      <*> optional userOpt
      <*> (flagWith' 1 'L' "latest" "Latest build or task" <|>
           optionalWith auto 'l' "limit" "INT" "Maximum number of tasks to show [default: 10]" 10)
      <*> many (parseTaskState <$> strOptionWith 's' "state" "STATE" "Filter tasks by state (open,close(d),cancel(ed),fail(ed),assigned,free)")
      <*> many (strOptionWith 'a' "arch" "ARCH" "Task arch")
      <*> optional (Before <$> strOptionWith 'B' "before" "TIMESTAMP" "Tasks completed before timedate [default: now]" <|>
                    After <$> strOptionWith 'F' "from" "TIMESTAMP" "Tasks completed after timedate")
      <*> (fmap normalizeMethod <$> optional (strOptionWith 'm' "method" "METHOD" ("Select tasks by method (default 'buildArch'): " ++ intercalate "," kojiMethods)))
      <*> switchWith 'd' "details" "Show more details of builds"
      <*> switchWith 'D' "debug" "Pretty-print raw XML result"
      -- FIXME error if integer (eg mistakenly taskid)
      <*> optional (TaskPackage <$> strOptionWith 'P' "only-package" "PKG" "Filter task results to specified package"
                   <|> TaskNVR <$> strOptionWith 'N' "only-nvr" "PREFIX" "Filter task results by NVR prefix")
      <*> switchWith 'T' "tail" "Fetch the tail of build.log"
      -- FIXME any way to pass --help to install?
      <*> optional (installArgs <$> strOptionWith 'i' "install" "INSTALLOPTS" "Install the package with 'install' options")
      <*> (Build <$> strOptionWith 'b' "build" "BUILD" "List child tasks of build"
           <|> Pattern <$> strOptionWith 'p' "pattern" "NVRPAT" "Build tasks of matching pattern"
           <|> argumentWith (maybeReader readTaskReq) "PACKAGE|TASKID"
           <|> pure TaskQuery)

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
      <*> flagWith No Yes 'y' "yes" "Assume yes to questions (implies --all if not installed)"
      <*> hubOpt
      <*> optional (strOptionWith 'P' "packages-url" "URL"
                    "KojiFiles packages url [default: Fedora]")
      <*> switchWith 'l' "list" "List builds"
      <*> switchWith 'L' "latest" "Latest build"
      <*> switchWith 't' "check-remote-time" "Check remote rpm timestamps"
      <*> optional pkgMgrOpt
      <*> optional existingOpt
      <*> optional (strOptionWith 'b' "prefix" "SUBPKGPREFIX" "Prefix to use for subpackages [default: base package]")
      <*> selectOpt
      <*> optional disttagOpt
      <*> (flagWith' ReqNVR 'R' "nvr" "Give an N-V-R instead of package name" <|>
           flagWith ReqName ReqNV 'V' "nv" "Give an N-V instead of package name")
      <*> some (strArg "PKG|NVR|TASKID...")

    , Subcommand "progress"
      "Track running Koji tasks by buildlog size" $
      progressCmd
      <$> switchWith 'D' "debug" "Pretty-print raw XML result"
      <*> switchWith 'm' "modules" "Track module builds"
      <*> many (TaskId <$> argumentWith auto "TASKID")

    , Subcommand "buildlog-sizes" "Show buildlog sizes for nvr patterns" $
      buildlogSizesCmd <$> strArg "NVRPATTERN"

    , Subcommand "find"
      ("Simple quick common queries using words like: [" ++
       intercalate ", " (wordsList head) ++ "]") $
      findCmd
      <$> hubOpt
      <*> switchWith 'D' "debug" "Debug output including XML results"
      <*> many (strArg "PHRASE")
    ]
  where
    hubOpt = optional (strOptionWith 'H' "hub" "HUB"
                       ("KojiHub shortname or url (HUB = " ++
                        intercalate ", " knownHubs ++
                        ") [default: fedora]"))

    userOpt :: Parser UserOpt
    userOpt =
      User <$> strOptionWith 'u' "user" "USER" "Koji user"
      <|> flagWith' UserSelf 'M' "mine" "Your tasks (krb fasid)"

    selectOpt :: Parser Select
    selectOpt =
      flagLongWith' All "all" "all subpackages" <|>
      flagLongWith' Ask "ask" "ask for each subpackge [default if not installed]" <|>
      PkgsReq
      <$> many (strOptionWith 'p' "package" "SUBPKG" "select subpackage (glob) matches")
      <*> many (strOptionWith 'x' "except" "SUBPKG" "select subpackages not matching (glob)")
      <*> many (strOptionWith 'e' "exclude" "SUBPKG" "deselect subpackage (glob): overrides -p and -x")
      <*> many (strOptionWith 'i' "include" "SUBPKG" "additional subpackage (glob) to install: overrides -x and -e")

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

    pkgMgrOpt :: Parser PkgMgr
    pkgMgrOpt =
      flagLongWith' RPM "rpm" "Use rpm instead of dnf" <|>
      flagLongWith' OSTREE "rpm-ostree" "Use rpm-ostree instead of dnf" <|>
      flagLongWith' DNF "dnf" "Use dnf to install [default unless ostree]"

    existingOpt :: Parser ExistingStrategy
    existingOpt =
      flagWith' ExistingNoReinstall 'N' "no-reinstall" "Do not reinstall existing NVRs" <|>
      flagWith' ExistingSkip 'S' "skip-existing" "Ignore already installed subpackages (implies --no-reinstall)"
