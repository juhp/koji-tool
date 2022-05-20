# koji-tool

A CLI UI to the [Koji](https://koji.fedoraproject.org/koji/) buildsystem
with commands to query builds and tasks, install rpms,
and check buildlog sizes.

[Koji](https://pagure.io/koji/) is the RPM package buildsystem used by
Fedora, CentOS, and some other projects.

By default Fedora Koji is used.

A few illustrative examples:

`koji-tool tasks --mine --latest --state fail --tail`:
shows details of your last buildArch failure and the tail of the build.log
(equivalently you can use `koji-tool find my last fail`).

`koji-tool install systemd`: will try to install or update to the newest rpm packages from koji.

`koji-tool builds -L -s complete firefox`:
shows the last successful build with a url and other details.

## Commands
```shellsession
$ koji-tool --version
0.9
$ koji-tool --help
Query and track Koji tasks, and install rpms from Koji.

Usage: koji-tool [--version] COMMAND
  see https://github.com/juhp/koji-tool#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  builds                   Query Koji builds (by default lists most recent
                           builds)
  tasks                    Query Koji tasks (by default lists most recent
                           buildArch tasks)
  latest                   Query latest Koji build for tag
  install                  Install rpm packages directly from a Koji build task
  progress                 Track running Koji tasks by buildlog size
  buildlog-sizes           Show buildlog sizes for nvr patterns
  find                     Simple quick common queries using words
                           ('my','last','fail','complete','current','build')
```

## koji-tool builds

Query Koji for builds.

Somewhat like `koji list-builds --quiet ...`,
but it shows duration, and kojiweb urls.
It uses `date` to parse a specified date string
and can use an NVR glob pattern to select builds.

Note results are ordered by build_id (not time) for speed.

### Usage

By default lists up to 10 Fedora Koji builds.

```shellsession
$ koji-tool builds --help
Usage: koji-tool builds [-H|--hub HUB] [(-u|--user USER) | (-M|--mine)]
                        [(-L|--latest) | (-l|--limit INT)] [-s|--state STATE]
                        [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)]
                        [-t|--type TYPE] [-d|--details] [-D|--debug]
                        [(-b|--build NVR/BUILDID) | (-p|--pattern NVRPAT) |
                          PACKAGE]
  Query Koji builds (by default lists most recent builds)

Available options:
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -u,--user USER           Koji user
  -M,--mine                Your tasks (krb fasid)
  -L,--latest              Latest build
  -l,--limit INT           Maximum number of builds to show [default: 10]
  -s,--state STATE         Filter builds by state
                           (building,complete,deleted,fail(ed),cancel(ed)
  -B,--before TIMESTAMP    Builds completed before timedate [default: now]
  -F,--from TIMESTAMP      Builds completed after timedate
  -t,--type TYPE           Select builds by type: all,image,maven,module,rpm,win
  -d,--details             Show more details of builds
  -D,--debug               Pretty-print raw XML result
  -b,--build NVR/BUILDID   Show build
  -p,--pattern NVRPAT      Builds matching glob pattern
  -h,--help                Show this help text
```

Examples:

```shellsession
$ koji-tool builds -M -s fail
```
lists your recent builds that failed.

List latest build of a package:
```shellsession
$ koji-tool builds --pattern redhat-rpm-config*.fc37 --latest

redhat-rpm-config-218-1.fc37 BuildComplete
https://koji.fedoraproject.org/koji/buildinfo?buildID=1963037
https://koji.fedoraproject.org/koji/taskinfo?taskID=86677797
Start: Thu May  5 19:02:33 +08 2022
End:   Thu May  5 19:04:26 +08 2022
duration: 0h 1m 53s
```

## koji-tool tasks

Query Koji for tasks.

Somewhat like `koji list-tasks --quiet --all ...`,
but it shows duration, kojiweb urls and build.log size,
and it uses `date` to parse a specified date string
and can filter task results by package or nvr prefix.

Note results are ordered by task id (not time) for speed.

### Usage

By default it lists 10 most recent Fedora Koji buildArch tasks.

```shellsession
$ koji-tool tasks --help
Usage: koji-tool tasks [-H|--hub HUB] [(-u|--user USER) | (-M|--mine)]
                       [(-L|--latest) | (-l|--limit INT)] [-s|--state STATE]
                       [-a|--arch ARCH]
                       [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)]
                       [-m|--method METHOD] [-d|--details] [-D|--debug]
                       [(-P|--only-package PKG) | (-N|--only-nvr PREFIX)]
                       [-T|--tail]
                       [(-b|--build BUILD) | (-p|--pattern NVRPAT) |
                         PACKAGE|TASKID]
  Query Koji tasks (by default lists most recent buildArch tasks)

Available options:
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -u,--user USER           Koji user
  -M,--mine                Your tasks (krb fasid)
  -L,--latest              Latest build or task
  -l,--limit INT           Maximum number of tasks to show [default: 10]
  -s,--state STATE         Filter tasks by state
                           (open,close(d),cancel(ed),fail(ed),assigned,free)
  -a,--arch ARCH           Task arch
  -B,--before TIMESTAMP    Tasks completed before timedate [default: now]
  -F,--from TIMESTAMP      Tasks completed after timedate
  -m,--method METHOD       Select tasks by method (default 'buildArch'):
                           all,appliance,build,buildArch,buildContainer,buildMaven,buildNotification,buildSRPMFromSCM,chainbuild,chainmaven,createAppliance,createContainer,createImage,createLiveCD,createLiveMedia,createdistrepo,createrepo,dependantTask,distRepo,image,indirectionimage,livecd,livemedia,maven,newRepo,rebuildSRPM,runroot,tagBuild,tagNotification,vmExec,waitrepo,winbuild,wrapperRPM
  -d,--details             Show more details of builds
  -D,--debug               Pretty-print raw XML result
  -P,--only-package PKG    Filter task results to specified package
  -N,--only-nvr PREFIX     Filter task results by NVR prefix
  -T,--tail                Fetch the tail of build.log
  -b,--build BUILD         List child tasks of build
  -p,--pattern NVRPAT      Build tasks of matching pattern
  -h,--help                Show this help text
```

Examples:

```shellsession
$ koji-tool tasks -M -a aarch64 -s fail
```
lists your recent arm64 tasks that failed.

Show latest newRepo task:
```shellsession
$ koji-tool tasks --method newrepo --latest

eln-build newRepo TaskClosed
https://koji.fedoraproject.org/koji/taskinfo?taskID=86821565
Start: Mon May  9 11:53:53 +08 2022
End:   Mon May  9 11:57:13 +08 2022
duration: 0h 3m 20s
```

List latest package build's tasks:
```shellsession
$ koji-tool tasks --latest redhat-rpm-config

redhat-rpm-config-218-1.eln118.noarch TaskClosed
https://koji.fedoraproject.org/koji/taskinfo?taskID=86685316 (parent: 86685296)
Start: Fri May  6 00:11:10 +08 2022
End:   Fri May  6 00:13:07 +08 2022
duration: 0h 1m 57s
https://kojipkgs.fedoraproject.org/work/tasks/5316/86685316/build.log (13kB)
```

## koji-tool find
This provides shortcuts to a few select common searches

### Usage
`koji-tool find my builds` shows your 10 most recent koji builds (equivalent to `koji-tool builds --mine`)

`koji-tool find my last fail` shows your most recent task failure including the tail of the build.log (equivalent to `koji-tool tasks -MLT -s fail`).

`koji-tool find last complete build` shows the latest completed koji build (equivalent to `koji-tool builds -L -s complete`).

### Help
```shellsession
$ koji-tool find
koji-tool: quick handles these words:

my mine
last latest
fail failure failed
complete completed completion close closed finish finished
current building open
build builds
PACKAGE
USER's

```

## koji-tool install

Download and install rpms from a Koji build or task.

By default it only downloads binaries of already-installed subpackages,
but there are options to list and select or exclude specific subpackages.

Note this command is intended for development and testing purposes
and should not be necessary/used normally on production systems,
but it can be very helpful for quickly testing an specific package build or
update.

### Usage

```shellsession
$ koji-tool install podman
```
will download the latest build for your Fedora version,
and try to install it.
Use `--disttag` suffix to select a different Fedora version.

```shellsession
$ koji-tool install TASKID --exclude "*-devel"
```
will install all the non-devel subpackages from the task.

A more complex example:
```shellsession
$ koji-tool install google-noto-fonts --prefix google-noto -p 'sans-*-vf-fonts' -x 'sans-*-ui-vf-fonts'
```
installs all the Google Noto Sans variable fonts excluding UI faces.

One can use `--hub` to specify a different Koji hub build service.

#### Selecting subpackages

By default only installed subpackages are downloaded and updated,
but the following options change the behavior:

`--package`: select subpackages by name or glob pattern (this doesn't work currently for multiple builds/tasks)

`--exclude`: exclude subpackages by name or glob pattern

`--all`: install all subpackages

`--ask`: ask about each subpackage

`--prefix`: override the subpackage prefix

### Help
```shellsession
$ koji-tool install --help
Usage: koji-tool install [-n|--dry-run] [-D|--debug] [-y|--yes] [-H|--hub HUB]
                         [-P|--packages-url URL] [-l|--list] [-L|--latest]
                         [-N|--no-reinstall] [-b|--prefix SUBPKGPREFIX]
                         [(-a|--all) | (-A|--ask) | [-p|--package SUBPKG]
                           [-x|--exclude SUBPKG]] [-d|--disttag DISTTAG]
                         [(-R|--nvr) | (-V|--nv)] PKG|NVR|TASKID...
  Install rpm packages directly from a Koji build task

Available options:
  -n,--dry-run             Don't actually download anything
  -D,--debug               More detailed output
  -y,--yes                 Assume yes to questions (implies --all if not
                           installed)
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -P,--packages-url URL    KojiFiles packages url [default: Fedora]
  -l,--list                List builds
  -L,--latest              Latest build
  -N,--no-reinstall        Do not reinstall existing NVRs
  -b,--prefix SUBPKGPREFIX Prefix to use for subpackages [default: base package]
  -a,--all                 all subpackages
  -A,--ask                 ask for each subpackge [default if not installed]
  -p,--package SUBPKG      Subpackage (glob) to install
  -x,--exclude SUBPKG      Subpackage (glob) not to install
  -d,--disttag DISTTAG     Use a different disttag [default: .fc36]
  -R,--nvr                 Give an N-V-R instead of package name
  -V,--nv                  Give an N-V instead of package name
  -h,--help                Show this help text
```

## koji-tool progress
Shows the progress of active koji builds tasks
by checking the size of their build.log files.

This is useful for monitoring the build progress of large packages that take
a long time to complete for which some arch's may take considerably longer.

### Usage

```shellsession
$ koji-tool progress --mine
:
$ koji-tool progress 81148584  # ← Koji taskid
:
23:19:19 vim-8.2.4068-1.fc36 (81148584)
aarch64    351kB [109,133 B/min]
armhfp     133kB [ 65,244 B/min]
ppc64le    493kB [141,598 B/min] TaskClosed
s390x      558kB [100,481 B/min] TaskClosed
:
```

The `buildlog-sizes` command is similar but runs once over nvr patterns.

## Installation
Builds for fedora are available in [copr](https://copr.fedorainfracloud.org/coprs/petersen/koji-tool/monitor/detailed).

## Build
`cabal-rpm builddep && cabal install || stack install`

## History
The query, install, progress, buildlog-sizes were originally separate programs
and projects (koji-query, koji-install, koji-progress),
and merged together into koji-install (after 0.5) and renamed
to koji-tool. See the other original repos for their history.
