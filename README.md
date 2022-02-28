# koji-tool

A CLI UI to the [Koji](https://koji.fedoraproject.org/koji/) buildsystem
with commands to query builds and tasks, install rpms,
and check buildlog sizes.

[Koji](https://pagure.io/koji/) is the RPM package buildsystem used by
Fedora, CentOS, and some other projects.

By default Fedora Koji is used.

## Commands
```shellsession
$ koji-tool --help
Query and track Koji tasks, and install rpms from Koji.

Usage: koji-tool [--version] COMMAND
  see https://github.com/juhp/koji-tool#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  install                  Install rpm packages directly from a Koji build task
  builds                   Query Koji builds (by default lists your most recent
                           builds)
  tasks                    Query Koji tasks (by default lists your most recent
                           buildArch tasks)
  progress                 Track running Koji tasks by buildlog size
  buildlog-sizes           Show buildlog sizes for nvr patterns
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

```
$ koji-tool install podman
```
will download the latest build for your Fedora version,
and try to install it.
Use `--disttag` suffix to select a different Fedora version.

```
$ koji-tool install TASKID --exclude "*-devel"
```
will install all the non-devel subpackages from the task.

One can use `--hub` to specify a different Koji hub build service.

#### Selecting subpackages

By default only installed subpackages are downloaded and updated,
but the following options change the behavior:

`--package`: select subpackages by name or glob pattern (this doesn't work currently on multiple builds/tasks)

`--exclude`: exclude subpackages by name or glob pattern

`--all`: install all subpackages

`--ask`: ask about each subpackage

### Help
```shellsession
$ koji-tool install --help
Usage: koji-tool install [-n|--dry-run] [-D|--debug] [-H|--hub HUB]
                         [-P|--packages-url URL] [-l|--list] [-L|--latest]
                         [(-a|--all) | (-A|--ask) | [-p|--package SUBPKG]
                           [-x|--exclude SUBPKG]] [-d|--disttag DISTTAG]
                         [(-R|--nvr) | (-V|--nv)] PKG|NVR|TASKID...
  Install rpm packages directly from a Koji build task

Available options:
  -n,--dry-run             Don't actually download anything
  -D,--debug               More detailed output
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -P,--packages-url URL    KojiFiles packages url [default: Fedora]
  -l,--list                List builds
  -L,--latest              Latest build
  -a,--all                 all subpackages
  -A,--ask                 ask for each subpackge [default if not installed]
  -p,--package SUBPKG      Subpackage (glob) to install
  -x,--exclude SUBPKG      Subpackage (glob) not to install
  -d,--disttag DISTTAG     Use a different disttag [default: .fc35]
  -R,--nvr                 Give an N-V-R instead of package name
  -V,--nv                  Give an N-V instead of package name
  -h,--help                Show this help text
```

## koji-tool builds

Query Koji for builds.

Somewhat like `koji list-builds --quiet ...`,
but it shows duration, and kojiweb urls.
It uses `date` to parse a specified date string
and can use an NVR glob pattern to select builds.

### Usage

By default it lists your 10 most recent Fedora builds.

```shellsession
$ koji-tool builds --help
Usage: koji-tool builds [-S|--server URL] [-u|--user USER]
                        [(-L|--latest) | (-l|--limit INT)]
                        [(-b|--build BUILD) | (-p|--package PKG)]
                        [-s|--state STATE]
                        [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)]
                        [-t|--type TYPE] [-d|--details] [-D|--debug]
                        [NVRPATTERN]
  Query Koji builds (by default lists your most recent builds)

Available options:
  -S,--server URL          Koji Hub [default: Fedora]
  -u,--user USER           Koji user [default: fasid]
  -L,--latest              Latest build
  -l,--limit INT           Maximum number of builds to show [default: 10]
  -b,--build BUILD         Show build details
  -p,--package PKG         Builds of package
  -s,--state STATE         Filter builds by state (FIXME list)
  -B,--before TIMESTAMP    Builds completed before timedate [default: now]
  -F,--from TIMESTAMP      Builds completed after timedate
  -t,--type TYPE           Select builds by type
  -d,--details             Show more details of builds
  -D,--debug               Pretty-print raw XML result
  -h,--help                Show this help text
```

Examples:

```shellsession
$ koji-tool builds --from "last week" -s fail
```
lists your builds that failed in the last week.

List builds of a package:
```shellsession
$ koji-tool builds --package redhat-rpm-config --limit 2
redhat-rpm-config-214-1.eln114 (2022-02-11 07:10:26)
redhat-rpm-config-214-1.fc37 (2022-02-10 15:47:32)
```

## koji-tool tasks

Query Koji for tasks.

Somewhat like `koji list-tasks --mine --quiet --all ...`,
but it shows duration, kojiweb urls and build.log size,
and it uses `date` to parse a specified date string
and can filter task results by package or nvr prefix.

### Usage

By default it lists your 10 most recent Fedora Koji buildArch tasks.

```shellsession
$ koji-tool tasks --help
Usage: koji-tool tasks [-S|--server URL] [-u|--user USER]
                       [(-L|--latest) | (-l|--limit INT)]
                       [(-t|--task TASKID) | (-c|--children TASKID) |
                         (-b|--build BUILD) | (-p|--package PKG)]
                       [-s|--state STATE] [-a|--arch ARCH]
                       [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)]
                       [-m|--method METHOD] [-D|--debug]
                       [(-P|--only-package PKG) | (-N|--only-nvr PREFIX)]
                       [-T|--tail]
  Query Koji tasks (by default lists your most recent buildArch tasks)

Available options:
  -S,--server URL          Koji Hub [default: Fedora]
  -u,--user USER           Koji user [default: fasid]
  -L,--latest              Latest build or task
  -l,--limit INT           Maximum number of tasks to show [default: 10]
  -t,--task TASKID         Show task
  -c,--children TASKID     List child tasks of parent
  -b,--build BUILD         List child tasks of build
  -p,--package PKG         Build tasks of package
  -s,--state STATE         Filter tasks by state (open, close(d), cancel(ed),
                           fail(ed), assigned, free)
  -a,--arch ARCH           Task arch
  -B,--before TIMESTAMP    Tasks completed before timedate [default: now]
  -F,--from TIMESTAMP      Tasks completed after timedate
  -m,--method METHOD       Select tasks by method (default 'buildArch'):
                           all,appliance,build,buildArch,buildContainer,buildMaven,buildNotification,buildSRPMFromSCM,chainbuild,chainmaven,createAppliance,createContainer,createImage,createLiveCD,createLiveMedia,createdistrepo,createrepo,dependantTask,distRepo,image,indirectionimage,livecd,livemedia,maven,newRepo,rebuildSRPM,runroot,tagBuild,tagNotification,vmExec,waitrepo,winbuild,wrapperRPM
  -D,--debug               Pretty-print raw XML result
  -P,--only-package PKG    Filter task results to specified package
  -N,--only-nvr PREFIX     Filter task results by NVR prefix
  -T,--tail                Fetch the tail of build.log
  -h,--help                Show this help text
```

Examples:

```shellsession
$ koji-tool tasks -a aarch64 --from "last week" -s fail
```
lists your arm64 tasks that failed in the last week.

List kojira tasks from the last hour:
```shellsession
$ koji-tool tasks --from hour -u kojira
completed after 2022-01-13 09:14:41+0800

epel7-infra-mailman newRepo TaskFailed
https://koji.fedoraproject.org/koji/taskinfo?taskID=81172651
Start: Thu Jan 13 10:12:09  2022
End:   Thu Jan 13 10:14:09  2022
duration: 0h 2m 0s
```

List package build tasks:
```shellsession
$ koji-tool tasks --package redhat-rpm-config --latest

redhat-rpm-config-214-1.eln114 noarch TaskClosed
https://koji.fedoraproject.org/koji/taskinfo?taskID=82667980 (parent: 82667916)
Start: Fri Feb 11 15:08:30  2022
End:   Fri Feb 11 15:10:09  2022
duration: 0h 1m 38s
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
`cabal-rpm builddep && cabal install` or `stack install`.

## History
The query, install, progress, buildlog-sizes were originally separate programs
and projects (koji-query, koji-install, koji-progress),
and merged together into koji-install (after 0.5) and renamed
to koji-tool. See the other original repos for their history.
