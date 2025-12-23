# koji-tool

A CLI UI to the [Koji](https://koji.fedoraproject.org/koji/) buildsystem
with commands to query builds and tasks, install rpms,
and check buildlog sizes.

[Koji](https://pagure.io/koji/) is the RPM package buildsystem used by
Fedora Linux, CentOS Stream, RHEL, and some other projects.

By default Fedora Koji is used.

A few illustrative examples:

`koji-tool tasks --mine --latest --state fail --tail`:
shows details of your last buildArch failure and the tail of the build.log
(equivalently you can use `koji-tool find my last fail`).

`koji-tool install systemd`: will try to install or update to the newest rpm packages from koji.

`koji-tool builds -L -s complete firefox`:
shows the last successful build with a url and other details.

## Commands
`$ koji-tool --version`

```
1.3.1
```

`$ koji-tool --help`

```
Query and track Koji tasks, and install rpms from Koji.

Usage: koji-tool [--version] COMMAND

  see https://github.com/juhp/koji-tool#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  builds                   Query Koji builds (by default lists the most recent
                           builds)
  tasks                    Query Koji tasks (by default lists the most recent
                           buildArch tasks)
  latest                   Query latest Koji build for tag
  install                  Install rpm packages directly from a Koji build task
  progress                 Track running Koji tasks by buildlog size
  buildlog-sizes           Show buildlog sizes for nvr patterns
  find                     Simple quick common queries using words like: [my,
                           last, fail, complete, current, build, detail,
                           install, tail, notail, hwinfo, rootlog, x86_64,
                           debug, PACKAGE, USER\'s, LIMIT]
  tagged                   List builds for tag
```

## koji-tool builds

The `builds` command queries Koji for builds.

Somewhat like `koji list-builds --quiet ...`,
but it shows duration, and kojiweb urls.

Note results are ordered by build_id (not time) for speed.

### Usage

By default it lists up to 10 Fedora Koji builds -
this can be changed with the `--limit` or `--unlimited` options.

One can select builds also by time/date options,
which are parsed by the system `date` utility.

One can also use an NVR glob pattern to select builds.


`$ koji-tool builds --help`

```
Usage: koji-tool builds [-H|--hub HUB] [(-u|--user USER) | (-M|--mine)] 
                        [(-L|--latest) | (-U|--unlimited) | (-l|--limit INT)] 
                        [-s|--state STATE] 
                        [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)] 
                        [--type TYPE] [(-d|--details) | (-t|--tasks)] 
                        [-T|--tail] [-i|--install INSTALLOPTS] [-D|--debug] 
                        [(-b|--build NVR/BUILDID) | (-p|--pattern NVRPAT) | 
                          PACKAGE]

  Query Koji builds (by default lists the most recent builds)

Available options:
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -u,--user USER           Koji user
  -M,--mine                Your tasks (krb fasid)
  -L,--latest              Latest build
  -U,--unlimited           No limit on number of results
  -l,--limit INT           Maximum number of builds to show [default: 5]
  -s,--state STATE         Filter builds by state
                           (building,complete,deleted,fail(ed),cancel(ed)
  -B,--before TIMESTAMP    Builds completed before timedate [default: now]
  -F,--from TIMESTAMP      Builds completed after timedate
  --type TYPE              Select builds by type: all,image,maven,module,rpm,win
  -d,--details             Show more build details
  -t,--tasks               Show details and tasks
  -T,--tail                Fetch the tail of build.log
  -i,--install INSTALLOPTS Install the package with 'install' options
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

`$ koji-tool builds --pattern redhat-rpm-config*.fc42 --latest`

```
redhat-rpm-config-342-4.fc42 BuildComplete (churchyard)
https://koji.fedoraproject.org/koji/buildinfo?buildID=2696410
https://koji.fedoraproject.org/koji/taskinfo?taskID=131402281
start:      Fri Apr 11 18:30:43 +08 2025
completion: Fri Apr 11 18:32:00 +08 2025
duration: 1 min 17 sec
https://kojipkgs.fedoraproject.org/packages/redhat-rpm-config/342/4.fc42
```

## koji-tool tasks

The `tasks` command queries Koji for tasks.

Somewhat like `koji list-tasks --quiet --all ...`,
but it shows duration, kojiweb urls and optionally build.log size.

Note results are ordered by task id (not time) for speed.

### Usage

By default it lists 10 most recent Fedora Koji buildArch tasks,
this can be changed with the `--limit` or `--unlimited` options.

Tasks can be searched by package, task id (including parent and children),
by build or NVR pattern, also by state, method, and user.

Results can be restricted by date options,
which are parsed by the `date` utility.
The received results can be filtered too locally by package or nvr prefix.

Multiple results are listed concisely each on one line by default.
A single task result is displayed in detail over multiple lines.
These levels of detail can be controlled with
the `--details` and `--concise` options.

`$ koji-tool tasks --help`

```
Usage: koji-tool tasks [-H|--hub HUB] [(-u|--user USER) | (-M|--mine)] 
                       [(-L|--latest) | (-U|--unlimited) | (-l|--limit INT)] 
                       [-s|--state STATE] [-a|--arch ARCH] 
                       [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)] 
                       [-m|--method METHOD] [-D|--debug] 
                       [(-P|--only-package PKG) | (-N|--only-nvr PREFIX)] 
                       [(-d|--details) | (-c|--concise)] [-T|--tail] 
                       [--hw-info | --rootlog] [-g|--grep STRING] 
                       [-i|--install INSTALLOPTS] 
                       [(-b|--build BUILD) | (-p|--pattern NVRPAT) | 
                         --children TASKID | --parent TASKID | PACKAGE|TASKID]

  Query Koji tasks (by default lists the most recent buildArch tasks)

Available options:
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -u,--user USER           Koji user
  -M,--mine                Your tasks (krb fasid)
  -L,--latest              Latest build/task
  -U,--unlimited           No limit on number of results
  -l,--limit INT           Maximum number of build/tasks to show [default: 20]
  -s,--state STATE         Filter tasks by state
                           (open,close(d),cancel(ed),fail(ed),assigned,free)
  -a,--arch ARCH           Task arch
  -B,--before TIMESTAMP    Tasks completed before timedate [default: now]
  -F,--from TIMESTAMP      Tasks completed after timedate
  -m,--method METHOD       Select tasks by method (default 'buildArch'):
                           all,appliance,build,buildArch,buildContainer,buildMaven,buildNotification,buildSRPMFromSCM,chainbuild,chainmaven,createAppliance,createContainer,createImage,createLiveCD,createLiveMedia,createdistrepo,createrepo,dependantTask,distRepo,image,indirectionimage,livecd,livemedia,maven,newRepo,rebuildSRPM,runroot,tagBuild,tagNotification,vmExec,waitrepo,winbuild,wrapperRPM
  -D,--debug               Pretty-print raw XML result
  -P,--only-package PKG    Filter task results to specified package
  -N,--only-nvr PREFIX     Filter task results by NVR prefix
  -d,--details             Show more task details
  -c,--concise             Compact task output
  -T,--tail                Fetch the tail of build.log
  --hw-info                Fetch hw_info.log
  --rootlog                Fetch root.log
  -g,--grep STRING         Filter matching log lines
  -i,--install INSTALLOPTS Install the package with 'install' options
  -b,--build BUILD         List child tasks of build
  -p,--pattern NVRPAT      Build tasks of matching pattern
  --children TASKID        Children tasks of parent
  --parent TASKID          Parent of task
  -h,--help                Show this help text
```

Examples:

```shellsession
$ koji-tool tasks -M -a aarch64 -s fail
```
lists your recent arm64 tasks that failed.

Show latest newRepo task:

`$ koji-tool tasks --method newrepo --latest`

```
ValueStruct [("id",ValueInt 45727),("name",ValueString "epel9-build")] ValueStruct [("opts",ValueStruct []),("__starstar",ValueBool True)] newRepo TaskClosed (kojira)
https://koji.fedoraproject.org/koji/taskinfo?taskID=140300590
create:     Tue Dec 23 14:41:11 +08 2025
start:      Tue Dec 23 14:42:03 +08 2025
completion: Tue Dec 23 14:45:38 +08 2025
duration: 3 min 35 sec (start delay 52 sec)
```

List latest package build's tasks:

`$ koji-tool tasks --latest redhat-rpm-config`

```
redhat-rpm-config-343-19.eln153.noarch TaskClosed (distrobuildsync-eln/jenkins-continuous-infra.apps.ci.centos.org)
https://koji.fedoraproject.org/koji/taskinfo?taskID=140205053 (parent: 140204866)
create:     Sat Dec 20 23:40:05 +08 2025
start:      Sat Dec 20 23:41:17 +08 2025
completion: Sat Dec 20 23:42:19 +08 2025
duration: 1 min 2 sec (start delay 1 min 12 sec)
https://kojipkgs.fedoraproject.org/work/tasks/5053/140205053/build.log (16kB)
```

It is also possible to install packages from a task using
`--install "SUBPKG OPTIONS"`.
See the install command documentation below for more details.

Use `--tail` to show the tail of the build.log: it falls back to root.log
automatically if the build.log is considered too small.
Use `--hw-info` to display hw_info.log instead.
Also using the `--grep` option one can filter the log output for lines matching
the given string (accepts leading `^` and trailing `$`).

## koji-tool install

Use the `install` command to download and install rpms
from a Koji build or task.

By default it only downloads binaries of already-installed subpackages,
but there are options to list and select or exclude specific subpackages.

Note this command is intended for development and testing purposes
and should not be necessary/used normally on production systems,
but it can be very helpful for quickly testing a specific package build or
update locally.

### Usage

```shellsession
$ koji-tool install podman
```
will download the latest build for your Fedora version,
and try to install it.
Use `--disttag` suffix to select a different OS version.

```shellsession
$ koji-tool install TASKID --except "*-devel"
```
will install all the non-devel subpackages from the task.

A more complex example:
```shellsession
$ koji-tool install google-noto-fonts --prefix google-noto --package 'sans-*-vf-fonts' --exclude 'sans-*-ui-vf-fonts'
```
installs all the Google Noto Sans variable fonts excluding UI faces.

One can use `--hub` to specify a different Koji hub build service.

#### Selecting subpackages

By default only installed subpackages are downloaded and updated,
but the following options change the behavior:

`--package`: select subpackages by name or glob pattern

`--except`: select subpackages not matching name or glob pattern

`--exclude`: exclude subpackages by name or glob pattern (overrides --package and --except)

`--include`: include subpackages by name or glob pattern (overrides --exclude)

`--all`: install all subpackages

`--ask`: ask about each subpackage

`--prefix`: override the subpackage prefix

(Subpackage selection has only really been tested so far
for a single build/task.)

### Help
`$ koji-tool install --help`

```
Usage: koji-tool install [-n|--dry-run] [-D|--debug] [-y|--yes] [-H|--hub HUB] 
                         [-P|--packages-url URL] [-l|--list] [-L|--latest] 
                         [-t|--check-remote-time] 
                         [--rpm | --rpm-ostree | --dnf5 | --dnf3] 
                         [--allow-erasing] [-a|--arch ARCH] 
                         [(-N|--no-reinstall) | (-S|--skip-existing) | 
                           (-O|--only-existing) | (-E|--error-existing)] 
                         [-b|--prefix SUBPKGPREFIX] 
                         [--all | --ask | [-p|--package SUBPKG] 
                           [-e|--except SUBPKG] [-x|--exclude SUBPKG] 
                           [-i|--include SUBPKG]] [-d|--disttag DISTTAG] 
                         [--tagged | (-R|--nvr) | (-V|--nv)]
                         PKG|NVR|TASKID|TAG...

  Install rpm packages directly from a Koji build task

Available options:
  -n,--dry-run             Don't actually download anything
  -D,--debug               More detailed output
  -y,--yes                 Assume yes to questions
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -P,--packages-url URL    KojiFiles packages url [default: Fedora]
  -l,--list                List RPMs
  -L,--latest              Latest build
  -t,--check-remote-time   Check remote rpm timestamps
  --rpm                    Use rpm instead of dnf
  --rpm-ostree             Use rpm-ostree instead of dnf
  --dnf5                   Use dnf5 to install
  --dnf3                   Use dnf-3 to install [default dnf unless ostree]
  --allow-erasing          Use dnf --allowerasing
  -a,--arch ARCH           Task arch
  -N,--no-reinstall        Do not reinstall existing NVRs
  -S,--skip-existing       Ignore already installed subpackages (implies
                           --no-reinstall)
  -O,--only-existing       Only update existing installed subpackages
  -E,--error-existing      Abort for existing installed subpackages
  -b,--prefix SUBPKGPREFIX Prefix to use for subpackages [default: base package]
  --all                    all subpackages [default if not installed]
  --ask                    ask for each subpackage
  -p,--package SUBPKG      select subpackage (glob) matches
  -e,--except SUBPKG       select subpackages not matching (glob)
  -x,--exclude SUBPKG      deselect subpackage (glob): overrides -p and -e
  -i,--include SUBPKG      additional subpackage (glob) to install: overrides -x
  -d,--disttag DISTTAG     Select a disttag different to system
  --tagged                 Give a tag instead of package name
  -R,--nvr                 Give an N-V-R instead of package name
  -V,--nv                  Give an N-V instead of (default) package name
  -h,--help                Show this help text
```

## koji-tool find
This provides shortcuts to a few select common searches

### Usage
`koji-tool find my builds` shows your 10 most recent koji builds (equivalent to `koji-tool builds --mine`)

`koji-tool find my last fail` shows your most recent task failure including the tail of the build.log (equivalent to `koji-tool tasks -MLT -s fail`).

`koji-tool find last complete build` shows the latest completed koji build (equivalent to `koji-tool builds -L -s complete`).

### Help
`$ koji-tool find --help`

```
Usage: koji-tool find [-H|--hub HUB] [PHRASE]

  Simple quick common queries using words like: [my, last, fail, complete,
  current, build, detail, install, tail, notail, hwinfo, rootlog, x86_64, debug,
  PACKAGE, USER\'s, LIMIT]

Available options:
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -h,--help                Show this help text
```

## koji-tool progress
Shows the progress of active koji builds tasks
by checking the size of their build.log files.

This is useful for monitoring the build progress of large packages that take
a long time to complete for which some arch's may take considerably longer.

By default it shows progress of the user's open build tasks.

### Usage

```shellsession
$ koji-tool progress
:
:
$ koji-tool progress 93808251
21:39:41 webkitgtk-2.38.2-1.eln123 (93808251) 9h 32m
aarch64  87,669kB (16:50:31)  3h 55m TaskClosed
i386     88,120kB (16:33:14)  4h 21m TaskClosed
noarch        3kB (12:11:19)  3m 28s TaskClosed SRPM
ppc64le  85,853kB (21:25:42)
s390x    87,692kB (21:20:43)  9h 11m TaskClosed
x86_64   89,914kB (19:57:47)  7h 48m TaskClosed

21:47:57 webkitgtk-2.38.2-1.eln123 (93808251) 89914kB, 9h 40m
ppc64le  88,117kB (21:47:21) [  1,742 B/s] (1299s)

:
```

The `buildlog-sizes` command is similar but runs once over nvr patterns.

## koji-tool tagged
Similar to `koji list-tagged` with an experimental `--install` option
allowing one to install all the builds from a sidetag.

### Help
`$ koji-tool tagged --help`

```
Usage: koji-tool tagged [-H|--hub HUB] [-i|--install] TAG

  List builds for tag

Available options:
  -H,--hub HUB             KojiHub shortname or url (HUB = fedora, stream,
                           rpmfusion, or URL) [default: fedora]
  -i,--install             Install packages
  -h,--help                Show this help text
```


## Installation
koji-tool is packaged in Fedora

## Building from git/source
Either use `cabal-install` or `stack`.
Either way you need a ghc compiler installed, of course.

### Build from source with cabal

With cabal, you can save a lot of time by first running `cabal-rpm builddep`
to install distro deps, and then:

```
$ cabal install
```

### Build from source with stack

```
$ stack init --resolver lts
$ stack install
```

## Contributing
koji-tool is distributed under a BSD license.

Bug reports, suggestions and contributions are welcomed:
please open an issue at: https://github.com/juhp/koji-tool/
