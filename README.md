# koji-tool

A CLI UI to the [Koji](https://koji.fedoraproject.org/koji/) buildsystem
with commands to query tasks, install rpms, and check buildlog sizes.

[Koji](https://pagure.io/koji/) is the RPM package buildsystem used by
Fedora, CentOS, and some other projects.

## koji-tool query

Query Koji for tasks.

Similar to `koji list-tasks --mine --quiet --all ...`,
but it shows duration, kojiweb urls and build.log size,
and it uses `date` to parse a specified date string
and can filter results by package.

### Usage

By default it lists your Fedora Koji tasks from today.

```shellsession
$ koji-tool query --help
Usage: koji-tool query [-S|--server URL] [-u|--user USER] [-l|--limit INT]
                       [(-t|--task TASKID) | (-c|--children TASKID) |
                         (-b|--build BUILD)] [-s|--state STATE] [-a|--arch ARCH]
                       [(-B|--before TIMESTAMP) | (-F|--from TIMESTAMP)]
                       [-m|--method METHOD] [-D|--debug]
                       [(-p|--package PKG) | (-n|--nvr PREFIX)]
  Query Koji tasks (by default lists your tasks today)

Available options:
  -S,--server URL          Koji Hub [default: Fedora]
  -u,--user USER           Koji user [default: fasid]
  -l,--limit INT           Maximum number of tasks to show [default: 10]
  -t,--task TASKID         Show task
  -c,--children TASKID     List child tasks of parent
  -b,--build BUILD         List child tasks of build
  -s,--state STATE         Filter tasks by state (open, close(d), cancel(ed),
                           fail(ed), assigned, free)
  -a,--arch ARCH           Task arch
  -B,--before TIMESTAMP    Tasks completed before timedate
  -F,--from TIMESTAMP      Tasks completed after timedate [default: today]
  -m,--method METHOD       Select tasks by method: [build,buildarch,etc]
  -D,--debug               Pretty-pretty raw XML result
  -p,--package PKG         Filter results to specified package
  -n,--nvr PREFIX          Filter results by NVR prefix
  -h,--help                Show this help text
```

Example:

```shellsession
$ koji-tool query -a aarch64 --from "last week" -s fail
```
lists your arm64 tasks that failed in the last week.

List kojira tasks from the last hour:
```shellsession
$ koji-tool query --from hour -u kojira
completed after 2022-01-13 09:14:41+0800

epel7-infra-mailman newRepo TaskFailed
https://koji.fedoraproject.org/koji/taskinfo?taskID=81172651
Start: Thu Jan 13 10:12:09  2022
End:   Thu Jan 13 10:14:09  2022
duration: 0h 2m 0s
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

By default it uses Fedora Koji.

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
                         [-P|--packages-url URL] [-l|--list]
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
  -a,--all                 all subpackages
  -A,--ask                 ask for each subpackge [default if not installed]
  -p,--package SUBPKG      Subpackage (glob) to install
  -x,--exclude SUBPKG      Subpackage (glob) not to install
  -d,--disttag DISTTAG     Use a different disttag [default: .fc35]
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
