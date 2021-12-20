# koji-install

A CLI tool to download and install rpms from a Koji build or task.

Koji is a package buildsystem used by Fedora, Centos, and some other projects.

## Usage

By default it uses Fedora Koji.

$ koji-install podman

Will download the latest build for your Fedora version,
and try to install it.

You can specify a different Koji hub service with `--hub`.

### Selecting subpackages

By default only installed subpackages are downloaded and updated,
but the following options change the behavior:

`--package`: select subpackages by name or glob pattern (this doesn't work currently on multiple builds/tasks)

`--exclude`: exclude subpackages by name or glob pattern

`--all`: install all subpackages

`--ask`: ask about each subpackage

### Help
```shellsession
$ koji-install --help
Download and install latest package build from Koji tag.

Usage: koji-install [--version] [-n|--dry-run] [-D|--debug] [-H|--hub HUB]
                    [-P|--packages-url URL] [-l|--list]
                    [(-a|--all) | (-A|--ask) | (-p|--package SUBPKG) |
                      (-x|--exclude SUBPKG)] [-d|--disttag DISTTAG]
                    [(-R|--nvr) | (-V|--nv)] PACKAGE|TASKID...
  HUB = fedora, stream, rpmfusion, or URL

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dry-run             Don't actually download anything
  -D,--debug               More detailed output
  -H,--hub HUB             KojiHub shortname or url [default: fedora]
  -P,--packages-url URL    KojiFiles packages url [default: Fedora]
  -l,--list                List builds
  -a,--all                 all subpackages
  -A,--ask                 ask for each subpackge [default if not installed]
  -p,--package SUBPKG      Subpackage (glob) to install
  -x,--exclude SUBPKG      Subpackage (glob) not to install
  -d,--disttag DISTTAG     Use a different disttag [default: .fc35]
  -R,--nvr                 Give an N-V-R instead of package name
  -V,--nv                  Give an N-V instead of package name
```

## Installation
Builds for fedora are available in [copr](https://copr.fedorainfracloud.org/coprs/petersen/koji-tools/monitor/detailed).

## Build
`cabal-rpm builddep && cabal install` or `stack install`.
