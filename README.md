# koji-install

A CLI tool for downloading and install rpms from Fedora koji.

## Usage

$ koji-install podman

Will download the latest build for your Fedora version,
and try to install it.

### Selecting subpackages

By default only installed subpackages are updated,
but the following options change the behavior:

`--all`: install all subpackages

`--ask`: ask about each subpackage

`--base-only`: only install base package

`--exclude-devel`: skip devel subpackages

### Help
```shellsession
$ koji-install
Install latest build from Koji

Usage: koji-install [--version] [-n|--dry-run]
                    [(-a|--all) | (-A|--ask) | (-b|--base-only) |
                      (-D|--exclude-devel)] [-d|--disttag DISTTAG]
                    [(-R|--nvr) | (-V|--nv)] PACKAGE
  Download and install latest package build from Koji tag.

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dry-run             Don't actually download anything
  -a,--all                 all subpackages
  -A,--ask                 ask for each subpackge
  -b,--base-only           only base package
  -D,--exclude-devel       Skip devel packages
  -d,--disttag DISTTAG     Use a different disttag [default: .fc35]
  -R,--nvr                 Give an N-V-R instead of package name
  -V,--nv                  Give an N-V instead of package name
```

## Installation

`stack install` or `cabal-rpm builddep && cabal install`.
