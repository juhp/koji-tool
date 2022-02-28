# Version history of koji-tool

# 0.8 (2022-02-28)
- rename 'query' to 'tasks'
- 'tasks': new '--tail' option to fetch and display tail of build.log
   and show root.log if build.log is too small
- 'tasks': expand methods list and list in --method help
- new 'builds' query command (replaces 'tasks' builds listing)
- only print "before <date>" when date given

# 0.7 (2022-01-25)
- query: new options: '--package' to search recent builds and '--latest'
- query --method: now defaults to buildArch (use 'any' to include all)
- query: now defaults to "--before now" (instead of "--from today 00:00")
- query --arch: map i686 to i386 and armv7hl to armhfp
- query: do not drop tasks without string request

# 0.6.1 (2022-01-14)
- install --list: now lists the rpms of a unique nvr
- install --list: new --latest option which only finds the latest build
- install --nv: now actually looks for N-V nor N-V-R
- install --exclude: don't exclude subpackage when a rpm package matches

## 0.6 (2022-01-13)
- renamed from koji-install to koji-tool, which combines koji-query and koji-progress
- subcommands are 'install', 'query', 'progress', and 'buildlog-sizes'
- 'query' now has separate before/after options: --before and --from
- 'query' now defaults to '--from 00:00' and supports days of week & today/yesterday timedates
- 'query' now limits to 10 results by default in descending order
- 'install' now uses a single curl invocation to download multiple rpms

# Release history for koji-install

## 0.5 (2021-12-27)
- --package and --exclude filters can now be combined
- --package and --exclude now also check subpackage names without base prefix

## 0.4 (2021-12-20)
- support installing/listing by koji taskid
- select subpackages with --package and --exclude, by name or globbing
- check remote files date/size with http-directory
- listing a task either lists the task's children or rpms
- use dnf reinstall for installed packages and otherwise localinstall
- more detailed debug output
- system arch no longer hardcoded to x86_64

## 0.3 (2021-12-03)
- add `--list` command to list recent builds
- fix bug in generating kojifiles url from short name
- workarounds for rpmfusion's older koji not supporting patterns
- check if %dist is defined

## 0.2.0 (2021-12-03)
- initial Hackage release
- `--hub` to select hub
- `--pkgsurl` to override kojifiles url
- override `--disttag`
- select specific build with `--nvr` or `--nv`
- `--debug` output

## 0.1.0 (2021-08-12)
- pre-release on copr
- initial options:
  --all/--ask/--base-only/--exclude-devel --dry-run
- only supports Fedora Koji
