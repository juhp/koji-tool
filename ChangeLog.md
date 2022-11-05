# Version history of koji-tool

## 0.9.6 (2022-11-05)
- install: completely rework subpackage selection logic
- install: separate --except from --exclude and rename --add to --include
- install: --no-reinstall and --skip-existing skip rpms before rpms selection
- install: show installed states with character symbol prefixes
- tasks: add --hw-info for hw_info.log and --grep to filter logs
- progress: back-off up to 5 times

## 0.9.5 (2022-09-12)
- 'install': fix determination of package name from srpm
- 'progress': correctly determine NVR from srpm again

## 0.9.4 (2022-09-09)
- 'install': rework install logic to combine reinstall and install pkgs
- 'install': do not choose rebuildSRPM subtask
- 'install': show correct top download dir
- 'install': --add to specify additional package
- 'install': change rpms prompt to require Enter for consistency with dnf
- 'progress': another major logic refactor:
   determine poll delay from last-modified time
- 'progress': skip wait for a build.log after [1..6] sec tries
- 'progress': put interval times into task outputs
- 'progress': only print task header if updated task outputs

## 0.9.3 (2022-08-03)
- 'builds' for a package now use specified query options
- 'builds': add --install and --tasks options
- 'install': --exclude globs now filter --package globs
- 'install': update installed subpackages first then install any new subpkgs
- 'install': separate prompt logic for "install above" packages
- 'install': experimental support for rpm-ostree overlaying (--rpm-ostree)
- 'install': allow noarch tasks
- 'install': allow package filtering in --list mode
- 'install': --skip-existing to leave already installed subpackages untouched
- 'progress' records the largest finished build.log size
- 'progress': exclude srpm from max log sizes
- 'progress': show pkg name instead of "unknown" nvr early on
- 'progress': only use current time if task unfinished
- 'tasks': fix output url selection for scratch builds
- 'tasks': check build.log exists before getting its size
- make parsing of states strict for earlier errors
- error if koji userid not found

## 0.9.2 (2022-06-12)
- tasks --install now takes install options string
- install: place rpms in a nvr subdirectory
- install: when downloading check if local files' timestamp within build time
- progress: no longer quit after srpm build
- progress: show duration of build and finished tasks

## 0.9.1 (2022-05-29)
- 'find': add "install", "tail", "notail", and archs support
- 'install': --rpm option to install directly with rpm instead of dnf
- 'tasks': add --install option
- 'tasks': major rework of the log file handling
- 'tasks --tail': fix output when task still running
- 'tasks': compact output shows duration
- 'builds': now outputs packages archive url

## 0.9 (2022-05-20)
- rename 'quick' command to 'find'
- 'builds': compact output now appends url
- 'find': add "details" word

## 0.8.7 (2022-05-14)
'quick' enhancements:
- better help
- support "current" state for ongoing jobs
- --debug flag
- an extra unknown word is now taken to be a package name
- append "\'s" to a username to search for their build tasks

## 0.8.6 (2022-05-13)
- new 'quick' command for a few limited common searchs
- install: --prefix to set subpackage prefix if different to package basename
- install: fix infinite loop when no match for subpackage glob

## 0.8.5 (2022-05-10)
- use rpm-nvr-0.1.2 for better NVR and NVRA parsing ensuring non-empty names
- when parsing koji build request fields, detect git hash and fork builds

## 0.8.4 (2022-05-09)
- Install: completely rework logic using installed status and NVRA etc
- install: '--no-reinstall' replaces '--reinstall-nvrs'
- install: determine package name from child or parent task
- Install: confirm for --all and --exclude, respecting --yes
- tasks: parse taskid as arg and drop --task and --children
- Tasks: print parent Task taskid compactly and then its children

## 0.8.3 (2022-04-23)
- 'latest': new cmd to list latest package build for tag
- 'install': use --reinstall-nvrs to reinstall rpms for current nvr
- 'install': now prompts before proceeding
- 'install': handle build tasks by finding buildArch
- 'install': --list now always lists rpms

## 0.8.2 (2022-03-28)
- use the formatting library for rendering aligned output

## 0.8.1 (2022-03-21)
- query results ordered by taskid or buildid for speed
- 'builds', 'tasks': argument is now a package otherwise use --pattern (replaces --package)
- 'install': add a --yes option to avoid questions (#2)
- print archs appended with '.' prefix

## 0.8 (2022-03-02)
- rename 'query' to 'tasks'
- 'tasks': new '--tail' option to fetch and display tail of build.log
  (shows root.log instead if build.log is too small)
- 'tasks': extend list of methods and list them in --method help
- new 'builds' query command (replaces 'tasks' builds listing)
- only query print date when option given by user
- --debug now displays query options
- 'tasks' and 'builds' now use --hub like 'install'
- new --mine (self-user) option (instead of defaulting to mine)
- 'tasks' and 'builds' support build NVR pattern search (requires Koji 1.24+)
- 'tasks' and 'builds' have --details option otherwise output is compact
  (unless there is a single result)

## 0.7 (2022-01-25)
- query: new options: '--package' to search recent builds and '--latest'
- query --method: now defaults to buildArch (use 'any' to include all)
- query: now defaults to "--before now" (instead of "--from today 00:00")
- query --arch: map i686 to i386 and armv7hl to armhfp
- query: do not drop tasks without string request

## 0.6.1 (2022-01-14)
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
