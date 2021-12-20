# Release history for koji-install

## 0.4 (2021-12-20)
- support installing/listing by koji taskid
- selection subpackages with --package and --exclude, which support globbing
- check remote files date/size with http-directory
- listing a task either lists task children or rpms
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
