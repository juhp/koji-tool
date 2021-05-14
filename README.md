# koji-install

A CLI tool for downloading and install rpms from Fedora koji.

## Usage

$ koji-install podman

Will download the latest build for your Fedora version,
and try to install it.  Use `--all` to install all subpackages
otherwise it will ask for each one.

## Installation

`stack install`, `cabal-rpm install`, or `cabal install`.
