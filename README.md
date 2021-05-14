# koji-install

A CLI tool for downloading and install rpms from Fedora koji.

## Usage

$ koji-install podman

Will download the latest build for your Fedora version,
and try to install it.

### Options

`--all`: install all subpackages

`--ask`: ask about each subpackage

`--base`: only install base package

`--exclude-devel`: skip devel subpackages

## Installation

`stack install`, `cabal-rpm install`, or `cabal install`.
