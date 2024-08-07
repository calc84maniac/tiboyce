#!/bin/bash
as="spasm -E -A"
build="build"
set -x

version_arg="-DVERSION=\"$(git describe --tags --dirty=* --abbrev=7)\"" || unset version_arg
set -e

mkdir -p "$build"

$as ${version_arg+"$version_arg"} "$@" tiboyce.asm "$build/TIBoyDat.8xv"
$as "$@" launcher.asm "$build/TIBOYCE.8xp"
$as "$@" skin.asm "$build/TIBoySkn.8xv"
