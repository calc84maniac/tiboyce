#!/bin/bash
set -x

version_arg="-DVERSION=\"$(git describe --tags --dirty=*)\"" || unset version_arg
set -e
as="spasm -E -T -L -A"
build="build"

mkdir -p "$build"

$as ${version_arg+"$version_arg"} "$@" tiboyce.asm "$build/TIBoyDat.8xv"
$as "$@" launcher.asm "$build/TIBOYCE.8xp"
$as "$@" skin.asm "$build/TIBoySkn.8xv"
