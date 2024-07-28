#!/bin/sh
set -ex

version=$(git describe --tags --dirty=*)
as="spasm -E -T -L -A"
build="build"

mkdir -p "$build"

$as "$@" -DVERSION="\"$version\"" tiboyce.asm "$build/TIBoyDat.8xv"
$as "$@" launcher.asm "$build/TIBOYCE.8xp"
$as "$@" skin.asm "$build/TIBoySkn.8xv"
