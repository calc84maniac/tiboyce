#!/bin/sh
set -e

version=$(git describe --tags --dirty=*)
as="spasm -E -T -L -A"
build="build"

mkdir -p $build

set -x
$as -DVERSION="\"$version\"" tiboyce.asm $build/TIBoyDat.8xv
$as launcher.asm $build/TIBOYCE.8xp
$as skin.asm $build/TIBoySkn.8xv
