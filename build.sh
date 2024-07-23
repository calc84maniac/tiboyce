#!/bin/sh

as="spasm -E -T -L -A"
build="build"

mkdir -p $build

set -x

$as launcher.asm $build/TIBOYCE.8xp
$as tiboyce.asm $build/TIBoyDat.8xv
$as skin.asm $build/TIBoySkn.8xv
