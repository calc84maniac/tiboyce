@echo off

set as=spasm -E -T -L -A
set build=build

if not exist %build% mkdir %build%

@echo on

%as% launcher.asm %build%/TIBOYCE.8xp
%as% tiboyce.asm %build%/TIBoyDat.8xv
%as% skin.asm %build%/TIBoySkn.8xv
