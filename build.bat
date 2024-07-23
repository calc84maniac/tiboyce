@echo off

for /f %%i in ('git describe --tags') do set "version=%%i" && goto version_ok
exit /b
:version_ok

set "as=spasm -E -T -L -A"
set "build=build"

if not exist %build% mkdir %build%

@echo on

%as% -DVERSION=\"%version%\" tiboyce.asm %build%/TIBoyDat.8xv || exit /b
%as% launcher.asm %build%/TIBOYCE.8xp || exit /b
%as% skin.asm %build%/TIBoySkn.8xv || exit /b
