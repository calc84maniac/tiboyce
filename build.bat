@echo off
set exit_timeout=-1
set version_arg=
set "as=spasm -E -A"
set "build=build"
@echo on

for /f %%i in ('git describe --tags "--dirty=*" "--abbrev=7"') do set "version_arg="-DVERSION=\"%%i\"""

if not exist "%build%" mkdir "%build%"

%as% %version_arg% %* tiboyce.asm "%build%/TIBoyDat.8xv" || goto exit
%as% %* launcher.asm "%build%/TIBOYCE.8xp" || goto exit
%as% %* skin.asm "%build%/TIBoySkn.8xv" || goto exit

@echo off
echo.
echo Success!
set exit_timeout=5

:exit
@echo off
@REM Pause if not run from cmd so the window persists and the user can see the output.
@REM https://stackoverflow.com/a/18501554
if /I %0 EQU "%~dpnx0" timeout /t %exit_timeout%
exit /b
