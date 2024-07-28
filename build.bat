@echo off
set exit_timeout=-1
@echo on

set version_arg=
for /f %%i in ('git describe --tags "--dirty=*" "--abbrev=7"') do set "version_arg="-DVERSION=\"%%i\"""
set "as=spasm -E -T -L -A"
set "build=build"

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
@REM https://superuser.com/a/1688485
if /i not "%CMDCMDLINE:"=%" == "%COMSPEC%" timeout /t %exit_timeout%
exit /b
