@echo off
setlocal

if "%1"=="" (
  echo Error: Invalid command. Use: inventory, playbook, module, or vault
  echo Usage: wac [inventory^|playbook^|module^|vault] [options]
  pause
  exit /b 1
)

set CMD=%1
shift

if "%CMD%"=="inventory" goto run_inventory
if "%CMD%"=="playbook" goto run_playbook
if "%CMD%"=="vault" goto run_vault
if "%CMD%"=="module" goto run_module

echo Unknown command: %CMD%
echo Usage: wac [inventory^|playbook^|module^|vault] [options]
pause
exit /b 1

:run_inventory
call "%~dp0inventory.bat" %*
pause
exit /b %ERRORLEVEL%

:run_playbook
call "%~dp0playbook.bat" %*
pause
exit /b %ERRORLEVEL%

:run_vault
call "%~dp0vault.bat" %*
pause
exit /b %ERRORLEVEL%

:run_module
call "%~dp0module.bat" %*
pause
exit /b %ERRORLEVEL%