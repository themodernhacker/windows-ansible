@echo off
SETLOCAL EnableDelayedExpansion

REM Check if arguments provided
IF "%~1"=="" (
  ECHO Error: Invalid command. Use: inventory, playbook, module, or vault
  ECHO Usage: wac [inventory^|playbook^|module^|vault] [options]
  EXIT /B 1
)

SET CMD=%1
SHIFT

IF "%CMD%"=="inventory" (
  CALL %~dp0\inventory.bat %*
  EXIT /B %ERRORLEVEL%
)

IF "%CMD%"=="playbook" (
  CALL %~dp0\playbook.bat %*
  EXIT /B %ERRORLEVEL%
)

IF "%CMD%"=="vault" (
  CALL %~dp0\vault.bat %*
  EXIT /B %ERRORLEVEL%
)

IF "%CMD%"=="module" (
  CALL %~dp0\module.bat %*
  EXIT /B %ERRORLEVEL%
)

ECHO Unknown command: %CMD%
ECHO Usage: wac [inventory^|playbook^|module^|vault] [options]
EXIT /B 1