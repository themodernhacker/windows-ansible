@echo off
setlocal

cd %~dp0..\src

if "%1"=="" (
    echo Windows Ansible Core (WAC) - Windows-native Ansible alternative
    echo Usage: wac [options] command [args...]
    echo.
    echo For more information, run: wac --help
    exit /b 0
)

sml -m wac.cm %*
endlocal