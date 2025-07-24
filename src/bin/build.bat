@echo off
echo Building Windows Ansible Core (WAC)...
cd %~dp0..\src
sml -m wac.cm

if %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    exit /b %ERRORLEVEL%
)

echo.
echo Build completed successfully!
echo Use wac.bat to run commands