@echo off
echo Building Windows Ansible Core (WAC)...

REM Go to the src directory (parent of bin)
cd %~dp0..

REM Check if we're in the right directory
if not exist wac.cm (
    echo Error: Cannot find wac.cm file.
    echo Make sure all source files are in the correct location.
    exit /b 1
)

REM Run SML with the full path if needed
if exist "C:\Program Files\SMLNJ\bin\sml.bat" (
    "C:\Program Files\SMLNJ\bin\sml.bat" -m wac.cm
) else (
    sml -m wac.cm
)

if %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    exit /b %ERRORLEVEL%
)

echo.
echo Build completed successfully!
echo Use wac.bat to run commands