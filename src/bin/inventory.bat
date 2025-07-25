@echo off
echo Running inventory command...

REM [existing code here]

sml < "%TEMP%\inv_run.sml"
del "%TEMP%\inv_run.sml" >nul 2>&1
echo.
echo Press any key to continue...
pause >nul