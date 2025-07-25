@echo off
echo Running %1 command...

echo structure DirectCommand = struct > "%TEMP%\direct.sml"
echo   val _ = print("Running %1 command\n") >> "%TEMP%\direct.sml"
echo   val _ = print("Command arguments: ") >> "%TEMP%\direct.sml"
echo   val _ = print("%*\n") >> "%TEMP%\direct.sml"
echo   val _ = print("\nOperation complete.\n") >> "%TEMP%\direct.sml"
echo end; >> "%TEMP%\direct.sml"

sml < "%TEMP%\direct.sml"
del "%TEMP%\direct.sml" >nul 2>&1