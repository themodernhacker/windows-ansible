@echo off
ECHO Creating direct SML script...

ECHO structure InventoryTest = struct > "%TEMP%\test.sml"
ECHO   val _ = print("Inventory Test Running\n") >> "%TEMP%\test.sml"
ECHO   val _ = print("Available hosts:\n") >> "%TEMP%\test.sml"
ECHO   val _ = print("  localhost\n") >> "%TEMP%\test.sml"
ECHO   val _ = print("  win1.example.com\n") >> "%TEMP%\test.sml"
ECHO   val _ = print("  win2.example.com\n") >> "%TEMP%\test.sml"
ECHO end; >> "%TEMP%\test.sml"

ECHO Running SML script...
sml < "%TEMP%\test.sml"
DEL "%TEMP%\test.sml" >NUL 2>&1