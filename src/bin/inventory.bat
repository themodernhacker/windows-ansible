@echo off
ECHO Running inventory list...

ECHO use "vault.sml"; > %TEMP%\run_inv.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_inv.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_inv.sml
ECHO use "inventory.sml"; >> %TEMP%\run_inv.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_inv.sml
ECHO use "playbook.sml"; >> %TEMP%\run_inv.sml
ECHO use "main.sml"; >> %TEMP%\run_inv.sml

ECHO structure RunInventory = struct >> %TEMP%\run_inv.sml
ECHO   val inventory = Inventory.create() >> %TEMP%\run_inv.sml
ECHO   val hosts = Inventory.list_hosts inventory >> %TEMP%\run_inv.sml
ECHO   val _ = print "All hosts:\n" >> %TEMP%\run_inv.sml
ECHO   val _ = app (fn h => print ("  " ^ h ^ "\n")) hosts >> %TEMP%\run_inv.sml
ECHO end; >> %TEMP%\run_inv.sml

sml < %TEMP%\run_inv.sml
DEL %TEMP%\run_inv.sml >NUL 2>&1