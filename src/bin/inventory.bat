@echo off
ECHO Running inventory command...

SET ACTION=--list
SET INV_PATH=inventory.ini

:parse_args
IF "%~1"=="" GOTO execute
IF "%~1"=="--list" (
  SET ACTION=--list
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--host" (
  SET ACTION=--host
  SET HOST=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--inventory" (
  SET INV_PATH=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
SHIFT
GOTO parse_args

:execute
ECHO use "vault.sml"; > %TEMP%\run_inv.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_inv.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_inv.sml
ECHO use "inventory.sml"; >> %TEMP%\run_inv.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_inv.sml
ECHO use "playbook.sml"; >> %TEMP%\run_inv.sml
ECHO use "main.sml"; >> %TEMP%\run_inv.sml

ECHO structure RunInventory = struct >> %TEMP%\run_inv.sml
ECHO   val inv_path = "%INV_PATH%" >> %TEMP%\run_inv.sml
ECHO   val _ = print("Loading inventory from " ^ inv_path ^ "...\n") >> %TEMP%\run_inv.sml
ECHO   val inventory = Inventory.from_file inv_path >> %TEMP%\run_inv.sml

IF "%ACTION%"=="--list" (
  ECHO   val hosts = Inventory.list_hosts inventory >> %TEMP%\run_inv.sml
  ECHO   val _ = print "All hosts:\n" >> %TEMP%\run_inv.sml
  ECHO   val _ = app (fn h => print ("  " ^ h ^ "\n")) hosts >> %TEMP%\run_inv.sml
) ELSE (
  ECHO   val host_info = Inventory.host_vars inventory "%HOST%" >> %TEMP%\run_inv.sml
  ECHO   val _ = print("Host: %HOST%\n") >> %TEMP%\run_inv.sml
  ECHO   val _ = app (fn (k,v) => print ("  " ^ k ^ ": " ^ v ^ "\n")) host_info >> %TEMP%\run_inv.sml
)

ECHO end; >> %TEMP%\run_inv.sml

sml < %TEMP%\run_inv.sml
DEL %TEMP%\run_inv.sml >NUL 2>&1