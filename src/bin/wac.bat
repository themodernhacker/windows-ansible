@echo off
SETLOCAL EnableDelayedExpansion

REM Get the command and subcommands
SET CMD=%1
SHIFT

REM Handle inventory command
IF "%CMD%"=="inventory" (
  ECHO Running inventory command...
  COPY %~dp0\run_command.sml %TEMP%\run_wac.sml >NUL
  ECHO structure CommandRunner = struct > %TEMP%\run_wac_cmd.sml
  ECHO   fun run [] = print "No arguments provided\n" >> %TEMP%\run_wac_cmd.sml
  ECHO   val _ = run ["inventory", "--list"] >> %TEMP%\run_wac_cmd.sml
  ECHO end; >> %TEMP%\run_wac_cmd.sml
  
  ECHO use "vault.sml"; > %TEMP%\run_wac_full.sml
  ECHO use "active_directory.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "windows_module.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "inventory.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "task_executor.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "playbook.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "main.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "%TEMP%\run_wac_cmd.sml"; >> %TEMP%\run_wac_full.sml
  
  sml < %TEMP%\run_wac_full.sml
  DEL %TEMP%\run_wac.sml >NUL 2>&1
  DEL %TEMP%\run_wac_cmd.sml >NUL 2>&1
  DEL %TEMP%\run_wac_full.sml >NUL 2>&1
  EXIT /B 0
)

REM Handle playbook command
IF "%CMD%"=="playbook" (
  SET PLAYBOOK=%1
  SHIFT
  ECHO Running playbook %PLAYBOOK%...
  
  ECHO use "vault.sml"; > %TEMP%\run_wac_full.sml
  ECHO use "active_directory.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "windows_module.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "inventory.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "task_executor.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "playbook.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "main.sml"; >> %TEMP%\run_wac_full.sml
  ECHO structure CommandRunner = struct >> %TEMP%\run_wac_full.sml
  ECHO   val _ = Main.runPlaybookCommand ({inventory = SOME "inventory.ini", limit = NONE, tags = NONE, verbose = true}, "%PLAYBOOK%") >> %TEMP%\run_wac_full.sml
  ECHO end; >> %TEMP%\run_wac_full.sml
  
  sml < %TEMP%\run_wac_full.sml
  DEL %TEMP%\run_wac_full.sml >NUL 2>&1
  EXIT /B 0
)

REM Handle vault command
IF "%CMD%"=="vault" (
  SET ACTION=%1
  SET FILE=%2
  SHIFT
  SHIFT
  ECHO Running vault %ACTION% on %FILE%...
  
  ECHO use "vault.sml"; > %TEMP%\run_wac_full.sml
  ECHO use "active_directory.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "windows_module.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "inventory.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "task_executor.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "playbook.sml"; >> %TEMP%\run_wac_full.sml
  ECHO use "main.sml"; >> %TEMP%\run_wac_full.sml
  ECHO structure CommandRunner = struct >> %TEMP%\run_wac_full.sml
  ECHO   val _ = Main.runVaultCommand ({vault_file = NONE, password = NONE, verbose = false}, "%ACTION%", "%FILE%") >> %TEMP%\run_wac_full.sml
  ECHO end; >> %TEMP%\run_wac_full.sml
  
  sml < %TEMP%\run_wac_full.sml
  DEL %TEMP%\run_wac_full.sml >NUL 2>&1
  EXIT /B 0
)

ECHO Unknown command: %CMD%
ECHO Usage: wac [inventory^|playbook^|module^|vault] [options]
EXIT /B 1