@echo off
IF "%~1"=="" (
  ECHO Error: Missing playbook file
  ECHO Usage: playbook PLAYBOOK_FILE [--inventory INV_PATH] [--limit HOSTS] [--tags TAGS]
  EXIT /B 1
)

SET PLAYBOOK=%~1
SET INV_PATH=inventory.ini
SET VERBOSE=false
SET LIMIT=
SET TAGS=

SHIFT
:parse_args
IF "%~1"=="" GOTO execute
IF "%~1"=="--inventory" (
  SET INV_PATH=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--limit" (
  SET LIMIT=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--tags" (
  SET TAGS=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--verbose" (
  SET VERBOSE=true
  SHIFT
  GOTO parse_args
)
SHIFT
GOTO parse_args

:execute
REM Convert backslashes to forward slashes in path
SET PLAYBOOK=%PLAYBOOK:\=/%

ECHO use "vault.sml"; > %TEMP%\run_playbook.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_playbook.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_playbook.sml
ECHO use "inventory.sml"; >> %TEMP%\run_playbook.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_playbook.sml
ECHO use "playbook.sml"; >> %TEMP%\run_playbook.sml
ECHO use "main.sml"; >> %TEMP%\run_playbook.sml

ECHO structure RunPlaybook = struct >> %TEMP%\run_playbook.sml
ECHO   val inv_path = "%INV_PATH%" >> %TEMP%\run_playbook.sml
ECHO   val playbook_path = "%PLAYBOOK%" >> %TEMP%\run_playbook.sml
ECHO   val _ = print("Loading inventory from " ^ inv_path ^ "...\n") >> %TEMP%\run_playbook.sml
ECHO   val _ = print("Loading playbook from " ^ playbook_path ^ "...\n") >> %TEMP%\run_playbook.sml
ECHO   val inventory = Inventory.from_file inv_path >> %TEMP%\run_playbook.sml
ECHO   val playbook = Playbook.load_file playbook_path >> %TEMP%\run_playbook.sml
ECHO   val opts = {verbose = %VERBOSE% >> %TEMP%\run_playbook.sml

IF NOT "%LIMIT%"=="" (
  ECHO              , limit = SOME "%LIMIT%" >> %TEMP%\run_playbook.sml
) ELSE (
  ECHO              , limit = NONE >> %TEMP%\run_playbook.sml
)

IF NOT "%TAGS%"=="" (
  ECHO              , tags = SOME ["%TAGS%"] >> %TEMP%\run_playbook.sml
) ELSE (
  ECHO              , tags = NONE >> %TEMP%\run_playbook.sml
)

ECHO              } >> %TEMP%\run_playbook.sml
ECHO   val results = Playbook.execute(playbook, inventory, opts) >> %TEMP%\run_playbook.sml
ECHO   val _ = print ("Playbook executed: " ^ Int.toString (length results) ^ " plays\n") >> %TEMP%\run_playbook.sml
ECHO end; >> %TEMP%\run_playbook.sml

sml < %TEMP%\run_playbook.sml
DEL %TEMP%\run_playbook.sml >NUL 2>&1