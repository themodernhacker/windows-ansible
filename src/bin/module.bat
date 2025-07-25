@echo off
SET MODULE=win_command
SET HOST=localhost
SET ARGS=
SET INV_PATH=inventory.ini
SET VERBOSE=false

:parse_args
IF "%~1"=="" GOTO execute
IF "%~1"=="--module" (
  SET MODULE=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--host" (
  SET HOST=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--args" (
  SET ARGS=%~2
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
IF "%~1"=="--verbose" (
  SET VERBOSE=true
  SHIFT
  GOTO parse_args
)
SHIFT
GOTO parse_args

:execute
ECHO use "vault.sml"; > %TEMP%\run_module.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_module.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_module.sml
ECHO use "inventory.sml"; >> %TEMP%\run_module.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_module.sml
ECHO use "playbook.sml"; >> %TEMP%\run_module.sml
ECHO use "main.sml"; >> %TEMP%\run_module.sml

ECHO structure RunModule = struct >> %TEMP%\run_module.sml
ECHO   val inv_path = "%INV_PATH%" >> %TEMP%\run_module.sml
ECHO   val _ = print("Loading inventory from " ^ inv_path ^ "...\n") >> %TEMP%\run_module.sml
ECHO   val inventory = Inventory.from_file inv_path >> %TEMP%\run_module.sml
ECHO   val module_name = "%MODULE%" >> %TEMP%\run_module.sml
ECHO   val args = "%ARGS%" >> %TEMP%\run_module.sml
ECHO   val _ = print("Running module " ^ module_name ^ " on %HOST%...\n") >> %TEMP%\run_module.sml
ECHO   val module = WindowsModule.get module_name >> %TEMP%\run_module.sml
ECHO   val result = TaskExecutor.run_module module args "%HOST%" inventory >> %TEMP%\run_module.sml
ECHO   val status = TaskExecutor.status_string result >> %TEMP%\run_module.sml
ECHO   val _ = print("Host: %HOST% => " ^ status ^ "\n") >> %TEMP%\run_module.sml
ECHO   val _ = if %VERBOSE% then print("Output: " ^ TaskExecutor.output result ^ "\n") else () >> %TEMP%\run_module.sml
ECHO end; >> %TEMP%\run_module.sml

sml < %TEMP%\run_module.sml
DEL %TEMP%\run_module.sml >NUL 2>&1