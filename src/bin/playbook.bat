@echo off
SET PLAYBOOK=%1
ECHO Running playbook %PLAYBOOK%...

ECHO use "vault.sml"; > %TEMP%\run_playbook.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_playbook.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_playbook.sml
ECHO use "inventory.sml"; >> %TEMP%\run_playbook.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_playbook.sml
ECHO use "playbook.sml"; >> %TEMP%\run_playbook.sml
ECHO use "main.sml"; >> %TEMP%\run_playbook.sml

ECHO structure RunPlaybook = struct >> %TEMP%\run_playbook.sml
ECHO   val inventory = Inventory.create() >> %TEMP%\run_playbook.sml
ECHO   val playbook = Playbook.load_file "%PLAYBOOK%" >> %TEMP%\run_playbook.sml
ECHO   val results = Playbook.execute(playbook, inventory) >> %TEMP%\run_playbook.sml
ECHO   val _ = print ("Playbook executed: " ^ Int.toString (length results) ^ " plays\n") >> %TEMP%\run_playbook.sml
ECHO end; >> %TEMP%\run_playbook.sml

sml < %TEMP%\run_playbook.sml
DEL %TEMP%\run_playbook.sml >NUL 2>&1