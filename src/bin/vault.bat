@echo off
SET ACTION=%1
SET FILE=%2
ECHO Running vault %ACTION% on %FILE%...

ECHO use "vault.sml"; > %TEMP%\run_vault.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_vault.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_vault.sml
ECHO use "inventory.sml"; >> %TEMP%\run_vault.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_vault.sml
ECHO use "playbook.sml"; >> %TEMP%\run_vault.sml
ECHO use "main.sml"; >> %TEMP%\run_vault.sml

ECHO structure RunVault = struct >> %TEMP%\run_vault.sml
ECHO   val password = "password123" >> %TEMP%\run_vault.sml
ECHO   val vault = Vault.create("%FILE%.vault", password) >> %TEMP%\run_vault.sml
ECHO   val _ = Vault.set_secret(vault, "test", "secret_value") >> %TEMP%\run_vault.sml
ECHO   val _ = print "Vault created and secret stored.\n" >> %TEMP%\run_vault.sml
ECHO end; >> %TEMP%\run_vault.sml

sml < %TEMP%\run_vault.sml
DEL %TEMP%\run_vault.sml >NUL 2>&1