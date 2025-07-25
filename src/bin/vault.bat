@echo off
IF "%~1"=="" (
  ECHO Error: Missing vault action
  ECHO Usage: vault [create^|encrypt^|decrypt^|view^|set] FILE [--vault-file PATH] [--password PWD]
  EXIT /B 1
)

SET ACTION=%1
SET FILE=%2
SET VAULT_FILE=
SET PASSWORD=
SET VERBOSE=false

SHIFT
SHIFT

:parse_args
IF "%~1"=="" GOTO execute
IF "%~1"=="--vault-file" (
  SET VAULT_FILE=%~2
  SHIFT
  SHIFT
  GOTO parse_args
)
IF "%~1"=="--password" (
  SET PASSWORD=%~2
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
SET FILE=%FILE:\=/%
IF NOT "%VAULT_FILE%"=="" SET VAULT_FILE=%VAULT_FILE:\=/%

ECHO use "vault.sml"; > %TEMP%\run_vault.sml
ECHO use "active_directory.sml"; >> %TEMP%\run_vault.sml
ECHO use "windows_module.sml"; >> %TEMP%\run_vault.sml
ECHO use "inventory.sml"; >> %TEMP%\run_vault.sml
ECHO use "task_executor.sml"; >> %TEMP%\run_vault.sml
ECHO use "playbook.sml"; >> %TEMP%\run_vault.sml
ECHO use "main.sml"; >> %TEMP%\run_vault.sml

ECHO structure RunVault = struct >> %TEMP%\run_vault.sml

IF "%ACTION%"=="create" (
  ECHO   val _ = print "Vault password: " >> %TEMP%\run_vault.sml
  ECHO   val password = valOf (TextIO.inputLine TextIO.stdIn) >> %TEMP%\run_vault.sml
  ECHO   val vault = Vault.create("%FILE%", password) >> %TEMP%\run_vault.sml
  ECHO   val _ = Vault.save vault >> %TEMP%\run_vault.sml
) ELSE IF "%ACTION%"=="encrypt" (
  ECHO   val _ = print "Vault password: " >> %TEMP%\run_vault.sml
  ECHO   val password = valOf (TextIO.inputLine TextIO.stdIn) >> %TEMP%\run_vault.sml
  ECHO   val vault = Vault.create("%FILE%.vault", password) >> %TEMP%\run_vault.sml
  ECHO   val _ = Vault.encrypt_file vault "%FILE%" >> %TEMP%\run_vault.sml
  ECHO   val _ = print ("File encrypted to " ^ "%FILE%.vault" ^ "\n") >> %TEMP%\run_vault.sml
) ELSE IF "%ACTION%"=="decrypt" (
  ECHO   val _ = print "Vault password: " >> %TEMP%\run_vault.sml
  ECHO   val password = valOf (TextIO.inputLine TextIO.stdIn) >> %TEMP%\run_vault.sml
  ECHO   val vault = Vault.open_vault("%FILE%", password) >> %TEMP%\run_vault.sml
  ECHO   val decrypted_file = String.substring("%FILE%", 0, String.size "%FILE%" - 6) >> %TEMP%\run_vault.sml
  ECHO   val _ = Vault.decrypt_file vault "%FILE%" decrypted_file >> %TEMP%\run_vault.sml
  ECHO   val _ = print ("File decrypted to " ^ decrypted_file ^ "\n") >> %TEMP%\run_vault.sml
) ELSE IF "%ACTION%"=="view" (
  ECHO   val _ = print "Vault password: " >> %TEMP%\run_vault.sml
  ECHO   val password = valOf (TextIO.inputLine TextIO.stdIn) >> %TEMP%\run_vault.sml
  ECHO   val vault = Vault.open_vault("%FILE%", password) >> %TEMP%\run_vault.sml
  ECHO   val content = Vault.get_secret(vault, "%FILE%") >> %TEMP%\run_vault.sml
  ECHO   val _ = print (content ^ "\n") >> %TEMP%\run_vault.sml
) ELSE IF "%ACTION%"=="set" (
  ECHO   val _ = print "Vault password: " >> %TEMP%\run_vault.sml
  ECHO   val password = valOf (TextIO.inputLine TextIO.stdIn) >> %TEMP%\run_vault.sml
  ECHO   val vault = if "%VAULT_FILE%"="" then Vault.create("%FILE%.vault", password) >> %TEMP%\run_vault.sml
  ECHO               else Vault.open_vault("%VAULT_FILE%", password) >> %TEMP%\run_vault.sml
  ECHO   val _ = print "Value: " >> %TEMP%\run_vault.sml
  ECHO   val value = valOf (TextIO.inputLine TextIO.stdIn) >> %TEMP%\run_vault.sml
  ECHO   val _ = Vault.set_secret(vault, "%FILE%", value) >> %TEMP%\run_vault.sml
  ECHO   val _ = Vault.save vault >> %TEMP%\run_vault.sml
  ECHO   val _ = print "Secret stored\n" >> %TEMP%\run_vault.sml
)

ECHO end; >> %TEMP%\run_vault.sml

sml < %TEMP%\run_vault.sml
DEL %TEMP%\run_vault.sml >NUL 2>&1