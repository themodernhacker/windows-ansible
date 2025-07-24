(* windows_module.sml
 * Implementation of Windows-specific Ansible modules
 *)

structure WindowsModule : WINDOWS_MODULE = struct
  (* Module result type - matches Ansible's return format *)
  type module_result = {
    changed: bool,
    failed: bool,
    msg: string,
    rc: int option,
    stdout: string option,
    stderr: string option,
    output: (string * string) list
  }
  
  (* Helper function to execute PowerShell commands *)
  fun execute_powershell cmd =
    let
      val temp_out = OS.FileSys.tmpName()
      val temp_err = OS.FileSys.tmpName()
      
      (* Construct the command to execute with output redirection *)
      val ps_cmd = "powershell -NonInteractive -NoProfile -ExecutionPolicy Bypass -Command \"" ^ 
                   cmd ^ "\" > \"" ^ temp_out ^ "\" 2> \"" ^ temp_err ^ "\""
      
      (* Execute the command and capture return code *)
      val process_status = OS.Process.system ps_cmd
      val rc = if OS.Process.isSuccess process_status then 0 else 1
      
      (* Read the output and error files *)
      val stdout = TextIO.inputAll(TextIO.openIn temp_out)
                   handle _ => ""
      val stderr = TextIO.inputAll(TextIO.openIn temp_err)
                   handle _ => ""
                   
      (* Clean up temporary files *)
      val _ = (OS.FileSys.remove temp_out; OS.FileSys.remove temp_err)
               handle _ => ()
    in
      (rc, stdout, stderr)
    end
    
  (* Create a failed result *)
  fun failed_result msg =
    {
      changed = false,
      failed = true,
      msg = msg,
      rc = SOME 1,
      stdout = NONE,
      stderr = SOME msg,
      output = []
    }
    
  (* Check if a file exists *)
  fun file_exists path =
    OS.FileSys.access(path, [])
    handle _ => false
  
  (* Implementation of win_copy module *)
  fun win_copy {src, dest, backup} =
    let
      val dest_exists = file_exists dest
      val backup_suffix = if backup then ".bak" else ""
      
      (* Check if source file exists *)
      val _ = if not (file_exists src) then
                raise OS.SysErr("Source file not found: " ^ src, NONE)
              else ()
              
      (* Create backup if needed *)
      val _ = if backup andalso dest_exists then
                OS.FileSys.rename {old = dest, new = dest ^ backup_suffix}
              else ()
      
      (* Check if files are identical *)
      fun files_identical () =
        let
          val cmd = "$srcContent = Get-Content -Path '" ^ src ^ 
                    "' -Raw -Encoding Byte; " ^
                    "$destContent = Get-Content -Path '" ^ dest ^ 
                    "' -Raw -Encoding Byte; " ^
                    "if ($srcContent -eq $destContent) { 'identical' } else { 'different' }"
          val (rc, stdout, _) = execute_powershell cmd
        in
          rc = 0 andalso String.isSubstring "identical" stdout
        end
        
      (* Only check if files are identical if destination exists *)
      val needs_copy = not dest_exists orelse not (files_identical())
      
      (* Perform the copy if needed *)
      val (rc, stdout, stderr) = 
        if needs_copy then
          execute_powershell ("Copy-Item -Path '" ^ src ^ 
                             "' -Destination '" ^ dest ^ 
                             "' -Force")
        else
          (0, "File unchanged", "")
    in
      {
        changed = needs_copy,
        failed = rc <> 0,
        msg = if rc = 0 then 
                if needs_copy then "File copied successfully" 
                else "File unchanged"
              else "Copy failed",
        rc = SOME rc,
        stdout = SOME stdout,
        stderr = SOME stderr,
        output = [("src", src), ("dest", dest)]
      }
    end
    handle e => failed_result(exnMessage e)
  
  (* Implementation of win_command module *)
  fun win_command {command, chdir, creates, removes} =
    let
      (* Check 'creates' condition - skip if file exists *)
      val skip_creates = case creates of
                          SOME path => file_exists path
                        | NONE => false
                        
      (* Check 'removes' condition - skip if file doesn't exist *)
      val skip_removes = case removes of
                          SOME path => not (file_exists path)
                        | NONE => false
                        
      (* Skip command if conditions say so *)
      val should_skip = skip_creates orelse skip_removes
      
      (* Build the command with optional directory change *)
      val full_cmd = case chdir of
                      SOME dir => "Push-Location -Path '" ^ dir ^ "'; " ^ 
                                 "try { " ^ command ^ " } " ^ 
                                 "finally { Pop-Location }"
                    | NONE => command
    in
      if should_skip then
        {
          changed = false,
          failed = false,
          msg = "Skipped due to creates/removes condition",
          rc = SOME 0,
          stdout = SOME "",
          stderr = SOME "",
          output = []
        }
      else
        let
          val (rc, stdout, stderr) = execute_powershell full_cmd
        in
          {
            changed = true,  (* Commands always report changed *)
            failed = rc <> 0,
            msg = if rc = 0 then "Command executed successfully" 
                  else "Command execution failed",
            rc = SOME rc,
            stdout = SOME stdout,
            stderr = SOME stderr,
            output = [("cmd", command)]
          }
        end
    end
  
  (* Implementation of win_shell module *)
  fun win_shell {command, chdir, creates, removes} =
    let
      (* Check conditions same as win_command *)
      val skip_creates = case creates of
                          SOME path => file_exists path
                        | NONE => false
                        
      val skip_removes = case removes of
                          SOME path => not (file_exists path)
                        | NONE => false
                        
      val should_skip = skip_creates orelse skip_removes
      
      (* Use cmd.exe shell instead of PowerShell *)
      val shell_cmd = case chdir of
                       SOME dir => "cd /d " ^ dir ^ " && " ^ command
                     | NONE => command
      
      val temp_out = OS.FileSys.tmpName()
      val temp_err = OS.FileSys.tmpName()
    in
      if should_skip then
        {
          changed = false,
          failed = false,
          msg = "Skipped due to creates/removes condition",
          rc = SOME 0,
          stdout = SOME "",
          stderr = SOME "",
          output = []
        }
      else
        let
          (* Execute using cmd.exe shell *)
          val cmd = "cmd.exe /c \"" ^ shell_cmd ^ "\" > \"" ^ 
                    temp_out ^ "\" 2> \"" ^ temp_err ^ "\""
          val process_status = OS.Process.system cmd
          val rc = if OS.Process.isSuccess process_status then 0 else 1
          
          (* Read output files *)
          val stdout = TextIO.inputAll(TextIO.openIn temp_out)
                       handle _ => ""
          val stderr = TextIO.inputAll(TextIO.openIn temp_err)
                       handle _ => ""
                       
          (* Clean up *)
          val _ = (OS.FileSys.remove temp_out; OS.FileSys.remove temp_err)
                   handle _ => ()
        in
          {
            changed = true,  (* Shell commands always report changed *)
            failed = rc <> 0,
            msg = if rc = 0 then "Shell command executed successfully" 
                  else "Shell command execution failed",
            rc = SOME rc,
            stdout = SOME stdout,
            stderr = SOME stderr,
            output = [("cmd", command)]
          }
        end
    end
  
  (* Implementation of win_service module *)
  fun win_service {name, display_name, path, state, startup_mode} =
    let
      (* Check if service exists *)
      val check_cmd = "Get-Service -Name '" ^ name ^ "' -ErrorAction SilentlyContinue | " ^
                      "Select-Object -ExpandProperty Name"
      val (check_rc, check_out, _) = execute_powershell check_cmd
      val service_exists = check_rc = 0 andalso String.size check_out > 0
      
      (* Get current state if service exists *)
      val get_state_cmd = "Get-Service -Name '" ^ name ^ "' | " ^
                         "Select-Object -ExpandProperty Status"
      val (_, current_state_str, _) = 
        if service_exists then execute_powershell get_state_cmd
        else (0, "", "")
        
      (* Convert state strings *)
      val current_state = 
        if String.isSubstring "Running" current_state_str then "started"
        else "stopped"
      
      (* Determine if change is needed *)
      val need_state_change = 
        case state of
          SOME s => service_exists andalso s <> current_state
        | NONE => false
        
      (* Execute state change if needed *)
      val (state_rc, state_out, state_err) = 
        case (need_state_change, state) of
          (true, SOME "started") => 
            execute_powershell ("Start-Service -Name '" ^ name ^ "'")
          (true, SOME "stopped") => 
            execute_powershell ("Stop-Service -Name '" ^ name ^ "'")
          (true, SOME "restarted") => 
            execute_powershell ("Restart-Service -Name '" ^ name ^ "'")
          _ => (0, "", "")
          
      (* Create service if it doesn't exist and path is provided *)
      val (create_rc, create_out, create_err) = 
        if not service_exists andalso isSome path then
          let
            val create_cmd = "New-Service -Name '" ^ name ^ "'" ^
                            (case display_name of
                               SOME dname => " -DisplayName '" ^ dname ^ "'"
                             | NONE => "") ^
                            " -BinaryPathName '" ^ valOf path ^ "'" ^
                            (case startup_mode of
                               SOME "auto" => " -StartupType Automatic"
                             | SOME "manual" => " -StartupType Manual"
                             | SOME "disabled" => " -StartupType Disabled"
                             | _ => "")
          in
            execute_powershell create_cmd
          end
        else (0, "", "")
        
      (* Set startup mode if specified *)
      val (startup_rc, startup_out, startup_err) = 
        case (service_exists, startup_mode) of
          (true, SOME mode) => 
            let
              val mode_str = 
                case mode of
                  "auto" => "Automatic"
                | "manual" => "Manual"
                | "disabled" => "Disabled"
                | _ => "Manual"
                
              val startup_cmd = 
                "Set-Service -Name '" ^ name ^ "' -StartupType " ^ mode_str
            in
              execute_powershell startup_cmd
            end
          _ => (0, "", "")
          
      (* Determine if any changes were made *)
      val changed = need_state_change orelse 
                    (not service_exists andalso isSome path) orelse
                    startup_rc = 0 andalso String.size startup_out > 0
                    
      (* Determine if any operation failed *)
      val failed = (need_state_change andalso state_rc <> 0) orelse
                   (not service_exists andalso isSome path andalso create_rc <> 0) orelse
                   (isSome startup_mode andalso startup_rc <> 0)
    in
      {
        changed = changed,
        failed = failed,
        msg = if failed then "Service operation failed"
              else if changed then "Service changed"
              else "Service unchanged",
        rc = SOME (if failed then 1 else 0),
        stdout = SOME (state_out ^ create_out ^ startup_out),
        stderr = SOME (state_err ^ create_err ^ startup_err),
        output = [("name", name), 
                  ("exists", if service_exists then "true" else "false"),
                  ("state", current_state)]
      }
    end
  
  (* Implementation of win_regedit module *)
  fun win_regedit {path, name, data, type_, state} =
    let
      (* Check if registry value exists *)
      val check_cmd = "if (Test-Path -Path '" ^ path ^ "') { " ^
                      "  $item = Get-ItemProperty -Path '" ^ path ^ 
                      "' -Name '" ^ name ^ "' -ErrorAction SilentlyContinue; " ^
                      "  if ($item -ne $null) { 'exists' } else { 'notexists' } " ^
                      "} else { 'pathnotexists' }"
      val (check_rc, check_out, _) = execute_powershell check_cmd
      val value_exists = check_rc = 0 andalso String.isSubstring "exists" check_out
      
      (* Create or remove based on state *)
      val (op_rc, op_out, op_err) = 
        case (state, value_exists) of
          ("present", true) => 
            (* Update existing value *)
            if isSome data then
              let
                val type_str = case type_ of
                                SOME "string" => "String"
                              | SOME "dword" => "DWord"
                              | SOME "binary" => "Binary"
                              | _ => "String"
                               
                val cmd = "Set-ItemProperty -Path '" ^ path ^ 
                         "' -Name '" ^ name ^ "' -Value '" ^ valOf data ^ 
                         "' -Type " ^ type_str
              in
                execute_powershell cmd
              end
            else (0, "No data to update", "")
          ("present", false) =>
            (* Create new value *)
            if isSome data then
              let
                (* Ensure path exists *)
                val ensure_path = "if (-not (Test-Path -Path '" ^ path ^ "')) { " ^
                                 "  New-Item -Path '" ^ path ^ "' -Force | Out-Null " ^
                                 "}"
                val (path_rc, _, _) = execute_powershell ensure_path
                
                (* Only continue if path creation succeeded *)
                val _ = if path_rc <> 0 then
                          raise OS.SysErr("Failed to create registry path", NONE)
                        else ()
                        
                val type_str = case type_ of
                                SOME "string" => "String"
                              | SOME "dword" => "DWord"
                              | SOME "binary" => "Binary"
                              | _ => "String"
                               
                val cmd = "New-ItemProperty -Path '" ^ path ^ 
                         "' -Name '" ^ name ^ "' -Value '" ^ valOf data ^ 
                         "' -PropertyType " ^ type_str
              in
                execute_powershell cmd
              end
            else (0, "No data to create", "")
          ("absent", true) =>
            (* Remove existing value *)
            let
              val cmd = "Remove-ItemProperty -Path '" ^ path ^ 
                       "' -Name '" ^ name ^ "'"
            in
              execute_powershell cmd
            end
          ("absent", false) =>
            (* Already absent *)
            (0, "Value already absent", "")
          (_, _) =>
            (* Invalid state *)
            (1, "", "Invalid state specified: must be 'present' or 'absent'")
            
      (* Determine if changes were made *)
      val changed = (state = "present" andalso not value_exists andalso isSome data) orelse
                    (state = "absent" andalso value_exists) orelse
                    (state = "present" andalso value_exists andalso 
                     isSome data andalso op_rc = 0 andalso 
                     not (String.isSubstring "No data to update" op_out))
    in
      {
        changed = changed,
        failed = op_rc <> 0,
        msg = if op_rc <> 0 then "Registry operation failed"
              else if changed then "Registry value changed"
              else "Registry value unchanged",
        rc = SOME op_rc,
        stdout = SOME op_out,
        stderr = SOME op_err,
        output = [("path", path), 
                  ("name", name), 
                  ("state", state),
                  ("exists", if value_exists then "true" else "false")]
      }
    end
  
  (* Implementation of win_feature module *)
  fun win_feature {name, state, restart_if_required} =
    let
      (* Check if feature is installed *)
      val check_cmd = "if (Get-Command Get-WindowsFeature -ErrorAction SilentlyContinue) { " ^
                     "  Get-WindowsFeature -Name '" ^ name ^ "' | " ^
                     "  Select-Object -ExpandProperty Installed " ^
                     "} else { " ^
                     "  Get-WindowsOptionalFeature -Online -FeatureName '" ^ name ^ "' | " ^
                     "  Select-Object -ExpandProperty State " ^
                     "}"
      val (check_rc, check_out, _) = execute_powershell check_cmd
      
      (* Parse output to determine if installed *)
      val is_installed = check_rc = 0 andalso 
                        (String.isSubstring "True" check_out orelse
                         String.isSubstring "Enabled" check_out)
      
      (* Determine if change is needed *)
      val need_change = (state = "present" andalso not is_installed) orelse
                       (state = "absent" andalso is_installed)
                       
      (* Execute change if needed *)
      val (op_rc, op_out, op_err) = 
        if need_change then
          if state = "present" then
            (* Install feature *)
            let
              val cmd = "if (Get-Command Add-WindowsFeature -ErrorAction SilentlyContinue) { " ^
                       "  Add-WindowsFeature -Name '" ^ name ^ "' -Restart:$" ^ 
                       (if restart_if_required then "true" else "false") ^ " | " ^
                       "  Select-Object -ExpandProperty RestartNeeded " ^
                       "} else { " ^
                       "  Enable-WindowsOptionalFeature -Online -FeatureName '" ^ name ^ 
                       "' -NoRestart:$" ^ (if restart_if_required then "false" else "true") ^
                       " | Select-Object -ExpandProperty RestartNeeded " ^
                       "}"
            in
              execute_powershell cmd
            end
          else
            (* Remove feature *)
            let
              val cmd = "if (Get-Command Remove-WindowsFeature -ErrorAction SilentlyContinue) { " ^
                       "  Remove-WindowsFeature -Name '" ^ name ^ "' -Restart:$" ^ 
                       (if restart_if_required then "true" else "false") ^ " | " ^
                       "  Select-Object -ExpandProperty RestartNeeded " ^
                       "} else { " ^
                       "  Disable-WindowsOptionalFeature -Online -FeatureName '" ^ name ^ 
                       "' -NoRestart:$" ^ (if restart_if_required then "false" else "true") ^
                       " | Select-Object -ExpandProperty RestartNeeded " ^
                       "}"
            in
              execute_powershell cmd
            end
        else
          (0, "", "")
      
      (* Check if restart is needed *)
      val restart_needed = op_rc = 0 andalso String.isSubstring "True" op_out
    in
      {
        changed = need_change andalso op_rc = 0,
        failed = need_change andalso op_rc <> 0,
        msg = if need_change then
                if op_rc = 0 then
                  if state = "present" then "Feature installed"
                  else "Feature removed"
                else "Feature operation failed"
              else "Feature already in desired state",
        rc = SOME op_rc,
        stdout = SOME op_out,
        stderr = SOME op_err,
        output = [("name", name), 
                  ("state", state),
                  ("installed", if is_installed then "true" else "false"),
                  ("restart_needed", if restart_needed then "true" else "false")]
      }
    end
  
  (* Implementation of win_file module *)
  fun win_file {path, state, attributes} =
    let
      (* Check if path exists and its type *)
      val check_cmd = "if (Test-Path -Path '" ^ path ^ "') { " ^
                     "  if (Test-Path -Path '" ^ path ^ "' -PathType Container) { " ^
                     "    'directory' " ^
                     "  } else { " ^
                     "    'file' " ^
                     "  } " ^
                     "} else { " ^
                     "  'absent' " ^
                     "}"
      val (check_rc, check_out, _) = execute_powershell check_cmd
      val current_state = String.substring(check_out, 0, 
                                          Int.min(String.size check_out, 9))
      
      (* Execute change based on desired state *)
      val (op_rc, op_out, op_err) = 
        case (current_state, state) of
          ("absent", "directory") => 
            (* Create directory *)
            execute_powershell ("New-Item -Path '" ^ path ^ "' -ItemType Directory -Force")
          ("absent", "file") => 
            (* Create empty file *)
            execute_powershell ("New-Item -Path '" ^ path ^ "' -ItemType File -Force")
          ("file", "directory") => 
            (* Can't convert file to directory - remove first *)
            (execute_powershell ("Remove-Item -Path '" ^ path ^ "' -Force");
             execute_powershell ("New-Item -Path '" ^ path ^ "' -ItemType Directory -Force"))
          ("directory", "file") => 
            (* Can't convert directory to file - remove first *)
            (execute_powershell ("Remove-Item -Path '" ^ path ^ "' -Force -Recurse");
             execute_powershell ("New-Item -Path '" ^ path ^ "' -ItemType File -Force"))
          (_, "absent") => 
            (* Remove path *)
            if current_state = "directory" then
              execute_powershell ("Remove-Item -Path '" ^ path ^ "' -Force -Recurse")
            else if current_state = "file" then
              execute_powershell ("Remove-Item -Path '" ^ path ^ "' -Force")
            else
              (0, "Already absent", "")
          (_, _) => 
            (* No change in type needed *)
            (0, "", "")
      
      (* Apply attributes if specified *)
      val (attr_rc, attr_out, attr_err) = 
        case attributes of
          SOME attrs if op_rc = 0 andalso current_state <> "absent" andalso state <> "absent" =>
            let
              fun apply_attr ((name, value), (rc, out, err)) =
                if rc <> 0 then (rc, out, err)  (* Skip if previous failed *)
                else
                  let
                    val cmd = 
                      case name of
                        "readonly" => 
                          if value then
                            "$file = Get-Item '" ^ path ^ "'; $file.IsReadOnly = $true"
                          else
                            "$file = Get-Item '" ^ path ^ "'; $file.IsReadOnly = $false"
                      | "hidden" =>
                          if value then
                            "$file = Get-Item '" ^ path ^ "' -Force; " ^
                            "$file.Attributes = $file.Attributes -bor [System.IO.FileAttributes]::Hidden"
                          else
                            "$file = Get-Item '" ^ path ^ "' -Force; " ^
                            "$file.Attributes = $file.Attributes -band -bnot [System.IO.FileAttributes]::Hidden"
                      | "archive" =>
                          if value then
                            "$file = Get-Item '" ^ path ^ "' -Force; " ^
                            "$file.Attributes = $file.Attributes -bor [System.IO.FileAttributes]::Archive"
                          else
                            "$file = Get-Item '" ^ path ^ "' -Force; " ^
                            "$file.Attributes = $file.Attributes -band -bnot [System.IO.FileAttributes]::Archive"
                      | "system" =>
                          if value then
                            "$file = Get-Item '" ^ path ^ "' -Force; " ^
                            "$file.Attributes = $file.Attributes -bor [System.IO.FileAttributes]::System"
                          else
                            "$file = Get-Item '" ^ path ^ "' -Force; " ^
                            "$file.Attributes = $file.Attributes -band -bnot [System.IO.FileAttributes]::System"
                      | _ => ""  (* Skip unknown attributes *)
                      
                    val (exec_rc, exec_out, exec_err) = 
                      if cmd <> "" then execute_powershell cmd
                      else (0, "", "")
                  in
                    (exec_rc, out ^ exec_out, err ^ exec_err)
                  end
              
              (* Apply all attributes *)
              val (final_rc, final_out, final_err) = 
                List.foldl apply_attr (0, "", "") attrs
            in
              (final_rc, final_out, final_err)
            end
          | _ => (0, "", "")
      
      (* Determine if changes were made *)
      val changed = (op_rc = 0 andalso current_state <> state) orelse
                   (attr_rc = 0 andalso String.size attr_out > 0)
    in
      {
        changed = changed,
        failed = op_rc <> 0 orelse attr_rc <> 0,
        msg = if op_rc <> 0 then "Failed to change file state"
              else if attr_rc <> 0 then "Failed to set file attributes"
              else if changed then "File state changed"
              else "File state unchanged",
        rc = SOME (if op_rc <> 0 then op_rc else attr_rc),
        stdout = SOME (op_out ^ attr_out),
        stderr = SOME (op_err ^ attr_err),
        output = [("path", path), 
                  ("state", state),
                  ("previous", current_state)]
      }
    end
  
  (* Implementation of win_acl module *)
  fun win_acl {path, user, rights, type_, state} =
    let
      (* Check if ACL exists *)
      val check_cmd = "$acl = Get-Acl -Path '" ^ path ^ "'; " ^
                     "$access = $acl.Access | Where-Object { " ^
                     "  $_.IdentityReference -eq '" ^ user ^ "' -and " ^
                     "  $_.FileSystemRights -eq '" ^ rights ^ "' -and " ^
                     "  $_.AccessControlType -eq '" ^ type_ ^ "'" ^
                     "}; " ^
                     "if ($access) { 'exists' } else { 'notexists' }"
      val (check_rc, check_out, _) = execute_powershell check_cmd
      val acl_exists = check_rc = 0 andalso String.isSubstring "exists" check_out
      
      (* Execute ACL change if needed *)
      val (op_rc, op_out, op_err) = 
        case (state, acl_exists) of
          ("present", false) =>
            (* Add ACL *)
            let
              val cmd = "$acl = Get-Acl -Path '" ^ path ^ "'; " ^
                       "$accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule(" ^
                       "  '" ^ user ^ "', " ^
                       "  '" ^ rights ^ "', " ^
                       "  'ContainerInherit,ObjectInherit', " ^
                       "  'None', " ^
                       "  '" ^ type_ ^ "'" ^
                       "); " ^
                       "$acl.AddAccessRule($accessRule); " ^
                       "Set-Acl -Path '" ^ path ^ "' -AclObject $acl"
            in
              execute_powershell cmd
            end
          ("absent", true) =>
            (* Remove ACL *)
            let
              val cmd = "$acl = Get-Acl -Path '" ^ path ^ "'; " ^
                       "$accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule(" ^
                       "  '" ^ user ^ "', " ^
                       "  '" ^ rights ^ "', " ^
                       "  'ContainerInherit,ObjectInherit', " ^
                       "  'None', " ^
                       "  '" ^ type_ ^ "'" ^
                       "); " ^
                       "$acl.RemoveAccessRule($accessRule); " ^
                       "Set-Acl -Path '" ^ path ^ "' -AclObject $acl"
            in
              execute_powershell cmd
            end
          (_, _) =>
            (* No change needed *)
            (0, "", "")
      
      (* Determine if changes were made *)
      val changed = (state = "present" andalso not acl_exists andalso op_rc = 0) orelse
                   (state = "absent" andalso acl_exists andalso op_rc = 0)
    in
      {
        changed = changed,
        failed = op_rc <> 0,
        msg = if op_rc <> 0 then "Failed to modify ACL"
              else if changed then "ACL modified"
              else "ACL unchanged",
        rc = SOME op_rc,
        stdout = SOME op_out,
        stderr = SOME op_err,
        output = [("path", path), 
                  ("user", user),
                  ("rights", rights),
                  ("type", type_),
                  ("state", state)]
      }
    end
  
  (* Implementation of win_updates module *)
  fun win_updates {category_names, log_path, state} =
    let
      (* Build category filter *)
      val category_filter = 
        case category_names of
          SOME categories =>
            let
              val category_str = String.concatWith "," 
                                (map (fn c => "'" ^ c ^ "'") categories)
            in
              "$categories = @(" ^ category_str ^ "); " ^
              "$criteriaByCategory = $categories | ForEach-Object { " ^
              "  \"IsInstalled=0 and Type='Software' and Category='$_'\" " ^
              "}; " ^
              "$SearchCriteria = $criteriaByCategory -join ' OR '"
            end
          | NONE =>
            "$SearchCriteria = \"IsInstalled=0 and Type='Software'\""
      
      (* Function to search for updates *)
      val search_cmd = category_filter ^ "; " ^
                      "$Session = New-Object -ComObject Microsoft.Update.Session; " ^
                      "$Searcher = $Session.CreateUpdateSearcher(); " ^
                      "$SearchResult = $Searcher.Search($SearchCriteria); " ^
                      "$SearchResult.Updates | " ^
                      "Select-Object Title, Description | " ^
                      "ConvertTo-Json -Compress"
      
      (* Execute the search *)
      val (search_rc, search_out, search_err) = execute_powershell search_cmd
      
      (* Install updates if requested *)
      val (install_rc, install_out, install_err) = 
        if state = "installed" andalso search_rc = 0 then
          let
            val install_cmd = category_filter ^ "; " ^
                             "$Session = New-Object -ComObject Microsoft.Update.Session; " ^
                             "$Searcher = $Session.CreateUpdateSearcher(); " ^
                             "$SearchResult = $Searcher.Search($SearchCriteria); " ^
                             
                             (* Create collection and add all updates *)
                             "$UpdatesToInstall = New-Object -ComObject Microsoft.Update.UpdateColl; " ^
                             "foreach($Update in $SearchResult.Updates) { " ^
                             "  if ($Update.IsDownloaded) { $UpdatesToInstall.Add($Update) | Out-Null } " ^
                             "} " ^
                             
                             (* If updates available, download and install *)
                             "if ($UpdatesToInstall.Count -gt 0) { " ^
                             "  $Downloader = $Session.CreateUpdateDownloader(); " ^
                             "  $Downloader.Updates = $UpdatesToInstall; " ^
                             "  $Downloader.Download(); " ^
                             
                             "  $Installer = $Session.CreateUpdateInstaller(); " ^
                             "  $Installer.Updates = $UpdatesToInstall; " ^
                             "  $InstallResult = $Installer.Install(); " ^
                             
                             "  $InstallResult | " ^
                             "  Select-Object ResultCode, RebootRequired | " ^
                             "  ConvertTo-Json -Compress " ^
                             "} else { " ^
                             "  Write-Output '{\"ResultCode\": 0, \"RebootRequired\": false}' " ^
                             "}"
                             
            val (rc, out, err) = execute_powershell install_cmd
            
            (* Log results if requested *)
            val _ = case log_path of
                      SOME path => 
                        let
                          val log_file = TextIO.openAppOut path
                          val log_content = 
                            "Time: " ^ Date.toString(Date.fromTimeLocal(Time.now())) ^ "\n" ^
                            "Search results: " ^ search_out ^ "\n" ^
                            "Install results: " ^ out ^ "\n\n"
                        in
                          TextIO.output(log_file, log_content);
                          TextIO.closeOut log_file
                        end
                    | NONE => ()
          in
            (rc, out, err)
          end
        else
          (0, "", "")
      
      (* Determine if changes were made *)
      val changed = state = "installed" andalso 
                   install_rc = 0 andalso
                   String.isSubstring "ResultCode" install_out
      
      (* Check if reboot is required *)
      val reboot_required = install_rc = 0 andalso
                           String.isSubstring "RebootRequired\": true" install_out
    in
      {
        changed = changed,
        failed = search_rc <> 0 orelse 
                (state = "installed" andalso install_rc <> 0),
        msg = if search_rc <> 0 then "Failed to search for updates"
              else if state = "installed" andalso install_rc <> 0 then 
                "Failed to install updates"
              else if changed then "Updates installed"
              else "No updates installed",
        rc = SOME (if search_rc <> 0 then search_rc else install_rc),
        stdout = SOME (search_out ^ "\n" ^ install_out),
        stderr = SOME (search_err ^ "\n" ^ install_err),
        output = [
          ("state", state),
          ("reboot_required", if reboot_required then "true" else "false")
        ]
      }
    end
end