(* windows_module.sml
 * Implementation of Windows-specific Ansible-like modules
 *)

structure WindowsModule : WINDOWS_MODULE = struct
  (* Type definitions *)
  type module_result = {
    changed: bool,
    failed: bool,
    msg: string option,
    results: (string * string) list
  }
  
  (* Module argument types *)
  type command_args = {
    command: string,
    chdir: string option,
    creates: string option,
    removes: string option
  }
  
  type file_args = {
    path: string,
    state: string, (* "absent", "directory", "file", "touch" *)
    recurse: bool,
    force: bool
  }

  type copy_args = {
    src: string option,
    content: string option,
    dest: string,
    backup: bool,
    force: bool
  }

  type service_args = {
    name: string,
    state: string option, (* "started", "stopped", "restarted" *)
    startup: string option (* "auto", "manual", "disabled" *)
  }

  type feature_args = {
    name: string,
    state: string (* "present", "absent" *)
  }

  type regedit_args = {
    path: string,
    name: string option,
    data: string option,
    type_str: string option, (* "string", "expandstring", "binary", "dword", "multistring" *)
    state: string (* "present", "absent" *)
  }

  type acl_args = {
    path: string,
    user: string,
    rights: string, (* "FullControl", "Modify", "ReadAndExecute", "Read", "Write" *)
    type_str: string, (* "allow", "deny" *)
    inherit_flags: string option
  }

  type updates_args = {
    category_names: string list option, (* "CriticalUpdates", "SecurityUpdates", etc. *)
    log_path: string option,
    state: string (* "installed", "searched" *)
  }

  (* Module exceptions *)
  exception ModuleError of string
  exception ArgumentError of string
  
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

  (* Helper function to check if a path exists *)
  fun path_exists path =
    let
      val cmd = "Test-Path -Path '" ^ path ^ "' -PathType Any"
      val (_, stdout, _) = execute_powershell cmd
    in
      String.isSubstring "True" stdout
    end

  (* Helper function to check if a path is a directory *)
  fun is_directory path =
    let
      val cmd = "Test-Path -Path '" ^ path ^ "' -PathType Container"
      val (_, stdout, _) = execute_powershell cmd
    in
      String.isSubstring "True" stdout
    end

  (* Helper function to check if a path is a file *)
  fun is_file path =
    let
      val cmd = "Test-Path -Path '" ^ path ^ "' -PathType Leaf"
      val (_, stdout, _) = execute_powershell cmd
    in
      String.isSubstring "True" stdout
    end
  
  (* win_command module implementation *)
  fun run_win_command (args: command_args) : module_result =
    let
      (* Check creates/removes conditions *)
      val skip_execution = 
        case (#creates args) of
          SOME path => path_exists path
        | NONE => 
            case (#removes args) of
              SOME path => not (path_exists path)
            | NONE => false
            
      val result = 
        if skip_execution then
          {
            changed = false,
            failed = false,
            msg = SOME "Skipped execution due to creates/removes condition",
            results = []
          }
        else
          let
            (* Build command with optional working directory *)
            val cmd = 
              case (#chdir args) of
                SOME dir => "Push-Location '" ^ dir ^ "'; " ^ (#command args) ^ "; Pop-Location"
              | NONE => (#command args)
              
            (* Execute command *)
            val (rc, stdout, stderr) = execute_powershell cmd
            
            (* Prepare result *)
            val failed = rc <> 0
            val msg = 
              if failed then
                SOME ("Command execution failed with exit code " ^ Int.toString rc)
              else
                NONE
          in
            {
              changed = true,
              failed = failed,
              msg = msg,
              results = [
                ("rc", Int.toString rc),
                ("stdout", stdout),
                ("stderr", stderr)
              ]
            }
          end
    in
      result
    end
  
  (* win_file module implementation *)
  fun run_win_file (args: file_args) : module_result =
    let
      val path = #path args
      val state = #state args
      val recurse = #recurse args
      val force = #force args
      
      (* Check if any changes are needed *)
      val path_status = 
        if not (path_exists path) then
          "absent"
        else if is_directory path then
          "directory"
        else
          "file"
          
      val needs_change = path_status <> state andalso 
                         (state <> "touch" orelse path_status = "absent")
                         
      (* Function to create, remove, or touch a file/directory *)
      fun make_change () =
        let
          val (cmd, success_msg) = 
            case state of
              "absent" => 
                let
                  val remove_cmd = 
                    if is_directory path then
                      if recurse then
                        "Remove-Item -Path '" ^ path ^ "' -Recurse -Force:$" ^ 
                        (if force then "true" else "false")
                      else
                        "Remove-Item -Path '" ^ path ^ "' -Force:$" ^ 
                        (if force then "true" else "false")
                    else
                      "Remove-Item -Path '" ^ path ^ "' -Force:$" ^ 
                      (if force then "true" else "false")
                in
                  (remove_cmd, "Removed " ^ path)
                end
            | "directory" => 
                ("New-Item -Path '" ^ path ^ "' -ItemType Directory -Force:$" ^ 
                 (if force then "true" else "false"),
                 "Created directory " ^ path)
            | "file" => 
                ("New-Item -Path '" ^ path ^ "' -ItemType File -Force:$" ^ 
                 (if force then "true" else "false"),
                 "Created file " ^ path)
            | "touch" => 
                if path_exists path then
                  ("(Get-Item '" ^ path ^ "').LastWriteTime = Get-Date",
                   "Updated timestamp of " ^ path)
                else
                  ("New-Item -Path '" ^ path ^ "' -ItemType File -Force:$" ^ 
                   (if force then "true" else "false"),
                   "Created file " ^ path)
            | _ => raise ArgumentError("Invalid state: " ^ state)
                
          (* Execute the command *)
          val (rc, stdout, stderr) = execute_powershell cmd
          
          val failed = rc <> 0
          val msg = 
            if failed then
              SOME ("Failed to change state: " ^ stderr)
            else
              SOME success_msg
        in
          {
            changed = true,
            failed = failed,
            msg = msg,
            results = [
              ("rc", Int.toString rc),
              ("stdout", stdout),
              ("stderr", stderr)
            ]
          }
        end
    in
      if needs_change then
        make_change()
      else
        {
          changed = false,
          failed = false,
          msg = SOME ("File already in desired state"),
          results = []
        }
    end
  
  (* win_copy module implementation *)
  fun run_win_copy (args: copy_args) : module_result =
    let
      val dest = #dest args
      val backup = #backup args
      val force = #force args
      
      (* Verify args *)
      val _ = 
        case (#src args, #content args) of
          (NONE, NONE) => raise ArgumentError("Either src or content must be provided")
        | (SOME _, SOME _) => raise ArgumentError("Cannot specify both src and content")
        | _ => ()
        
      (* Check if destination exists *)
      val dest_exists = path_exists dest
      
      (* Skip if destination exists and force is false *)
      val skip_operation = dest_exists andalso not force
      
      (* Function to create backup *)
      fun create_backup () =
        if backup andalso dest_exists then
          let
            (* FIXED: Added IntInf.toInt conversion *)
            val backup_path = dest ^ "." ^ 
                             Int.toString(IntInf.toInt(Time.toSeconds(Time.now()))) ^ ".bak"
            val cmd = "Copy-Item -Path '" ^ dest ^ "' -Destination '" ^ backup_path ^ "' -Force"
            val (rc, _, stderr) = execute_powershell cmd
            
            val _ = if rc <> 0 then
                      raise ModuleError("Failed to create backup: " ^ stderr)
                    else
                      ()
          in
            SOME backup_path
          end
        else
          NONE
          
      (* Function to copy content *)
      fun perform_copy () =
        let
          (* Create backup if needed *)
          val backup_path = create_backup()
          
          (* Prepare directory if needed *)
          val dest_dir = String.substring(dest, 0, 
                                         String.size dest - 
                                         String.size(OS.Path.file dest))
          val _ = 
            if not (path_exists dest_dir) then
              let
                val mkdir_cmd = "New-Item -Path '" ^ dest_dir ^ 
                               "' -ItemType Directory -Force"
                val (rc, _, stderr) = execute_powershell mkdir_cmd
                
                val _ = if rc <> 0 then
                          raise ModuleError("Failed to create directory: " ^ stderr)
                        else
                          ()
              in
                ()
              end
            else
              ()
          
          (* Execute copy operation *)
          val (cmd, desc) = 
            case (#src args, #content args) of
              (SOME src, NONE) => 
                ("Copy-Item -Path '" ^ src ^ "' -Destination '" ^ dest ^ "' -Force",
                 "Copied " ^ src ^ " to " ^ dest)
              | (NONE, SOME content) => 
                let
                  (* Escape content for PowerShell *)
                  val escaped_content = String.translate 
                    (fn #"\"" => "\\\"" | #"$" => "`$" | #"'" => "''" | c => str c) 
                    content
                in
                  ("Set-Content -Path '" ^ dest ^ "' -Value '" ^ escaped_content ^ "' -Force",
                   "Wrote content to " ^ dest)
                end
              | _ => raise ModuleError("Unexpected argument combination")
                
          val (rc, stdout, stderr) = execute_powershell cmd
          
          val failed = rc <> 0
          val msg = 
            if failed then
              SOME ("Failed to copy content: " ^ stderr)
            else
              SOME desc
        in
          {
            changed = true,
            failed = failed,
            msg = msg,
            results = [
              ("rc", Int.toString rc),
              ("stdout", stdout),
              ("stderr", stderr),
              ("backup", case backup_path of SOME path => path | NONE => "")
            ]
          }
        end
    in
      if skip_operation then
        {
          changed = false,
          failed = false,
          msg = SOME ("File exists and force=false"),
          results = []
        }
      else
        perform_copy()
    end
    handle e => 
      {
        changed = false,
        failed = true,
        msg = SOME (exnMessage e),
        results = []
      }
  
  (* win_service module implementation *)
  fun run_win_service (args: service_args) : module_result =
    let
      val name = #name args
      
      (* Get current service status *)
      val status_cmd = 
        "Get-Service -Name '" ^ name ^ "' | " ^ 
        "Select-Object -Property Name, Status, StartType | " ^
        "ConvertTo-Json"
        
      val (status_rc, status_stdout, status_stderr) = execute_powershell status_cmd
      
      (* Check if service exists *)
      val service_exists = status_rc = 0
      
      (* If service doesn't exist, return error *)
      val result = 
        if not service_exists then
          {
            changed = false,
            failed = true,
            msg = SOME ("Service '" ^ name ^ "' not found"),
            results = [("stderr", status_stderr)]
          }
        else
          let
            (* Extract current status from PowerShell output *)
            val current_status = 
              if String.isSubstring "Running" status_stdout then "started"
              else "stopped"
              
            (* Extract current startup type *)
            val current_startup = 
              if String.isSubstring "Automatic" status_stdout then "auto"
              else if String.isSubstring "Manual" status_stdout then "manual"
              else "disabled"
              
            (* Check if changes are needed *)
            val needs_state_change = 
              case (#state args) of
                SOME desired_state => desired_state <> current_status
              | NONE => false
              
            val needs_startup_change = 
              case (#startup args) of
                SOME desired_startup => desired_startup <> current_startup
              | NONE => false
              
            (* Apply changes if needed *)
            val (changed, failed, msg, change_results) = 
              if needs_state_change orelse needs_startup_change then
                let
                  (* Change service state if needed *)
                  val (state_cmd, state_changed, state_failed, state_msg) = 
                    case (#state args) of
                      SOME "started" => 
                        if current_status <> "started" then
                          let
                            val cmd = "Start-Service -Name '" ^ name ^ "'"
                            val (rc, _, stderr) = execute_powershell cmd
                            
                            val failed = rc <> 0
                            val msg = 
                              if failed then
                                "Failed to start service: " ^ stderr
                              else
                                "Service started"
                          in
                            (cmd, true, failed, msg)
                          end
                        else
                          ("", false, false, "")
                    | SOME "stopped" => 
                        if current_status <> "stopped" then
                          let
                            val cmd = "Stop-Service -Name '" ^ name ^ "'"
                            val (rc, _, stderr) = execute_powershell cmd
                            
                            val failed = rc <> 0
                            val msg = 
                              if failed then
                                "Failed to stop service: " ^ stderr
                              else
                                "Service stopped"
                          in
                            (cmd, true, failed, msg)
                          end
                        else
                          ("", false, false, "")
                    | SOME "restarted" => 
                        let
                          val cmd = "Restart-Service -Name '" ^ name ^ "'"
                          val (rc, _, stderr) = execute_powershell cmd
                          
                          val failed = rc <> 0
                          val msg = 
                            if failed then
                              "Failed to restart service: " ^ stderr
                            else
                              "Service restarted"
                        in
                          (cmd, true, failed, msg)
                        end
                    | NONE => ("", false, false, "")
                    | SOME s => 
                        raise ArgumentError("Invalid service state: " ^ s)
                  
                  (* Change startup type if needed *)
                  val (startup_cmd, startup_changed, startup_failed, startup_msg) = 
                    case (#startup args) of
                      SOME "auto" => 
                        if current_startup <> "auto" then
                          let
                            val cmd = "Set-Service -Name '" ^ name ^ "' -StartupType Automatic"
                            val (rc, _, stderr) = execute_powershell cmd
                            
                            val failed = rc <> 0
                            val msg = 
                              if failed then
                                "Failed to set startup type: " ^ stderr
                              else
                                "Startup type set to Automatic"
                          in
                            (cmd, true, failed, msg)
                          end
                        else
                          ("", false, false, "")
                    | SOME "manual" => 
                        if current_startup <> "manual" then
                          let
                            val cmd = "Set-Service -Name '" ^ name ^ "' -StartupType Manual"
                            val (rc, _, stderr) = execute_powershell cmd
                            
                            val failed = rc <> 0
                            val msg = 
                              if failed then
                                "Failed to set startup type: " ^ stderr
                              else
                                "Startup type set to Manual"
                          in
                            (cmd, true, failed, msg)
                          end
                        else
                          ("", false, false, "")
                    | SOME "disabled" => 
                        if current_startup <> "disabled" then
                          let
                            val cmd = "Set-Service -Name '" ^ name ^ "' -StartupType Disabled"
                            val (rc, _, stderr) = execute_powershell cmd
                            
                            val failed = rc <> 0
                            val msg = 
                              if failed then
                                "Failed to set startup type: " ^ stderr
                              else
                                "Startup type set to Disabled"
                          in
                            (cmd, true, failed, msg)
                          end
                        else
                          ("", false, false, "")
                    | NONE => ("", false, false, "")
                    | SOME s => 
                        raise ArgumentError("Invalid startup type: " ^ s)
                  
                  (* Combine results *)
                  val changed = state_changed orelse startup_changed
                  val failed = state_failed orelse startup_failed
                  val msg = 
                    if state_msg <> "" andalso startup_msg <> "" then
                      state_msg ^ "; " ^ startup_msg
                    else if state_msg <> "" then
                      state_msg
                    else
                      startup_msg
                      
                  val results = [
                    ("state_changed", Bool.toString state_changed),
                    ("startup_changed", Bool.toString startup_changed)
                  ]
                in
                  (changed, failed, SOME msg, results)
                end
              else
                (false, false, SOME "No changes required", [])
                
            (* Get final service status *)
            val final_status_cmd = 
              "Get-Service -Name '" ^ name ^ "' | " ^ 
              "Select-Object -Property Name, Status, StartType | " ^
              "ConvertTo-Json"
              
            val (_, final_status_stdout, _) = execute_powershell final_status_cmd
            
            (* Extract final status *)
            val final_running = String.isSubstring "Running" final_status_stdout
            val final_status = if final_running then "started" else "stopped"
            
            (* Extract final startup type *)
            val final_startup = 
              if String.isSubstring "Automatic" final_status_stdout then "auto"
              else if String.isSubstring "Manual" final_status_stdout then "manual"
              else "disabled"
            
            (* Add status info to results *)
            val info_results = [
              ("name", name),
              ("state", final_status),
              ("startup", final_startup)
            ]
          in
            {
              changed = changed,
              failed = failed,
              msg = msg,
              results = info_results @ change_results
            }
          end
    in
      result
    end
    handle e => 
      {
        changed = false,
        failed = true,
        msg = SOME (exnMessage e),
        results = []
      }
  
  (* win_feature module implementation *)
  fun run_win_feature (args: feature_args) : module_result =
    let
      val name = #name args
      val state = #state args
      
      (* Check if feature exists *)
      val check_cmd = 
        "Import-Module ServerManager; " ^
        "Get-WindowsFeature -Name '" ^ name ^ "' | " ^
        "Select-Object -Property Name, InstallState | " ^
        "ConvertTo-Json"
        
      val (check_rc, check_stdout, check_stderr) = execute_powershell check_cmd
      
      (* Check if feature exists *)
      val feature_exists = check_rc = 0 andalso not (String.isSubstring "not found" check_stderr)
      
      (* If feature doesn't exist, return error *)
      val result = 
        if not feature_exists then
          {
            changed = false,
            failed = true,
            msg = SOME ("Feature '" ^ name ^ "' not found"),
            results = [("stderr", check_stderr)]
          }
        else
          let
            (* Check current install state *)
            val is_installed = String.isSubstring "Installed" check_stdout
            
            (* Check if change is needed *)
            val needs_change = 
              (state = "present" andalso not is_installed) orelse
              (state = "absent" andalso is_installed)
              
            (* Apply change if needed *)
            val (changed, failed, msg, cmd_results) = 
              if needs_change then
                let
                  val cmd = 
                    if state = "present" then
                      "Import-Module ServerManager; " ^
                      "Add-WindowsFeature -Name '" ^ name ^ "' -IncludeAllSubFeature | " ^
                      "ConvertTo-Json"
                    else
                      "Import-Module ServerManager; " ^
                      "Remove-WindowsFeature -Name '" ^ name ^ "' | " ^
                      "ConvertTo-Json"
                      
                  val (rc, stdout, stderr) = execute_powershell cmd
                  
                  val failed = rc <> 0
                  val msg = 
                    if failed then
                      "Failed to " ^ (if state = "present" then "install" else "remove") ^ 
                      " feature: " ^ stderr
                    else
                      "Feature " ^ (if state = "present" then "installed" else "removed")
                in
                  (true, failed, msg, [("stdout", stdout), ("stderr", stderr)])
                end
              else
                (false, false, 
                 "Feature already " ^ (if state = "present" then "installed" else "absent"),
                 [])
          in
            {
              changed = changed,
              failed = failed,
              msg = SOME msg,
              results = [
                ("name", name),
                ("state", state),
                ("installed", Bool.toString (if changed then state = "present" else is_installed))
              ] @ cmd_results
            }
          end
    in
      result
    end
    handle e => 
      {
        changed = false,
        failed = true,
        msg = SOME (exnMessage e),
        results = []
      }
  
  (* win_regedit module implementation *)
  fun run_win_regedit (args: regedit_args) : module_result =
    let
      val path = #path args
      val state = #state args
      
      (* Check if registry path exists *)
      val path_check_cmd = "Test-Path -Path '" ^ path ^ "'"
      val (_, path_check_stdout, _) = execute_powershell path_check_cmd
      val path_exists = String.isSubstring "True" path_check_stdout
      
      (* If checking a value, handle differently than path *)
      val result = 
        case (#name args) of
          NONE => 
            (* Handle registry path operations *)
            if state = "present" then
              if not path_exists then
                (* Create registry key *)
                let
                  val cmd = "New-Item -Path '" ^ path ^ "' -Force"
                  val (rc, stdout, stderr) = execute_powershell cmd
                  
                  val failed = rc <> 0
                  val msg = 
                    if failed then
                      "Failed to create registry key: " ^ stderr
                    else
                      "Registry key created"
                in
                  {
                    changed = true,
                    failed = failed,
                    msg = SOME msg,
                    results = [("stdout", stdout), ("stderr", stderr)]
                  }
                end
              else
                {
                  changed = false,
                  failed = false,
                  msg = SOME "Registry key already exists",
                  results = []
                }
            else (* state = "absent" *)
              if path_exists then
                (* Remove registry key *)
                let
                  val cmd = "Remove-Item -Path '" ^ path ^ "' -Force -Recurse"
                  val (rc, stdout, stderr) = execute_powershell cmd
                  
                  val failed = rc <> 0
                  val msg = 
                    if failed then
                      "Failed to remove registry key: " ^ stderr
                    else
                      "Registry key removed"
                in
                  {
                    changed = true,
                    failed = failed,
                    msg = SOME msg,
                    results = [("stdout", stdout), ("stderr", stderr)]
                  }
                end
              else
                {
                  changed = false,
                  failed = false,
                  msg = SOME "Registry key does not exist",
                  results = []
                }
        | SOME name => 
            (* Handle registry value operations *)
            let
              (* Check if registry key exists first *)
              val _ = 
                if not path_exists andalso state = "present" then
                  (* Create registry key *)
                  let
                    val cmd = "New-Item -Path '" ^ path ^ "' -Force"
                    val (rc, _, stderr) = execute_powershell cmd
                    
                    val _ = if rc <> 0 then
                              raise ModuleError("Failed to create registry key: " ^ stderr)
                            else
                              ()
                  in
                    ()
                  end
                else
                  ()
                  
              (* Check if value exists *)
              val value_check_cmd = 
                "if (Test-Path -Path '" ^ path ^ "') {" ^
                "  if (Get-ItemProperty -Path '" ^ path ^ "' -Name '" ^ name ^ "' -ErrorAction SilentlyContinue) {" ^
                "    $value = (Get-ItemProperty -Path '" ^ path ^ "' -Name '" ^ name ^ "').$(" ^ name ^ ")" ^
                "    Write-Output \"EXISTS:$value\"" ^
                "  } else {" ^
                "    Write-Output \"NOT_EXISTS\"" ^
                "  }" ^
                "} else {" ^
                "  Write-Output \"NOT_EXISTS\"" ^
                "}"
                
              val (_, value_check_stdout, _) = execute_powershell value_check_cmd
              val value_exists = String.isSubstring "EXISTS:" value_check_stdout
              val current_value = 
                if value_exists then
                  SOME (String.extract(value_check_stdout, 
                                      String.size "EXISTS:", 
                                      NONE))
                else
                  NONE
            in
              if state = "present" then
                case (#data args, #type_str args) of
                  (SOME data, SOME type_str) => 
                    if not value_exists orelse 
                       (case current_value of 
                          SOME v => not (String.isSubstring data v)
                        | NONE => true) then
                      (* Create/update registry value *)
                      let
                        (* Convert type string to PowerShell type *)
                        val ps_type = 
                          case type_str of
                            "string" => "String"
                          | "expandstring" => "ExpandString"
                          | "binary" => "Binary"
                          | "dword" => "DWord"
                          | "qword" => "QWord"
                          | "multistring" => "MultiString"
                          | _ => raise ArgumentError("Invalid registry value type: " ^ type_str)
                          
                        (* Prepare value based on type *)
                        val ps_value = 
                          case type_str of
                            "binary" => 
                              (* Convert comma-separated hex to byte array *)
                              "(byte[](\"" ^ data ^ "\" -split ',' -replace ' ',''))"
                          | "multistring" => 
                              (* Convert semicolon-separated strings to array *)
                              "@(\"" ^ String.translate 
                                    (fn #";" => "\",\"" | c => str c) 
                                    data ^ "\")"
                          | _ => "\"" ^ data ^ "\""
                          
                        val cmd = 
                          "New-ItemProperty -Path '" ^ path ^ "' -Name '" ^ name ^ 
                          "' -PropertyType " ^ ps_type ^ " -Value " ^ ps_value ^ " -Force"
                          
                        val (rc, stdout, stderr) = execute_powershell cmd
                        
                        val failed = rc <> 0
                        val msg = 
                          if failed then
                            "Failed to set registry value: " ^ stderr
                          else
                            if value_exists then
                              "Registry value updated"
                            else
                              "Registry value created"
                      in
                        {
                          changed = true,
                          failed = failed,
                          msg = SOME msg,
                          results = [("stdout", stdout), ("stderr", stderr)]
                        }
                      end
                    else
                      {
                        changed = false,
                        failed = false,
                        msg = SOME "Registry value already set correctly",
                        results = []
                      }
                | _ => raise ArgumentError("Both data and type must be specified for present state")
              else (* state = "absent" *)
                if value_exists then
                  (* Remove registry value *)
                  let
                    val cmd = "Remove-ItemProperty -Path '" ^ path ^ "' -Name '" ^ name ^ "' -Force"
                    val (rc, stdout, stderr) = execute_powershell cmd
                    
                    val failed = rc <> 0
                    val msg = 
                      if failed then
                        "Failed to remove registry value: " ^ stderr
                      else
                        "Registry value removed"
                  in
                    {
                      changed = true,
                      failed = failed,
                      msg = SOME msg,
                      results = [("stdout", stdout), ("stderr", stderr)]
                    }
                  end
                else
                  {
                    changed = false,
                    failed = false,
                    msg = SOME "Registry value does not exist",
                    results = []
                  }
            end
    in
      result
    end
    handle e => 
      {
        changed = false,
        failed = true,
        msg = SOME (exnMessage e),
        results = []
      }
  
  (* win_acl module implementation *)
  fun run_win_acl (args: acl_args) : module_result =
    let
      val path = #path args
      val user = #user args
      val rights = #rights args
      val type_str = #type_str args
      val inherit_flags = #inherit_flags args
      
      (* Translate rights to PowerShell FileSystemRights *)
      val ps_rights = 
        case rights of
          "FullControl" => "FullControl"
        | "Modify" => "Modify"
        | "ReadAndExecute" => "ReadAndExecute"
        | "Read" => "Read"
        | "Write" => "Write"
        | _ => raise ArgumentError("Invalid ACL rights: " ^ rights)
          
      (* Translate type to PowerShell AccessControlType *)
      val ps_type = 
        case type_str of
          "allow" => "Allow"
        | "deny" => "Deny"
        | _ => raise ArgumentError("Invalid ACL type: " ^ type_str)
      
      (* Check if path exists *)
      val _ = 
        if not (path_exists path) then
          raise ModuleError("Path does not exist: " ^ path)
        else
          ()
          
      (* Check if ACL already exists *)
      val check_cmd = 
        "$acl = Get-Acl -Path '" ^ path ^ "';" ^
        "$hasRule = $false;" ^
        "foreach ($rule in $acl.Access) {" ^
        "  if ($rule.IdentityReference.Value -eq '" ^ user ^ "' -and " ^
        "      $rule.FileSystemRights -eq [System.Security.AccessControl.FileSystemRights]::'" ^ ps_rights ^ "' -and " ^
        "      $rule.AccessControlType -eq [System.Security.AccessControl.AccessControlType]::'" ^ ps_type ^ "') {" ^
        "    $hasRule = $true;" ^
        "    break;" ^
        "  }" ^
        "}" ^
        "Write-Output $hasRule"
        
      val (_, check_stdout, _) = execute_powershell check_cmd
      val acl_exists = String.isSubstring "True" check_stdout
      
      (* Apply ACL if it doesn't exist *)
      val result = 
        if not acl_exists then
          let
            (* Build inheritance flags if specified *)
            val inherit_params = 
              case inherit_flags of
                SOME flags => 
                  " -InheritanceFlags '" ^ flags ^ "' -PropagationFlags '" ^ flags ^ "'"
              | NONE => ""
              
            (* Build command to add ACL *)
            val cmd = 
              "$acl = Get-Acl -Path '" ^ path ^ "';" ^
              "$rule = New-Object System.Security.AccessControl.FileSystemAccessRule('" ^ 
              user ^ "', '" ^ ps_rights ^ "', " ^ 
              inherit_params ^ ", '" ^ ps_type ^ "');" ^
              "$acl.AddAccessRule($rule);" ^
              "Set-Acl -Path '" ^ path ^ "' -AclObject $acl"
              
            val (rc, stdout, stderr) = execute_powershell cmd
            
            val failed = rc <> 0
            val msg = 
              if failed then
                "Failed to set ACL: " ^ stderr
              else
                "ACL rule added"
          in
            {
              changed = true,
              failed = failed,
              msg = SOME msg,
              results = [("stdout", stdout), ("stderr", stderr)]
            }
          end
        else
          {
            changed = false,
            failed = false,
            msg = SOME "ACL rule already exists",
            results = []
          }
    in
      result
    end
    handle e => 
      {
        changed = false,
        failed = true,
        msg = SOME (exnMessage e),
        results = []
      }
  
  (* win_updates module implementation *)
  fun run_win_updates (args: updates_args) : module_result =
    let
      val state = #state args
      
      (* Prepare category filter *)
      val category_filter = 
        case (#category_names args) of
          SOME categories => 
            let
              (* Format categories for PowerShell *)
              val categories_str = String.concatWith "','" categories
            in
              " | Where-Object { $_.Categories -contains '" ^ categories_str ^ "' }"
            end
        | NONE => ""
        
      (* Prepare log path *)
      val log_cmd = 
        case (#log_path args) of
          SOME log_path => 
            "Start-Transcript -Path '" ^ log_path ^ "' -Append;"
        | NONE => ""
        
      (* Function to search for updates *)
      fun search_updates () =
        let
          val cmd = 
            log_cmd ^ 
            "$session = New-Object -ComObject 'Microsoft.Update.Session';" ^
            "$searcher = $session.CreateUpdateSearcher();" ^
            "$result = $searcher.Search('IsInstalled=0');" ^
            "$updates = $result.Updates" ^ category_filter ^ " | " ^ 
            "Select-Object Title, Description, @{Name='KB';Expression={$_.KBArticleIDs[0]}} | " ^
            "ConvertTo-Json"
            
          val (rc, stdout, stderr) = execute_powershell cmd
          
          val failed = rc <> 0
          val updates_count = 
            if failed then 0
            else
              let
                (* Count updates - very simplified, would need proper JSON parsing *)
                val lines = String.tokens (fn c => c = #"\n") stdout
                val count = List.length lines
              in
                count
              end
          
          val msg = 
            if failed then
              "Failed to search for updates: " ^ stderr
            else
              Int.toString updates_count ^ " updates found"
        in
          {
            changed = false,
            failed = failed,
            msg = SOME msg,
            results = [
              ("updates_found", Int.toString updates_count),
              ("updates", stdout)
            ]
          }
        end
        
      (* Function to install updates *)
      fun install_updates () =
        let
          val cmd = 
            log_cmd ^ 
            "$session = New-Object -ComObject 'Microsoft.Update.Session';" ^
            "$searcher = $session.CreateUpdateSearcher();" ^
            "$result = $searcher.Search('IsInstalled=0');" ^
            "$updates = $result.Updates" ^ category_filter ^ ";" ^
            "if ($updates.Count -eq 0) {" ^
            "  Write-Output '0:No updates found'" ^
            "} else {" ^
            "  $updatesToInstall = New-Object -ComObject 'Microsoft.Update.UpdateColl';" ^
            "  foreach ($update in $updates) {" ^
            "    $updatesToInstall.Add($update) | Out-Null" ^
            "  };" ^
            "  $installer = $session.CreateUpdateInstaller();" ^
            "  $installer.Updates = $updatesToInstall;" ^
            "  $installResult = $installer.Install();" ^
            "  Write-Output \"$($updates.Count):$($installResult.ResultCode)\"" ^
            "}"
            
          val (rc, stdout, stderr) = execute_powershell cmd
          
          val failed = rc <> 0
          
          (* Parse result - format is "count:resultcode" *)
          val (updates_count, success) = 
            if failed then
              (0, false)
            else
              let
                val parts = String.tokens (fn c => c = #":") stdout
                val count = 
                  if List.length parts >= 1 then
                    Option.getOpt(Int.fromString(List.hd parts), 0)
                  else
                    0
                    
                val result_code = 
                  if List.length parts >= 2 then
                    List.nth(parts, 1)
                  else
                    "Failed"
              in
                (count, result_code = "2" orelse result_code = "3") (* 2=Success, 3=SuccessReboot *)
              end
              
          val msg = 
            if failed then
              "Failed to install updates: " ^ stderr
            else if updates_count = 0 then
              "No updates to install"
            else if success then
              Int.toString updates_count ^ " updates installed successfully"
            else
              "Updates installation failed"
        in
          {
            changed = updates_count > 0 andalso success,
            failed = failed orelse (updates_count > 0 andalso not success),
            msg = SOME msg,
            results = [
              ("updates_installed", Int.toString updates_count),
              ("reboot_required", if success andalso stdout = "3" then "true" else "false"),
              ("stdout", stdout),
              ("stderr", stderr)
            ]
          }
        end
    in
      case state of
        "searched" => search_updates()
      | "installed" => install_updates()
      | _ => raise ArgumentError("Invalid state: " ^ state)
    end
    handle e => 
      {
        changed = false,
        failed = true,
        msg = SOME (exnMessage e),
        results = []
      }

  (* Helper functions to convert between arg types for module dispatch *)
  (* Convert generic args list to command_args record *)
  fun convert_to_command_args args =
    let
      val command = 
        case List.find (fn (k, _) => k = "command") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: command")
        
      val chdir = 
        case List.find (fn (k, _) => k = "chdir") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val creates = 
        case List.find (fn (k, _) => k = "creates") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val removes = 
        case List.find (fn (k, _) => k = "removes") args of
          SOME (_, v) => SOME v
        | NONE => NONE
    in
      {command = command, chdir = chdir, creates = creates, removes = removes}
    end

  (* Convert generic args list to file_args record *)
  fun convert_to_file_args args =
    let
      val path = 
        case List.find (fn (k, _) => k = "path") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: path")
        
      val state = 
        case List.find (fn (k, _) => k = "state") args of
          SOME (_, v) => v
        | NONE => "file" (* default *)
        
      val recurse = 
        case List.find (fn (k, _) => k = "recurse") args of
          SOME (_, "true") => true
        | _ => false
        
      val force = 
        case List.find (fn (k, _) => k = "force") args of
          SOME (_, "true") => true
        | _ => false
    in
      {path = path, state = state, recurse = recurse, force = force}
    end

  (* Convert generic args list to copy_args record *)
  fun convert_to_copy_args args =
    let
      val dest = 
        case List.find (fn (k, _) => k = "dest") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: dest")
        
      val src = 
        case List.find (fn (k, _) => k = "src") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val content = 
        case List.find (fn (k, _) => k = "content") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val backup = 
        case List.find (fn (k, _) => k = "backup") args of
          SOME (_, "true") => true
        | _ => false
        
      val force = 
        case List.find (fn (k, _) => k = "force") args of
          SOME (_, "true") => true
        | _ => false
    in
      {src = src, content = content, dest = dest, backup = backup, force = force}
    end

  (* Convert generic args list to service_args record *)
  fun convert_to_service_args args =
    let
      val name = 
        case List.find (fn (k, _) => k = "name") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: name")
        
      val state = 
        case List.find (fn (k, _) => k = "state") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val startup = 
        case List.find (fn (k, _) => k = "startup") args of
          SOME (_, v) => SOME v
        | NONE => NONE
    in
      {name = name, state = state, startup = startup}
    end

  (* Convert generic args list to feature_args record *)
  fun convert_to_feature_args args =
    let
      val name = 
        case List.find (fn (k, _) => k = "name") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: name")
        
      val state = 
        case List.find (fn (k, _) => k = "state") args of
          SOME (_, v) => v
        | NONE => "present" (* default *)
    in
      {name = name, state = state}
    end

  (* Convert generic args list to regedit_args record *)
  fun convert_to_regedit_args args =
    let
      val path = 
        case List.find (fn (k, _) => k = "path") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: path")
        
      val name = 
        case List.find (fn (k, _) => k = "name") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val data = 
        case List.find (fn (k, _) => k = "data") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val type_str = 
        case List.find (fn (k, _) => k = "type") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val state = 
        case List.find (fn (k, _) => k = "state") args of
          SOME (_, v) => v
        | NONE => "present" (* default *)
    in
      {path = path, name = name, data = data, type_str = type_str, state = state}
    end

  (* Convert generic args list to acl_args record *)
  fun convert_to_acl_args args =
    let
      val path = 
        case List.find (fn (k, _) => k = "path") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: path")
        
      val user = 
        case List.find (fn (k, _) => k = "user") args of
          SOME (_, v) => v
        | NONE => raise ArgumentError("Missing required argument: user")
        
      val rights = 
        case List.find (fn (k, _) => k = "rights") args of
          SOME (_, v) => v
        | NONE => "ReadAndExecute" (* default *)
        
      val type_str = 
        case List.find (fn (k, _) => k = "type") args of
          SOME (_, v) => v
        | NONE => "allow" (* default *)
        
      val inherit_flags = 
        case List.find (fn (k, _) => k = "inherit_flags") args of
          SOME (_, v) => SOME v
        | NONE => NONE
    in
      {path = path, user = user, rights = rights, type_str = type_str, inherit_flags = inherit_flags}
    end

  (* Convert generic args list to updates_args record *)
  fun convert_to_updates_args args =
    let
      val categories = 
        case List.find (fn (k, _) => k = "category_names") args of
          SOME (_, v) => SOME (String.tokens (fn c => c = #",") v)
        | NONE => NONE
        
      val log_path = 
        case List.find (fn (k, _) => k = "log_path") args of
          SOME (_, v) => SOME v
        | NONE => NONE
        
      val state = 
        case List.find (fn (k, _) => k = "state") args of
          SOME (_, v) => v
        | NONE => "searched" (* default *)
    in
      {category_names = categories, log_path = log_path, state = state}
    end

  (* Function to dispatch to the appropriate module *)
  fun run_module module_name args =
    case module_name of
      "win_command" => run_win_command (convert_to_command_args args)
    | "win_file" => run_win_file (convert_to_file_args args)
    | "win_copy" => run_win_copy (convert_to_copy_args args)
    | "win_service" => run_win_service (convert_to_service_args args)
    | "win_feature" => run_win_feature (convert_to_feature_args args)
    | "win_regedit" => run_win_regedit (convert_to_regedit_args args)
    | "win_acl" => run_win_acl (convert_to_acl_args args)
    | "win_updates" => run_win_updates (convert_to_updates_args args)
    | _ => raise ModuleError("Unknown module: " ^ module_name)
    
  (* Export convenience functions to match the signature *)
  val win_command = run_win_command
  val win_copy = run_win_copy
  val win_shell = run_win_command  (* Shell is the same as command for Windows *)
  val win_service = run_win_service
  val win_regedit = run_win_regedit
  val win_feature = run_win_feature
  val win_file = run_win_file
  val win_acl = run_win_acl
  val win_updates = run_win_updates
end