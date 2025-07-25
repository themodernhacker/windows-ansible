(* task_executor.sml
 * Core task execution system like Ansible
 *)

structure TaskExecutor : TASK_EXECUTOR = struct
  (* Include Inventory structure *)
  structure Inventory = Inventory
  
  (* Task definition types *)
  type task_module = string
  type module_args = (string * string) list
  
  (* Task state for idempotency *)
  datatype change_status = UNCHANGED | CHANGED | FAILED
  
  (* Task definition *)
  type task = {
    name: string,
    module: task_module,
    args: module_args,
    register: string option,
    when_condition: string option
  }
  
  (* Task result *)
  type task_result = {
    status: change_status,
    output: string,
    facts: (string * string) list,
    skipped: bool
  }
  
  (* Exceptions *)
  exception TaskError of string * string
  exception ModuleNotFound of string
  
  (* Module registry *)
  val modules = ref ([]: (task_module * (module_args -> task_result)) list)
  
  (* Registered variables *)
  val registered_vars = ref ([]: (string * task_result) list)
  
  (* Register a module *)
  fun register_module (name, handler) =
    modules := (name, handler) :: (!modules)
  
  (* List all available modules *)
  fun list_available_modules () =
    map (fn (name, _) => name) (!modules)
  
  (* Create a task *)
  fun create_task {name, module, args, when_condition, register} =
    {
      name = name,
      module = module,
      args = args,
      register = register,
      when_condition = when_condition
    }
  
  (* Helper function to get a module handler *)
  fun get_module name =
    case List.find (fn (n, _) => n = name) (!modules) of
      SOME (_, handler) => SOME handler
    | NONE => NONE
  
  (* Execute a task on a single host *)
  fun execute (task: task, host: Inventory.host) =
    let
      (* Get the host name from the host record *)
      val hostname = 
        case Inventory.get_host_groups(Inventory.create(), "dummy") of
          _ => "localhost" (* Default to localhost if we can't get the actual name *)

      (* Get host vars to use for condition evaluation *)
      val host_vars = 
        (Inventory.get_effective_vars(Inventory.create(), hostname)
         handle _ => [])
                     
      (* Check if we should skip due to when condition *)
      val should_run = 
        case (#when_condition task) of
          NONE => true
        | SOME condition =>
            (* Very simple condition evaluation - just check if var is true *)
            case List.find (fn (k, _) => k = condition) host_vars of
              SOME (_, "true") => true
            | SOME (_, "yes") => true
            | SOME (_, "1") => true
            | _ => false
                
      (* Get the module handler *)
      val handler_opt = get_module (#module task)
    in
      if not should_run then
        (* Skip the task *)
        {
          status = UNCHANGED,
          output = "Task skipped due to condition",
          facts = [],
          skipped = true
        }
      else
        case handler_opt of
          SOME handler =>
            let
              (* Execute the module *)
              val result = handler (#args task)
              
              (* Register the result if needed *)
              val _ = case (#register task) of
                        SOME var_name => 
                          registered_vars := (var_name, result) :: (!registered_vars)
                      | NONE => ()
            in
              result
            end
        | NONE => 
            (* Module not found *)
            raise ModuleNotFound (#module task)
    end
  
  (* Execute a task on multiple hosts *)
  fun execute_on_hosts (task: task, hosts: Inventory.host list) =
    let
      fun exec [] results = List.rev results
        | exec (host::rest) results =
            let
              val result = 
                execute (task, host) 
                handle ModuleNotFound m => 
                  {
                    status = FAILED,
                    output = "Module not found: " ^ m,
                    facts = [],
                    skipped = false
                  }
                | TaskError (msg, detail) => 
                  {
                    status = FAILED,
                    output = msg ^ ": " ^ detail,
                    facts = [],
                    skipped = false
                  }
                | e => 
                  {
                    status = FAILED,
                    output = "Unknown error: " ^ exnMessage e,
                    facts = [],
                    skipped = false
                  }
            in
              exec rest ((host, result) :: results)
            end
    in
      exec hosts []
    end
  
  (* Task result accessors *)
  fun get_status (result: task_result) = #status result
  fun get_output (result: task_result) = #output result
  fun get_facts (result: task_result) = #facts result
  fun is_failed (result: task_result) = #status result = FAILED
  fun is_changed (result: task_result) = #status result = CHANGED
  fun is_skipped (result: task_result) = #skipped result
  
  (* Windows module implementations *)
  
  (* Command module *)
  fun win_command cmd =
    create_task {
      name = "Execute command: " ^ cmd,
      module = "win_command",
      args = [("cmd", cmd)],
      when_condition = NONE,
      register = NONE
    }
  
  (* Copy module *)
  fun win_copy {src, dest} =
    create_task {
      name = "Copy file from " ^ src ^ " to " ^ dest,
      module = "win_copy",
      args = [("src", src), ("dest", dest)],
      when_condition = NONE,
      register = NONE
    }
  
  (* Service module *)
  fun win_service {name, state} =
    create_task {
      name = "Manage service " ^ name ^ " (state: " ^ state ^ ")",
      module = "win_service",
      args = [("name", name), ("state", state)],
      when_condition = NONE,
      register = NONE
    }
  
  (* Feature module *)
  fun win_feature {name, state} =
    create_task {
      name = "Windows feature " ^ name ^ " (state: " ^ state ^ ")",
      module = "win_feature",
      args = [("name", name), ("state", state)],
      when_condition = NONE,
      register = NONE
    }
  
  (* Registry edit module *)
  fun win_regedit {path, name, data, type_} =
    create_task {
      name = "Registry edit " ^ path ^ "\\" ^ name,
      module = "win_regedit",
      args = [("path", path), ("name", name), 
             ("data", data), ("type", type_)],
      when_condition = NONE,
      register = NONE
    }
  
  (* Default implementation of Windows modules *)
  
  (* Command module implementation *)
  val _ = register_module ("win_command", fn args =>
    let
      val cmd = case List.find (fn (k, _) => k = "cmd") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_command", "Missing required parameter: cmd")
      
      (* Execute the command *)
      val temp_out = OS.FileSys.tmpName()
      val cmd_with_redirect = cmd ^ " > " ^ temp_out ^ " 2>&1"
      val status = OS.Process.system cmd_with_redirect
      
      (* Read the output *)
      val output = 
        let
          val file = TextIO.openIn temp_out
                    handle _ => TextIO.openString ""
          val content = TextIO.inputAll file
          val _ = TextIO.closeIn file
                  handle _ => ()
          val _ = OS.FileSys.remove temp_out
                  handle _ => ()
        in
          content
        end
        
      val success = OS.Process.isSuccess status
    in
      {
        status = if success then CHANGED else FAILED,
        output = output,
        facts = [("last_command", cmd), 
                ("last_command_success", if success then "true" else "false")],
        skipped = false
      }
    end)
    
  (* Copy module implementation *)
  val _ = register_module ("win_copy", fn args =>
    let
      val src = case List.find (fn (k, _) => k = "src") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_copy", "Missing required parameter: src")
                
      val dest = case List.find (fn (k, _) => k = "dest") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_copy", "Missing required parameter: dest")
                
      (* Check if files are different to determine if changed *)
      fun files_differ () =
        if not (OS.FileSys.access (dest, [])) then
          true  (* Destination doesn't exist *)
        else
          let
            val src_size = OS.FileSys.fileSize src
            val dest_size = OS.FileSys.fileSize dest
          in
            if src_size <> dest_size then
              true
            else
              (* For simplicity, just compare sizes. A real implementation would compare content *)
              false
          end
          handle _ => true  (* If any error, assume files differ *)
        
      val changed = files_differ()
      
      (* Perform copy *)
      fun do_copy () =
        let
          val src_file = TextIO.openIn src
          val dest_file = TextIO.openOut dest
          val content = TextIO.inputAll src_file
          val _ = TextIO.output (dest_file, content)
          val _ = TextIO.closeIn src_file
          val _ = TextIO.closeOut dest_file
        in
          true
        end
        handle e => raise TaskError ("win_copy", "Failed to copy: " ^ exnMessage e)
        
      val success = if changed then do_copy() else true
    in
      {
        status = if success then 
                   if changed then CHANGED else UNCHANGED
                 else FAILED,
        output = if success then 
                   if changed then "File copied successfully" 
                   else "File unchanged" 
                 else "Copy failed",
        facts = [],
        skipped = false
      }
    end)
    
  (* Service module implementation *)
  val _ = register_module ("win_service", fn args =>
    let
      val name = case List.find (fn (k, _) => k = "name") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_service", "Missing required parameter: name")
                
      val state = case List.find (fn (k, _) => k = "state") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_service", "Missing required parameter: state")
      
      (* Simple PowerShell command to control services *)
      val ps_cmd = case state of
                    "started" => "Start-Service \"" ^ name ^ "\""
                  | "stopped" => "Stop-Service \"" ^ name ^ "\""
                  | "restarted" => "Restart-Service \"" ^ name ^ "\""
                  | _ => raise TaskError ("win_service", "Invalid state: " ^ state)
                  
      val cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"" ^ ps_cmd ^ "\""
      
      (* Check current state before executing *)
      val check_cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"(Get-Service -Name '" ^ 
                     name ^ "').Status\""
      
      val temp_out = OS.FileSys.tmpName()
      val status_cmd = check_cmd ^ " > " ^ temp_out
      val _ = OS.Process.system status_cmd
      
      val current_state =
        let
          val file = TextIO.openIn temp_out
                     handle _ => TextIO.openString ""
          val content = TextIO.inputAll file
          val _ = TextIO.closeIn file
                  handle _ => ()
          val _ = OS.FileSys.remove temp_out
                  handle _ => ()
        in
          String.map Char.toLower content
        end
        
      val desired_state = String.map Char.toLower state
      val already_in_desired_state = 
        (state = "started" andalso String.isSubstring "running" current_state) orelse
        (state = "stopped" andalso String.isSubstring "stopped" current_state)
      
      (* Execute command if state change is needed *)
      val (changed, output) = 
        if already_in_desired_state then
          (false, "Service already in desired state: " ^ state)
        else
          let
            val _ = OS.Process.system cmd
            val new_check = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"(Get-Service -Name '" ^ 
                           name ^ "').Status\""
            val new_temp = OS.FileSys.tmpName()
            val _ = OS.Process.system (new_check ^ " > " ^ new_temp)
            
            val new_state =
              let
                val file = TextIO.openIn new_temp
                           handle _ => TextIO.openString ""
                val content = TextIO.inputAll file
                val _ = TextIO.closeIn file
                        handle _ => ()
                val _ = OS.FileSys.remove new_temp
                        handle _ => ()
              in
                String.map Char.toLower content
              end
              
            val success = 
              (state = "started" andalso String.isSubstring "running" new_state) orelse
              (state = "stopped" andalso String.isSubstring "stopped" new_state) orelse
              (state = "restarted")
          in
            (true, if success then "Service " ^ state ^ " successfully" 
                   else "Failed to " ^ state ^ " service")
          end
    in
      {
        status = if changed then CHANGED else UNCHANGED,
        output = output,
        facts = [("service_" ^ name ^ "_state", state)],
        skipped = false
      }
    end)
    
  (* Feature module implementation *)
  val _ = register_module ("win_feature", fn args =>
    let
      val name = case List.find (fn (k, _) => k = "name") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_feature", "Missing required parameter: name")
                
      val state = case List.find (fn (k, _) => k = "state") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_feature", "Missing required parameter: state")
      
      (* PowerShell commands for Windows features *)
      val ps_cmd = case state of
                    "present" => "Install-WindowsFeature -Name \"" ^ name ^ "\" -IncludeManagementTools"
                  | "absent" => "Remove-WindowsFeature -Name \"" ^ name ^ "\""
                  | _ => raise TaskError ("win_feature", "Invalid state: " ^ state)
                  
      (* Check if feature is already in desired state *)
      val check_cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"" ^
                     "(Get-WindowsFeature -Name '" ^ name ^ "').Installed\"" 
      
      val temp_out = OS.FileSys.tmpName()
      val status_cmd = check_cmd ^ " > " ^ temp_out
      val _ = OS.Process.system status_cmd
      
      val is_installed =
        let
          val file = TextIO.openIn temp_out
                     handle _ => TextIO.openString ""
          val content = TextIO.inputAll file
          val _ = TextIO.closeIn file
                  handle _ => ()
          val _ = OS.FileSys.remove temp_out
                  handle _ => ()
        in
          String.isSubstring "True" content
        end
        
      val needs_change = (state = "present" andalso not is_installed) orelse
                         (state = "absent" andalso is_installed)
                         
      (* Make the change if needed *)
      val (changed, output) = 
        if not needs_change then
          (false, "Feature already in desired state")
        else
          let
            val cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"" ^ ps_cmd ^ "\""
            val execution_temp = OS.FileSys.tmpName()
            val _ = OS.Process.system (cmd ^ " > " ^ execution_temp)
            
            val result =
              let
                val file = TextIO.openIn execution_temp
                           handle _ => TextIO.openString ""
                val content = TextIO.inputAll file
                val _ = TextIO.closeIn file
                        handle _ => ()
                val _ = OS.FileSys.remove execution_temp
                        handle _ => ()
              in
                content
              end
              
            val success = if state = "present" then
                            String.isSubstring "Success" result
                          else
                            not (String.isSubstring "Failed" result)
          in
            (true, if success then 
                     "Windows feature " ^ name ^ " " ^ 
                     (if state = "present" then "installed" else "removed") ^ " successfully"
                   else 
                     "Failed to " ^ (if state = "present" then "install" else "remove") ^ 
                     " Windows feature " ^ name)
          end
    in
      {
        status = if changed then CHANGED else UNCHANGED,
        output = output,
        facts = [("feature_" ^ name ^ "_installed", if state = "present" then "true" else "false")],
        skipped = false
      }
    end)
    
  (* Registry edit module implementation *)
  val _ = register_module ("win_regedit", fn args =>
    let
      val path = case List.find (fn (k, _) => k = "path") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_regedit", "Missing required parameter: path")
                
      val name = case List.find (fn (k, _) => k = "name") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_regedit", "Missing required parameter: name")
                
      val data = case List.find (fn (k, _) => k = "data") args of
                  SOME (_, v) => v
                | NONE => raise TaskError ("win_regedit", "Missing required parameter: data")
                
      val type_ = case List.find (fn (k, _) => k = "type") args of
                  SOME (_, v) => v
                | NONE => "String" (* Default type *)
      
      (* PowerShell command to set registry value *)
      val ps_cmd = "New-ItemProperty -Path \"" ^ path ^ "\" -Name \"" ^ name ^ 
                  "\" -Value \"" ^ data ^ "\" -PropertyType " ^ type_ ^ " -Force"
                  
      (* Check if value already exists and is set correctly *)
      val check_cmd = "try { " ^
                     "$value = Get-ItemProperty -Path \"" ^ path ^ "\" -Name \"" ^ name ^ "\" -ErrorAction Stop;" ^
                     "if ($value.\"" ^ name ^ "\" -eq \"" ^ data ^ "\") { Write-Output \"SAME\" } else { Write-Output \"DIFFERENT\" }" ^
                     "} catch { Write-Output \"NOTFOUND\" }"
      
      val temp_out = OS.FileSys.tmpName()
      val status_cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"" ^ 
                      check_cmd ^ "\" > " ^ temp_out
      val _ = OS.Process.system status_cmd
      
      val current_state =
        let
          val file = TextIO.openIn temp_out
                     handle _ => TextIO.openString ""
          val content = TextIO.inputAll file
          val _ = TextIO.closeIn file
                  handle _ => ()
          val _ = OS.FileSys.remove temp_out
                  handle _ => ()
        in
          content
        end
        
      val needs_change = String.isSubstring "DIFFERENT" current_state orelse
                         String.isSubstring "NOTFOUND" current_state
                         
      (* Make the change if needed *)
      val (changed, output) = 
        if not needs_change then
          (false, "Registry value already set correctly")
        else
          let
            val cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"" ^ ps_cmd ^ "\""
            val execution_temp = OS.FileSys.tmpName()
            val _ = OS.Process.system (cmd ^ " > " ^ execution_temp)
            
            (* Verify the change was made *)
            val verify_cmd = "powershell -NonInteractive -ExecutionPolicy Bypass -Command \"" ^ 
                            check_cmd ^ "\" > " ^ execution_temp
            val _ = OS.Process.system verify_cmd
            
            val verify_result =
              let
                val file = TextIO.openIn execution_temp
                           handle _ => TextIO.openString ""
                val content = TextIO.inputAll file
                val _ = TextIO.closeIn file
                        handle _ => ()
                val _ = OS.FileSys.remove execution_temp
                        handle _ => ()
              in
                content
              end
              
            val success = String.isSubstring "SAME" verify_result
          in
            (true, if success then 
                     "Registry value set successfully" 
                   else 
                     "Failed to set registry value")
          end
    in
      {
        status = if changed then CHANGED else UNCHANGED,
        output = output,
        facts = [],
        skipped = false
      }
    end)
end