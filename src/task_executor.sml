(* task_executor.sml
 * Implementation of Ansible-like task execution system
 *)

structure TaskExecutor : TASK_EXECUTOR = struct
  structure Inventory = Inventory
  
  (* Task definition types *)
  type task_module = string
  type module_args = (string * string) list
  
  (* Result and task types *)
  datatype change_status = UNCHANGED | CHANGED | FAILED
  
  type task_result = {
    status: change_status,
    output: string,
    facts: (string * string) list,
    skipped: bool
  }
  
  type task = {
    name: string,
    module: task_module,
    args: module_args,
    when_condition: string option,
    register: string option
  }
  
  (* Exceptions *)
  exception TaskError of string * string
  exception ModuleNotFound of string
  
  (* Module registry *)
  val module_registry : (string, module_args -> task_result) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (50, ModuleNotFound "Module not found")
    
  (* Variables and facts storage *)
  val facts_registry : (string, (string * string) list) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (100, Fail "Fact not found")
    
  (* Task creation *)
  fun create_task {name, module, args, when_condition, register} = {
    name = name,
    module = module,
    args = args,
    when_condition = when_condition,
    register = register
  }
  
  (* Evaluate a condition with available facts *)
  fun evaluate_condition condition host_facts =
    let
      (* Very simplified condition evaluation - in real implementation would use a proper expression parser *)
      val result = 
        case condition of
          "true" => true
        | "false" => false
        | _ => 
          let
            (* Check if it's a variable comparison *)
            val parts = String.tokens (fn c => c = #" ") condition
          in
            case parts of
              [var, "==", value] => 
                (case List.find (fn (k, _) => k = var) host_facts of
                  SOME (_, v) => v = value
                | NONE => false)
            | [var, "!=", value] => 
                (case List.find (fn (k, _) => k = var) host_facts of
                  SOME (_, v) => v <> value
                | NONE => true)
            | _ => false
          end
    in
      result
    end
  
  (* Execute a task on a specific host *)
  fun execute (task, host) =
    let
      val host_name = #name host
      val host_vars = Inventory.get_effective_vars (Inventory.create(), host_name)
      
      (* Check if task should be skipped due to condition *)
      val should_skip = 
        case #when_condition task of
          SOME condition => not (evaluate_condition condition host_vars)
        | NONE => false
        
      (* If skipped, return immediately *)
      val result = 
        if should_skip then
          {
            status = UNCHANGED,
            output = "Task skipped due to condition",
            facts = [],
            skipped = true
          }
        else
          (* Execute the module *)
          let
            val module_fn = 
              HashTable.lookup module_registry (#module task)
              handle _ => raise ModuleNotFound (#module task)
              
            val result = module_fn (#args task)
            
            (* Store result in registry if needed *)
            val _ = 
              case #register task of
                SOME var_name => 
                  HashTable.insert facts_registry (var_name, #facts result)
              | NONE => ()
          in
            result
          end
    in
      result
    end
  
  (* Execute a task on multiple hosts *)
  fun execute_on_hosts (task, hosts) =
    map (fn host => (host, execute(task, host))) hosts
  
  (* Result functions *)
  fun get_status result = #status result
  fun get_output result = #output result
  fun get_facts result = #facts result
  fun is_failed result = #status result = FAILED
  fun is_changed result = #status result = CHANGED
  fun is_skipped result = #skipped result
  
  (* Module registration *)
  fun register_module (name, handler) =
    HashTable.insert module_registry (name, handler)
    
  fun list_available_modules () =
    HashTable.foldi (fn (name, _, acc) => name :: acc) [] module_registry
  
  (* Convenience functions for common modules *)
  fun win_command cmd =
    create_task {
      name = "Execute command",
      module = "win_command",
      args = [("command", cmd)],
      when_condition = NONE,
      register = NONE
    }
    
  fun win_copy {src, dest} =
    create_task {
      name = "Copy file",
      module = "win_copy",
      args = [("src", src), ("dest", dest), ("backup", "true")],
      when_condition = NONE,
      register = NONE
    }
    
  fun win_service {name, state} =
    create_task {
      name = "Manage service",
      module = "win_service",
      args = [("name", name), ("state", state)],
      when_condition = NONE,
      register = NONE
    }
    
  fun win_feature {name, state} =
    create_task {
      name = "Manage Windows feature",
      module = "win_feature",
      args = [("name", name), ("state", state), ("restart_if_required", "false")],
      when_condition = NONE,
      register = NONE
    }
    
  fun win_regedit {path, name, data, type_} =
    create_task {
      name = "Manage registry",
      module = "win_regedit",
      args = [("path", path), ("name", name), ("data", data), 
              ("type", type_), ("state", "present")],
      when_condition = NONE,
      register = NONE
    }

  (* Initialize common modules *)
  val _ = 
    let
      (* Helper function to convert WindowsModule.module_result to task_result *)
      fun convert_result (mod_result : WindowsModule.module_result) : task_result =
        {
          status = if #failed mod_result then FAILED
                   else if #changed mod_result then CHANGED
                   else UNCHANGED,
          output = case #stdout mod_result of
                     SOME s => s
                   | NONE => #msg mod_result,
          facts = #output mod_result,
          skipped = false
        }
        
      (* Register win_command module *)
      fun win_command_handler args =
        let
          val command = case List.find (fn (k, _) => k = "command") args of
                         SOME (_, v) => v
                       | NONE => raise TaskError("win_command", "command parameter required")
                       
          val chdir = Option.map #2 (List.find (fn (k, _) => k = "chdir") args)
          val creates = Option.map #2 (List.find (fn (k, _) => k = "creates") args)
          val removes = Option.map #2 (List.find (fn (k, _) => k = "removes") args)
          
          val mod_result = WindowsModule.win_command {
            command = command,
            chdir = chdir,
            creates = creates,
            removes = removes
          }
        in
          convert_result mod_result
        end
        
      (* Register win_copy module *)
      fun win_copy_handler args =
        let
          val src = case List.find (fn (k, _) => k = "src") args of
                      SOME (_, v) => v
                    | NONE => raise TaskError("win_copy", "src parameter required")
                    
          val dest = case List.find (fn (k, _) => k = "dest") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_copy", "dest parameter required")
                     
          val backup = case List.find (fn (k, _) => k = "backup") args of
                         SOME (_, "true") => true
                       | SOME (_, "yes") => true
                       | _ => false
                       
          val mod_result = WindowsModule.win_copy {
            src = src,
            dest = dest,
            backup = backup
          }
        in
          convert_result mod_result
        end
        
      (* Register win_service module *)
      fun win_service_handler args =
        let
          val name = case List.find (fn (k, _) => k = "name") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_service", "name parameter required")
                     
          val display_name = Option.map #2 (List.find (fn (k, _) => k = "display_name") args)
          val path = Option.map #2 (List.find (fn (k, _) => k = "path") args)
          val state = Option.map #2 (List.find (fn (k, _) => k = "state") args)
          val startup_mode = Option.map #2 (List.find (fn (k, _) => k = "startup_mode") args)
                    
          val mod_result = WindowsModule.win_service {
            name = name,
            display_name = display_name,
            path = path,
            state = state,
            startup_mode = startup_mode
          }
        in
          convert_result mod_result
        end

      (* Register win_feature module *)
      fun win_feature_handler args =
        let
          val name = case List.find (fn (k, _) => k = "name") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_feature", "name parameter required")
                     
          val state = case List.find (fn (k, _) => k = "state") args of
                        SOME (_, v) => v
                      | NONE => "present" (* Default value *)
                      
          val restart = case List.find (fn (k, _) => k = "restart_if_required") args of
                          SOME (_, "true") => true
                        | SOME (_, "yes") => true
                        | _ => false
                          
          val mod_result = WindowsModule.win_feature {
            name = name,
            state = state,
            restart_if_required = restart
          }
        in
          convert_result mod_result
        end

      (* Register win_regedit module *)
      fun win_regedit_handler args =
        let
          val path = case List.find (fn (k, _) => k = "path") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_regedit", "path parameter required")
                     
          val name = case List.find (fn (k, _) => k = "name") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_regedit", "name parameter required")
                     
          val data = Option.map #2 (List.find (fn (k, _) => k = "data") args)
          val type_ = Option.map #2 (List.find (fn (k, _) => k = "type") args)
          val state = case List.find (fn (k, _) => k = "state") args of
                        SOME (_, v) => v
                      | NONE => "present" (* Default value *)
                       
          val mod_result = WindowsModule.win_regedit {
            path = path,
            name = name,
            data = data,
            type_ = type_,
            state = state
          }
        in
          convert_result mod_result
        end

      (* Register win_file module *)
      fun win_file_handler args =
        let
          val path = case List.find (fn (k, _) => k = "path") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_file", "path parameter required")
                     
          val state = case List.find (fn (k, _) => k = "state") args of
                        SOME (_, v) => v
                      | NONE => "present" (* Default value *)
                      
          (* Process attributes if any *)
          fun find_attr_value prefix k =
            Option.map (fn (_, v) => v = "true" orelse v = "yes") 
                       (List.find (fn (key, _) => key = prefix ^ k) args)
            
          val attrs = 
            let
              val readonly = find_attr_value "attributes_" "readonly"
              val hidden = find_attr_value "attributes_" "hidden"
              val archive = find_attr_value "attributes_" "archive"
              val system = find_attr_value "attributes_" "system"
              
              val attr_list = []
              val attr_list = case readonly of SOME v => ("readonly", v) :: attr_list | NONE => attr_list
              val attr_list = case hidden of SOME v => ("hidden", v) :: attr_list | NONE => attr_list
              val attr_list = case archive of SOME v => ("archive", v) :: attr_list | NONE => attr_list
              val attr_list = case system of SOME v => ("system", v) :: attr_list | NONE => attr_list
            in
              if null attr_list then NONE else SOME attr_list
            end
                       
          val mod_result = WindowsModule.win_file {
            path = path,
            state = state,
            attributes = attrs
          }
        in
          convert_result mod_result
        end

      (* Register win_acl module *)
      fun win_acl_handler args =
        let
          val path = case List.find (fn (k, _) => k = "path") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_acl", "path parameter required")
                     
          val user = case List.find (fn (k, _) => k = "user") args of
                       SOME (_, v) => v
                     | NONE => raise TaskError("win_acl", "user parameter required")
                     
          val rights = case List.find (fn (k, _) => k = "rights") args of
                         SOME (_, v) => v
                       | NONE => "ReadAndExecute" (* Default value *)
                       
          val type_ = case List.find (fn (k, _) => k = "type") args of
                        SOME (_, v) => v
                      | NONE => "Allow" (* Default value *)
                      
          val state = case List.find (fn (k, _) => k = "state") args of
                        SOME (_, v) => v
                      | NONE => "present" (* Default value *)
                       
          val mod_result = WindowsModule.win_acl {
            path = path,
            user = user,
            rights = rights,
            type_ = type_,
            state = state
          }
        in
          convert_result mod_result
        end
    in
      register_module("win_command", win_command_handler);
      register_module("win_copy", win_copy_handler);
      register_module("win_service", win_service_handler);
      register_module("win_feature", win_feature_handler);
      register_module("win_regedit", win_regedit_handler);
      register_module("win_file", win_file_handler);
      register_module("win_acl", win_acl_handler);
      ()
    end
end