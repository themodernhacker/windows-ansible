(* playbook.sml
 * Ansible-compatible playbook structure
 *)

structure Playbook : PLAYBOOK = struct
  (* Import required structures *)
  structure Inventory = Inventory
  structure TaskExecutor = TaskExecutor
  
  (* Type definitions *)
  type vars = (string * string) list
  
  (* Variable scope type - for handling variable precedence *)
  datatype var_scope = GLOBAL | PLAY | HOST | TASK
  
  (* Handler type *)
  type handler = {
    name: string,
    task: TaskExecutor.task,
    triggered: bool ref
  }
  
  (* Play type *)
  type play = {
    name: string,
    hosts: string,  (* host pattern *)
    tasks: TaskExecutor.task list,
    handlers: handler list,
    vars: vars,
    tags: string list
  }
  
  (* Playbook type *)
  type playbook = {
    name: string,
    plays: play list
  }
  
  (* Play result type *)
  type play_result = {
    play_name: string,
    host_results: (string * (string * TaskExecutor.change_status) list) list,
    ok_count: int,
    changed_count: int,
    unreachable_count: int,
    failed_count: int,
    skipped_count: int
  }
  
  (* Exceptions *)
  exception PlaybookError of string
  exception PlayError of string * string  (* play name, error message *)
  
  (* Create a handler *)
  fun create_handler {name, task} =
    {
      name = name,
      task = task,
      triggered = ref false
    }
  
  (* Create a play *)
  fun create_play {name, hosts, tasks, handlers, vars, tags} =
    {
      name = name,
      hosts = hosts,
      tasks = tasks,
      handlers = handlers,
      vars = vars,
      tags = tags
    }
  
  (* Create an empty playbook *)
  fun create_playbook name =
    {
      name = name,
      plays = []
    }
  
  (* Add a play to a playbook *)
  fun add_play (pb: playbook, p: play) : playbook =
    {
      name = #name pb,
      plays = p :: (#plays pb)
    }
  
  (* Custom string trim function *)
  fun string_trim s =
    let
      val chars = explode s
      
      (* Skip leading whitespace *)
      fun skipLeading [] = []
        | skipLeading (c::cs) = 
            if Char.isSpace c then skipLeading cs else c::cs
            
      (* Skip trailing whitespace *)
      fun skipTrailing [] = []
        | skipTrailing lst =
            case List.rev lst of
              [] => []
            | (c::cs) => if Char.isSpace c 
                         then List.rev (skipTrailing (List.rev cs))
                         else lst
    in
      implode (skipTrailing (skipLeading chars))
    end
    
  (* Custom string position function - finds position of a substring *)
  fun string_position substr s =
    let
      val n = String.size substr
      val m = String.size s
      
      fun loop i =
        if i + n > m then NONE
        else if String.substring(s, i, n) = substr then SOME i
        else loop (i + 1)
    in
      if n = 0 orelse m = 0 then NONE
      else loop 0
    end
  
  (* Convert string to boolean *)
  fun str_to_bool s =
    let val lower = String.map Char.toLower s
    in
      lower = "true" orelse lower = "yes" orelse lower = "1"
    end
  
  (* Load a playbook from a YAML-like file - simplified parser *)
  fun load_file filename =
    let
      val file = TextIO.openIn filename
                 handle _ => raise PlaybookError ("Cannot open playbook file: " ^ filename)
                 
      val content = TextIO.inputAll file
      val _ = TextIO.closeIn file
      
      val lines = String.tokens (fn c => c = #"\n") content
      
      (* Simplified YAML parser state *)
      val current_playbook = ref (create_playbook "")
      val current_play = ref NONE
      val current_task = ref NONE
      val current_handler = ref NONE
      val in_tasks = ref false
      val in_handlers = ref false
      val in_vars = ref false
      val indent_level = ref 0
      
      (* Process each line *)
      fun process_line [] = ()
        | process_line (line::rest) =
            if String.size line = 0 orelse String.sub(line, 0) = #"#" then
              (* Skip empty lines and comments *)
              process_line rest
            else
              let
                val trimmed = string_trim line
                
                (* Count leading spaces for indent *)
                fun count_spaces i =
                  if i < String.size line andalso Char.isSpace (String.sub(line, i)) then
                    count_spaces (i + 1)
                  else
                    i
                    
                val new_indent = count_spaces 0
                val _ = indent_level := new_indent
                
                (* Check for playbook level definitions *)
                val _ = 
                  if String.isSubstring "name:" trimmed then
                    let
                      val name_value = string_trim(String.extract(trimmed, 5, NONE))
                    in
                      if !current_play = NONE then
                        current_playbook := create_playbook name_value
                      else
                        ()
                    end
                  else ()
                  
                (* Check for play level definitions *)
                val _ =
                  if String.isSubstring "- hosts:" trimmed then
                    let
                      val hosts_value = string_trim(String.extract(trimmed, 8, NONE))
                      val new_play = {
                        name = "", (* Will be filled in later *)
                        hosts = hosts_value,
                        tasks = [],
                        handlers = [],
                        vars = [],
                        tags = []
                      }
                    in
                      current_play := SOME new_play;
                      in_tasks := false;
                      in_handlers := false;
                      in_vars := false
                    end
                  else if String.isSubstring "  name:" trimmed andalso !current_play <> NONE then
                    case !current_play of
                      SOME play =>
                        let
                          val name_value = string_trim(String.extract(trimmed, 7, NONE))
                        in
                          current_play := SOME {
                            name = name_value,
                            hosts = #hosts play,
                            tasks = #tasks play,
                            handlers = #handlers play,
                            vars = #vars play,
                            tags = #tags play
                          }
                        end
                    | NONE => ()
                  else ()
                  
                (* Check for section markers *)
                val _ =
                  if String.isSubstring "  tasks:" trimmed then
                    (in_tasks := true; in_handlers := false; in_vars := false)
                  else if String.isSubstring "  handlers:" trimmed then
                    (in_tasks := false; in_handlers := true; in_vars := false)
                  else if String.isSubstring "  vars:" trimmed then
                    (in_tasks := false; in_handlers := false; in_vars := true)
                  else ()
                  
                (* Process tasks *)
                val _ =
                  if !in_tasks andalso String.isSubstring "    - name:" trimmed andalso !current_play <> NONE then
                    let
                      val task_name = string_trim(String.extract(trimmed, 11, NONE))
                      val task_ref = ref {
                        name = task_name,
                        module = "",
                        args = [],
                        register = NONE,
                        when_condition = NONE
                      }
                      val _ = current_task := SOME task_ref
                    in
                      ()
                    end
                  else if !in_tasks andalso !current_task <> NONE andalso String.isSubstring "      " trimmed then
                    (* Task details *)
                    case !current_task of
                      SOME task_ref =>
                        let
                          val module_line = string_trim trimmed
                        in
                          if String.isSubstring ":" module_line then
                            let
                              val colon_pos = valOf (string_position ":" module_line)
                              val module_name = string_trim(String.substring(module_line, 0, colon_pos))
                              val arg_str = string_trim(String.extract(module_line, colon_pos + 1, NONE))
                              val arg_parts = String.tokens (fn c => c = #" " orelse c = #"=") arg_str
                              val args = 
                                if length arg_parts >= 2 then
                                  [(hd arg_parts, hd(tl arg_parts))]
                                else
                                  []
                              
                              val new_task = TaskExecutor.create_task {
                                name = (#name (!task_ref)),
                                module = module_name,
                                args = args,
                                register = NONE,
                                when_condition = NONE
                              }
                            in
                              case !current_play of
                                SOME play =>
                                  current_play := SOME {
                                    name = #name play,
                                    hosts = #hosts play,
                                    tasks = new_task :: (#tasks play),
                                    handlers = #handlers play,
                                    vars = #vars play,
                                    tags = #tags play
                                  }
                              | NONE => ()
                            end
                          else
                            ()
                        end
                    | NONE => ()
                  else ()
                  
                (* Process handlers *)
                val _ =
                  if !in_handlers andalso String.isSubstring "    - name:" trimmed andalso !current_play <> NONE then
                    let
                      val handler_name = string_trim(String.extract(trimmed, 11, NONE))
                      val handler_task = TaskExecutor.create_task {
                        name = handler_name,
                        module = "",
                        args = [],
                        register = NONE,
                        when_condition = NONE
                      }
                      val handler = create_handler {
                        name = handler_name,
                        task = handler_task
                      }
                    in
                      current_handler := SOME handler
                    end
                  else if !in_handlers andalso !current_handler <> NONE andalso String.isSubstring "      " trimmed then
                    (* Handler details *)
                    case !current_handler of
                      SOME handler =>
                        let
                          val module_line = string_trim trimmed
                        in
                          if String.isSubstring ":" module_line then
                            let
                              val colon_pos = valOf (string_position ":" module_line)
                              val module_name = string_trim(String.substring(module_line, 0, colon_pos))
                              val arg_str = string_trim(String.extract(module_line, colon_pos + 1, NONE))
                              val arg_parts = String.tokens (fn c => c = #" " orelse c = #"=") arg_str
                              val args = 
                                if length arg_parts >= 2 then
                                  [(hd arg_parts, hd(tl arg_parts))]
                                else
                                  []
                              
                              val handler_task = TaskExecutor.create_task {
                                name = (#name handler),
                                module = module_name,
                                args = args,
                                register = NONE,
                                when_condition = NONE
                              }
                              
                              val new_handler = create_handler {
                                name = #name handler,
                                task = handler_task
                              }
                            in
                              case !current_play of
                                SOME play =>
                                  current_play := SOME {
                                    name = #name play,
                                    hosts = #hosts play,
                                    tasks = #tasks play,
                                    handlers = new_handler :: (#handlers play),
                                    vars = #vars play,
                                    tags = #tags play
                                  }
                              | NONE => ()
                            end
                          else
                            ()
                        end
                    | NONE => ()
                  else ()
                  
                (* Process vars *)
                val _ =
                  if !in_vars andalso String.isSubstring "    " trimmed andalso !current_play <> NONE then
                    let
                      val var_line = string_trim trimmed
                    in
                      if String.isSubstring ":" var_line then
                        let
                          val colon_pos = valOf (string_position ":" var_line)
                          val var_name = string_trim(String.substring(var_line, 0, colon_pos))
                          val var_value = string_trim(String.extract(var_line, colon_pos + 1, NONE))
                        in
                          case !current_play of
                            SOME play =>
                              current_play := SOME {
                                name = #name play,
                                hosts = #hosts play,
                                tasks = #tasks play,
                                handlers = #handlers play,
                                vars = (var_name, var_value) :: (#vars play),
                                tags = #tags play
                              }
                          | NONE => ()
                        end
                      else
                        ()
                    end
                  else ()
                  
                (* Check for end of play and add it to playbook *)
                val _ =
                  if new_indent = 0 andalso !current_play <> NONE then
                    let
                      val _ = case !current_play of
                                SOME play =>
                                  current_playbook := add_play (!current_playbook, play)
                              | NONE => ()
                      val _ = current_play := NONE
                      val _ = current_task := NONE
                      val _ = current_handler := NONE
                    in
                      ()
                    end
                  else ()
              in
                process_line rest
              end
    in
      (* Return the parsed playbook *)
      !current_playbook
    end
  
  (* Save a playbook to a YAML file *)
  fun save_file (playbook: playbook, filename: string) =
    let
      val file = TextIO.openOut filename
                 handle _ => raise PlaybookError ("Cannot open file for writing: " ^ filename)
                 
      (* Write playbook name *)
      val _ = TextIO.output(file, "name: " ^ (#name playbook) ^ "\n\n")
      
      (* Helper for writing plays *)
      fun write_play (play: play) =
        let
          (* Write play header *)
          val _ = TextIO.output(file, "- hosts: " ^ (#hosts play) ^ "\n")
          val _ = TextIO.output(file, "  name: " ^ (#name play) ^ "\n")
          
          (* Write vars section *)
          val _ = if not (null (#vars play)) then
                    let
                      val _ = TextIO.output(file, "  vars:\n")
                      val _ = app (fn (k, v) => 
                                    TextIO.output(file, "    " ^ k ^ ": " ^ v ^ "\n")) 
                                 (#vars play)
                    in
                      ()
                    end
                  else
                    ()
                    
          (* Write tasks section *)
          val _ = if not (null (#tasks play)) then
                    let
                      val _ = TextIO.output(file, "  tasks:\n")
                      val _ = app (fn task =>
                                    let
                                      val _ = TextIO.output(file, "    - name: " ^ (#name task) ^ "\n")
                                      val _ = TextIO.output(file, "      " ^ (#module task) ^ ":")
                                      val _ = app (fn (k, v) => 
                                                   TextIO.output(file, " " ^ k ^ "=" ^ v)) 
                                                (#args task)
                                      val _ = TextIO.output(file, "\n")
                                    in
                                      ()
                                    end)
                                  (#tasks play)
                    in
                      ()
                    end
                  else
                    ()
                    
          (* Write handlers section *)
          val _ = if not (null (#handlers play)) then
                    let
                      val _ = TextIO.output(file, "  handlers:\n")
                      val _ = app (fn (handler: handler) =>
                                    let
                                      val task = #task handler
                                      val _ = TextIO.output(file, "    - name: " ^ (#name handler) ^ "\n")
                                      val _ = TextIO.output(file, "      " ^ (#module task) ^ ":")
                                      val _ = app (fn (k, v) => 
                                                   TextIO.output(file, " " ^ k ^ "=" ^ v)) 
                                                (#args task)
                                      val _ = TextIO.output(file, "\n")
                                    in
                                      ()
                                    end)
                                  (#handlers play)
                    in
                      ()
                    end
                  else
                    ()
                    
          (* Add spacing between plays *)
          val _ = TextIO.output(file, "\n")
        in
          ()
        end
        
      (* Write all plays *)
      val _ = app write_play (#plays playbook)
      
      (* Close the file *)
      val _ = TextIO.closeOut file
    in
      ()
    end
  
  (* Match hosts against a pattern *)
  fun match_host_pattern (pattern: string, host: string) =
    if pattern = "all" then
      true
    else if pattern = host then
      true
    else if String.isSubstring "*" pattern then
      (* Very simple wildcard matching - real Ansible uses more complex patterns *)
      let
        val parts = String.tokens (fn c => c = #"*") pattern
        fun check_parts [] _ = true
          | check_parts (p::ps) s =
              if String.isSubstring p s then
                check_parts ps s
              else
                false
      in
        check_parts parts host
      end
    else
      false
  
  (* Get matching hosts from inventory using a host pattern *)
  fun get_matching_hosts (inventory, pattern) =
    let
      val all_hosts = Inventory.list_hosts inventory
      
      fun matches [] = []
        | matches (h::hs) =
            if match_host_pattern (pattern, h) then
              h :: matches hs
            else
              matches hs
    in
      matches all_hosts
    end
  
  (* Execute a play *)
  fun execute_play (play: play, inventory: Inventory.inventory) =
    let
      (* Create a temporary inventory with just the hosts for this play *)
      val temp_inventory = Inventory.create()
      
      (* Get matching hosts *)
      val host_pattern = #hosts play
      
      (* Extract host group if the pattern refers to a group *)
      val is_group = 
        (case Inventory.get_group(inventory, host_pattern) of
           SOME _ => true
         | NONE => false)
         handle _ => false
         
      (* Get hosts either directly or from group *)
      val matching_hosts = 
        if is_group then
          (Inventory.get_group_hosts(inventory, host_pattern)
           handle _ => [])
        else
          get_matching_hosts(inventory, host_pattern)
          
      (* Add hosts to temporary inventory *)
      val _ = app (fn h => 
                   let
                     val host_vars = Inventory.get_host_vars(inventory, h)
                              handle _ => []
                     val _ = ignore(Inventory.add_host(temp_inventory, h, host_vars))
                   in
                     ()
                   end) matching_hosts
                   
      (* Add the pattern as a group if it is one *)
      val _ = if is_group then
                (case Inventory.get_group(temp_inventory, host_pattern) of
                   SOME _ => ()
                 | NONE => 
                     let val group_vars = Inventory.get_group_vars(inventory, host_pattern)
                               handle _ => []
                     in ignore(Inventory.add_group(temp_inventory, host_pattern, group_vars))
                     end)
              else
                ()
                
      (* Add the play variables to all hosts *)
      val _ = app (fn host =>
                    app (fn (k, v) => 
                           Inventory.set_host_var(temp_inventory, host, k, v))
                        (#vars play))
                 matching_hosts
                 
      (* Track task results *)
      val host_results = ref ([]: (string * (string * TaskExecutor.change_status) list) list)
      
      (* Execute each task in order *)
      fun execute_tasks [] _ = ()
        | execute_tasks (task::rest) hosts =
            let
              val task_name = #name task
              
              (* Get host objects *)
              val host_objs = 
                map (fn h => 
                      case Inventory.get_host(temp_inventory, h) of
                        SOME host => host
                      | NONE => raise PlayError(#name play, "Host not found: " ^ h))
                    hosts
                    
              (* Execute the task on all hosts *)
              val results = TaskExecutor.execute_on_hosts(task, host_objs)
              
              (* Update host_results with this task's results *)
              val _ = 
                app (fn (host, result) =>
                      let
                        val host_name = 
                          case Inventory.get_host_groups(temp_inventory, "dummy") of
                            _ => "localhost" (* Simplified - would need proper access to host name *)
                            
                        val status = TaskExecutor.get_status result
                        val current = 
                          case List.find (fn (h, _) => h = host_name) (!host_results) of
                            SOME (_, tasks) => tasks
                          | NONE => []
                          
                        val updated = (task_name, status) :: current
                        
                        val new_results = 
                          List.filter (fn (h, _) => h <> host_name) (!host_results)
                      in
                        host_results := (host_name, updated) :: new_results
                      end)
                    results
                    
              (* Check for notifications to handlers *)
              val _ = 
                app (fn (host, result) =>
                      if TaskExecutor.is_changed result then
                        app (fn (handler: handler) => 
                               if String.isSubstring ("notify: " ^ (#name handler)) (#name task) then
                                 (#triggered handler) := true
                               else
                                 ())
                            (#handlers play)
                      else
                        ())
                    results
            in
              execute_tasks rest hosts
            end
            
      (* Execute triggered handlers *)
      fun execute_handlers [] _ = ()
        | execute_handlers ((handler: handler)::rest) hosts =
            if !(#triggered handler) then
              let
                val _ = execute_tasks [(#task handler)] hosts
              in
                execute_handlers rest hosts
              end
            else
              execute_handlers rest hosts
              
      (* Main execution *)
      val _ = execute_tasks (#tasks play) matching_hosts
      val _ = execute_handlers (#handlers play) matching_hosts
      
      (* Calculate summary statistics *)
      val all_results = List.concat (map (fn (_, tasks) => 
                                          map (fn (_, status) => status) tasks) 
                                       (!host_results))
                                       
      val ok_count = List.length (List.filter (fn s => s <> TaskExecutor.FAILED) all_results)
      val changed_count = List.length (List.filter (fn s => s = TaskExecutor.CHANGED) all_results)
      val failed_count = List.length (List.filter (fn s => s = TaskExecutor.FAILED) all_results)
      
      (* Create the play result *)
      val result = {
        play_name = #name play,
        host_results = !host_results,
        ok_count = ok_count,
        changed_count = changed_count,
        unreachable_count = 0,  (* Not tracked in this implementation *)
        failed_count = failed_count,
        skipped_count = 0  (* Not tracked in this implementation *)
      }
    in
      result
    end
  
  (* Execute a playbook *)
  fun execute (playbook: playbook, inventory: Inventory.inventory) =
    let
      fun execute_plays [] results = List.rev results
        | execute_plays (play::rest) results =
            let
              val result = execute_play (play, inventory)
            in
              execute_plays rest (result :: results)
            end
    in
      execute_plays (#plays playbook) []
    end
  
  (* Check if a play has any of the given tags *)
  fun play_has_tags (play: play, tags: string list) =
    List.exists (fn t => List.exists (fn pt => t = pt) (#tags play)) tags
  
  (* Execute a playbook with tag filtering *)
  fun execute_with_tags (playbook: playbook, inventory: Inventory.inventory, tags: string list) =
    let
      val filtered_plays = List.filter (fn (p: play) => play_has_tags(p, tags)) (#plays playbook)
      val filtered_book = {name = #name playbook, plays = filtered_plays}
    in
      execute (filtered_book, inventory)
    end
  
  (* Execute a playbook with host filtering *)
  fun execute_with_hosts (playbook: playbook, inventory: Inventory.inventory, hosts: string list) =
    let
      fun filter_play (play: play) =
        let
          val pattern = #hosts play
          val matches = List.exists (fn h => match_host_pattern(pattern, h)) hosts
        in
          if matches then
            SOME {
              name = #name play,
              hosts = String.concatWith "," hosts,  (* Use explicit host list *)
              tasks = #tasks play,
              handlers = #handlers play,
              vars = #vars play,
              tags = #tags play
            }
          else
            NONE
        end
        
      val filtered_plays = List.mapPartial filter_play (#plays playbook)
      val filtered_book = {name = #name playbook, plays = filtered_plays}
    in
      execute (filtered_book, inventory)
    end
  
  (* Get a summary of play results *)
  fun get_play_summary (result: play_result) =
    {
      ok = #ok_count result,
      changed = #changed_count result,
      unreachable = #unreachable_count result,
      failed = #failed_count result,
      skipped = #skipped_count result
    }
    
  (* Get task results from a play *)
  fun get_task_results (result: play_result) =
    List.concat (map (fn (_, tasks) => tasks) (#host_results result))
end