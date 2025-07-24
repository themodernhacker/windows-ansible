(* playbook.sml
 * Implementation of Ansible-compatible playbook system
 *)

structure Playbook : PLAYBOOK = struct
  structure Inventory = Inventory
  structure TaskExecutor = TaskExecutor

  (* Type definitions *)
  type vars = (string * string) list
  
  type handler = {
    name: string,
    task: TaskExecutor.task,
    triggered: bool ref
  }
  
  type play = {
    name: string,
    hosts: string,
    tasks: TaskExecutor.task list,
    handlers: handler list,
    vars: vars,
    tags: string list
  }
  
  type playbook = {
    name: string,
    plays: play list
  }
  
  type var_scope = {
    play_vars: vars,
    host_vars: vars,
    global_vars: vars
  }
  
  type play_result = {
    play_name: string,
    host_results: (string * (string * TaskExecutor.task_result) list) list,
    handler_results: (string * (string * TaskExecutor.task_result) list) list,
    duration: Time.time
  }
  
  (* Exceptions *)
  exception PlaybookError of string
  exception PlayError of string * string
  
  (* Helper function to expand host patterns - simplified version *)
  fun expand_host_pattern (inventory, pattern) =
    let
      val all_hosts = Inventory.list_hosts inventory
      
      (* Simple pattern matching - in real implementation would handle more complex patterns *)
      fun matches_pattern (host, patt) =
        case patt of
          "all" => true
        | "localhost" => host = "localhost" orelse host = "127.0.0.1"
        | _ => 
            if String.isSubstring "*" patt then
              let
                val prefix = String.substring(patt, 0, String.size patt - 1)
              in
                String.isPrefix prefix host
              end
            else
              host = patt
              
      val matching_hosts = List.filter (fn h => matches_pattern(h, pattern)) all_hosts
      
      (* If it's a group name, add all hosts in that group *)
      val group_hosts = 
        (Inventory.get_group_hosts(inventory, pattern))
        handle _ => []
        
      (* Combine direct matches and group members *)
      val all_matching = 
        List.foldr (fn (h, acc) => 
                     if List.exists (fn h' => h = h') acc
                     then acc
                     else h :: acc) 
                   matching_hosts
                   group_hosts
    in
      all_matching
    end
  
  (* YAML parser for playbooks - simplified *)
  fun load_file filename =
    let
      (* In a real implementation, this would parse YAML *)
      (* This is a very simplified version that creates a basic playbook *)
      val playbook_name = OS.Path.file filename
      
      (* Example tasks *)
      val example_task = TaskExecutor.create_task {
        name = "Example task",
        module = "win_command",
        args = [("command", "echo Hello World")],
        when_condition = NONE,
        register = NONE
      }
      
      (* Example handlers *)
      val example_handler = create_handler {
        name = "Example handler",
        task = TaskExecutor.create_task {
          name = "Handler task",
          module = "win_command",
          args = [("command", "echo Handler executed")],
          when_condition = NONE,
          register = NONE
        }
      }
      
      (* Example play *)
      val example_play = create_play {
        name = "Example play",
        hosts = "all",
        tasks = [example_task],
        handlers = [example_handler],
        vars = [("example_var", "example_value")],
        tags = ["example"]
      }
    in
      add_play(create_playbook playbook_name, example_play)
    end
    handle IO.Io {name, ...} =>
      raise PlaybookError("Failed to load playbook: " ^ name)
  
  (* Save playbook to file - simplified *)
  fun save_file (playbook, filename) =
    let
      val file = TextIO.openOut filename
      
      (* Write a simple YAML-like representation *)
      val _ = TextIO.output(file, "---\n")
      val _ = TextIO.output(file, "# Playbook: " ^ (#name playbook) ^ "\n")
      
      fun write_play play =
        let
          val _ = TextIO.output(file, "- name: " ^ (#name play) ^ "\n")
          val _ = TextIO.output(file, "  hosts: " ^ (#hosts play) ^ "\n")
          
          (* Write vars *)
          val _ = 
            if not (null (#vars play)) then
              (TextIO.output(file, "  vars:\n");
               List.app (fn (k, v) => 
                 TextIO.output(file, "    " ^ k ^ ": " ^ v ^ "\n")) 
                 (#vars play))
            else ()
            
          (* Write tags *)
          val _ = 
            if not (null (#tags play)) then
              (TextIO.output(file, "  tags:\n");
               List.app (fn tag => 
                 TextIO.output(file, "    - " ^ tag ^ "\n")) 
                 (#tags play))
            else ()
            
          (* Write tasks *)
          val _ = TextIO.output(file, "  tasks:\n")
          val _ = List.app (fn task => 
                     let
                       val _ = TextIO.output(file, "    - name: " ^ (#name task) ^ "\n")
                       val _ = TextIO.output(file, "      " ^ (#module task) ^ ":\n")
                       val _ = List.app (fn (k, v) => 
                                 TextIO.output(file, "        " ^ k ^ ": " ^ v ^ "\n")) 
                                 (#args task)
                     in
                       ()
                     end) (#tasks play)
                     
          (* Write handlers *)
          val _ = 
            if not (null (#handlers play)) then
              (TextIO.output(file, "  handlers:\n");
               List.app (fn handler => 
                 let
                   val task = #task handler
                   val _ = TextIO.output(file, "    - name: " ^ (#name handler) ^ "\n")
                   val _ = TextIO.output(file, "      " ^ (#module task) ^ ":\n")
                   val _ = List.app (fn (k, v) => 
                             TextIO.output(file, "        " ^ k ^ ": " ^ v ^ "\n")) 
                             (#args task)
                 in
                   ()
                 end) (#handlers play))
            else ()
        in
          ()
        end
      
      (* Write all plays *)
      val _ = List.app write_play (#plays playbook)
      
      val _ = TextIO.closeOut file
    in
      ()
    end
    handle IO.Io {name, ...} =>
      raise PlaybookError("Failed to save playbook: " ^ name)
  
  (* Create a handler *)
  fun create_handler {name, task} = {
    name = name,
    task = task,
    triggered = ref false
  }
  
  (* Create a play *)
  fun create_play {name, hosts, tasks, handlers, vars, tags} = {
    name = name,
    hosts = hosts,
    tasks = tasks,
    handlers = handlers,
    vars = vars,
    tags = tags
  }
  
  (* Create a playbook *)
  fun create_playbook name = {
    name = name,
    plays = []
  }
  
  (* Add a play to a playbook *)
  fun add_play (playbook, play) = {
    name = #name playbook,
    plays = play :: (#plays playbook)
  }
  
  (* Execute a single play *)
  fun execute_play (play, inventory) =
    let
      val start_time = Time.now()
      
      (* Find hosts that match the pattern *)
      val matching_hosts = expand_host_pattern(inventory, #hosts play)
      
      (* Get host objects *)
      val host_objects = 
        List.mapPartial (fn host_name => 
                          case Inventory.get_host(inventory, host_name) of
                            SOME host => SOME (host_name, host)
                          | NONE => NONE)
                        matching_hosts
                        
      (* Execute each task on each host *)
      val host_results = 
        List.map (fn (host_name, host) => 
                   let
                     (* Execute all tasks for this host *)
                     val task_results = 
                       List.map (fn task => 
                                  let
                                    (* Execute the task *)
                                    val result = TaskExecutor.execute(task, host)
                                    
                                    (* Trigger handlers if needed *)
                                    val _ = 
                                      if TaskExecutor.is_changed result then
                                        List.app (fn handler => #triggered handler := true) 
                                                (#handlers play)
                                      else ()
                                  in
                                    (#name task, result)
                                  end)
                                (#tasks play)
                   in
                     (host_name, task_results)
                   end)
                 host_objects
                 
      (* Execute triggered handlers *)
      val handler_results = 
        List.map (fn (host_name, host) => 
                   let
                     (* Execute triggered handlers *)
                     val handler_results = 
                       List.mapPartial (fn handler => 
                                         if !(#triggered handler) then
                                           SOME (#name handler, 
                                                TaskExecutor.execute(#task handler, host))
                                         else
                                           NONE)
                                      (#handlers play)
                   in
                     (host_name, handler_results)
                   end)
                 host_objects
                 
      val end_time = Time.now()
    in
      {
        play_name = #name play,
        host_results = host_results,
        handler_results = handler_results,
        duration = Time.-(end_time, start_time)
      }
    end
  
  (* Execute an entire playbook *)
  fun execute (playbook, inventory) =
    List.map (fn play => execute_play(play, inventory)) (#plays playbook)
  
  (* Filter a play by tags *)
  fun play_matches_tags (play, tags) =
    List.exists (fn play_tag => 
                  List.exists (fn tag => play_tag = tag) tags)
                (#tags play)
  
  (* Execute playbook with tag filtering *)
  fun execute_with_tags (playbook, inventory, tags) =
    let
      val matching_plays = 
        List.filter (fn play => play_matches_tags(play, tags)) (#plays playbook)
    in
      List.map (fn play => execute_play(play, inventory)) matching_plays
    end
  
  (* Execute playbook with host filtering *)
  fun execute_with_hosts (playbook, inventory, host_list) =
    let
      (* Create a temporary inventory with only the specified hosts *)
      val temp_inventory = Inventory.create()
      
      (* Copy hosts from original inventory to temp inventory *)
      val _ = List.app (fn host_name => 
                case Inventory.get_host(inventory, host_name) of
                  SOME host => 
                    let
                      val host_vars = Inventory.get_host_vars(inventory, host_name)
                      val _ = Inventory.add_host(temp_inventory, host_name, host_vars)
                      
                      (* Copy group memberships *)
                      val groups = Inventory.get_host_groups(inventory, host_name)
                      val _ = List.app (fn group => 
                                let
                                  val _ = 
                                    (Inventory.get_group(temp_inventory, group))
                                    handle _ => 
                                      let
                                        val group_vars = Inventory.get_group_vars(inventory, group)
                                      in
                                        Inventory.add_group(temp_inventory, group, group_vars)
                                      end
                                      
                                  val _ = Inventory.add_host_to_group(temp_inventory, host_name, group)
                                in
                                  ()
                                end)
                              groups
                    in
                      ()
                    end
                | NONE => ())
              host_list
    in
      (* Execute plays with the temporary inventory *)
      execute(playbook, temp_inventory)
    end
  
  (* Summarize play results *)
  fun get_play_summary result =
    let
      (* Count task results across all hosts *)
      fun count_statuses task_results =
        List.foldl (fn ((_, result), (ok, changed, failed, skipped)) => 
                     if TaskExecutor.is_skipped result then
                       (ok, changed, failed, skipped + 1)
                     else if TaskExecutor.is_failed result then
                       (ok, changed, failed + 1, skipped)
                     else if TaskExecutor.is_changed result then
                       (ok, changed + 1, failed, skipped)
                     else
                       (ok + 1, changed, failed, skipped))
                   (0, 0, 0, 0)
                   task_results
                   
      (* Process all hosts *)
      val (ok, changed, failed, skipped, unreachable) = 
        List.foldl (fn ((host, tasks), (ok, changed, failed, skipped, unreachable)) => 
                     let
                       val (host_ok, host_changed, host_failed, host_skipped) = 
                         count_statuses tasks
                     in
                       (ok + host_ok, 
                        changed + host_changed, 
                        failed + host_failed,
                        skipped + host_skipped,
                        unreachable)
                     end)
                   (0, 0, 0, 0, 0)
                   (#host_results result)
    in
      {
        ok = ok,
        changed = changed,
        unreachable = unreachable,
        failed = failed,
        skipped = skipped
      }
    end
  
  (* Get detailed task results *)
  fun get_task_results result =
    List.concat 
      (List.map (fn (host, tasks) => 
                  List.map (fn (task_name, task_result) => 
                             (task_name, TaskExecutor.get_status task_result))
                           tasks)
                (#host_results result))
end