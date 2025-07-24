(* main.sml completion - vault handling section *)

  (* Handle vault operations *)
  fun handle_vault(action, file) =
    let
      (* Get vault password *)
      val _ = print "Vault password: "
      val password = 
        case TextIO.inputLine TextIO.stdIn of
          SOME line => String.substring(line, 0, String.size line - 1)  (* Remove newline *)
        | NONE => ""
        
      (* Open or create vault *)
      val vault = 
        if OS.FileSys.access(default_vault, []) then
          Vault.open_vault(default_vault, password)
        else
          Vault.create(default_vault, password)
          
      val _ = 
        case action of
          "encrypt" => 
            if OS.FileSys.access(file, []) then
              let
                val output_file = file ^ ".vault"
                val _ = Vault.encrypt_file(vault, file, output_file)
                val _ = print ("File encrypted: " ^ output_file ^ "\n")
              in
                ()
              end
            else
              print ("Error: File not found: " ^ file ^ "\n")
        | "decrypt" =>
            if OS.FileSys.access(file, []) then
              let
                (* Determine output name - remove .vault extension if present *)
                val output_file = 
                  if String.isSuffix ".vault" file then
                    String.substring(file, 0, String.size file - 6)
                  else
                    file ^ ".decrypted"
                    
                val _ = Vault.decrypt_file(vault, file, output_file)
                val _ = print ("File decrypted: " ^ output_file ^ "\n")
              in
                ()
              end
            else
              print ("Error: File not found: " ^ file ^ "\n")
        | _ => print ("Error: Unknown vault action: " ^ action ^ "\n")
        
      (* Close vault *)
      val _ = Vault.close_vault(vault)
    in
      ()
    end
    handle Vault.AuthenticationError msg =>
      (print ("Authentication error: " ^ msg ^ "\n");
       OS.Process.exit OS.Process.failure)
    | Vault.VaultError msg =>
      (print ("Vault error: " ^ msg ^ "\n");
       OS.Process.exit OS.Process.failure)
    | e =>
      (print ("Error: " ^ exnMessage e ^ "\n");
       OS.Process.exit OS.Process.failure)
  
  (* List hosts in inventory *)
  fun list_inventory(inventory_file) =
    let
      val _ = if not (OS.FileSys.access(inventory_file, [])) then
                (print ("Error: Inventory file not found: " ^ inventory_file ^ "\n");
                 OS.Process.exit OS.Process.failure)
              else ()
              
      (* Load inventory *)
      val inventory = Inventory.load_file inventory_file
      
      (* Get all hosts and groups *)
      val hosts = Inventory.list_hosts inventory
      val groups = Inventory.list_groups inventory
      
      (* Print host information *)
      val _ = print "Hosts:\n"
      val _ = List.app (fn host =>
                  let
                    val groups = Inventory.get_host_groups(inventory, host)
                    val group_str = String.concatWith ", " groups
                  in
                    print ("  " ^ host ^ 
                           (if group_str <> "" then " (groups: " ^ group_str ^ ")" else "") ^ 
                           "\n")
                  end)
              hosts
              
      (* Print group information *)
      val _ = print "\nGroups:\n"
      val _ = List.app (fn group =>
                  let
                    val hosts = Inventory.get_group_hosts(inventory, group)
                    val host_count = length hosts
                  in
                    print ("  " ^ group ^ " (" ^ Int.toString(host_count) ^ " hosts)\n")
                  end)
              groups
    in
      OS.Process.exit OS.Process.success
    end
    handle e => 
      (print ("Error: " ^ exnMessage e ^ "\n");
       OS.Process.exit OS.Process.failure)
  
  (* List hosts that would be targeted by a playbook *)
  fun list_playbook_hosts(playbook_file, inventory_file) =
    let
      val _ = if not (OS.FileSys.access(playbook_file, [])) then
                (print ("Error: Playbook file not found: " ^ playbook_file ^ "\n");
                 OS.Process.exit OS.Process.failure)
              else ()
              
      val _ = if not (OS.FileSys.access(inventory_file, [])) then
                (print ("Error: Inventory file not found: " ^ inventory_file ^ "\n");
                 OS.Process.exit OS.Process.failure)
              else ()
              
      (* Load inventory *)
      val inventory = Inventory.load_file inventory_file
      
      (* Load playbook *)
      val playbook = Playbook.load_file playbook_file
      
      (* Extract plays and host patterns *)
      val plays = #plays playbook
      
      (* For each play, expand host patterns *)
      val _ = List.app (fn play =>
                let
                  val host_pattern = #hosts play
                  val play_name = #name play
                  
                  (* Simple pattern expansion - would be more sophisticated in real implementation *)
                  fun expand_pattern pattern =
                    let
                      val hosts = Inventory.list_hosts inventory
                      
                      fun matches_pattern host =
                        case pattern of
                          "all" => true
                        | "localhost" => host = "localhost"
                        | _ =>
                            if String.isSubstring "*" pattern then
                              let
                                val prefix = String.substring(pattern, 0, 
                                                            String.size pattern - 1)
                              in
                                String.isPrefix prefix host
                              end
                            else if List.exists (fn g => g = pattern) 
                                             (Inventory.list_groups inventory) then
                              List.exists (fn h => h = host) 
                                         (Inventory.get_group_hosts(inventory, pattern))
                            else
                              host = pattern
                    in
                      List.filter matches_pattern hosts
                    end
                    
                  val matching_hosts = expand_pattern host_pattern
                  val host_count = length matching_hosts
                in
                  print ("Play: " ^ play_name ^ " (" ^ Int.toString(host_count) ^ " hosts)\n");
                  List.app (fn h => print ("  " ^ h ^ "\n")) matching_hosts;
                  print "\n"
                end)
              plays
    in
      OS.Process.exit OS.Process.success
    end
    handle e => 
      (print ("Error: " ^ exnMessage e ^ "\n");
       OS.Process.exit OS.Process.failure)
  
  (* Main function *)
  fun main() =
    let
      val args = CommandLine.arguments()
      
      (* Parse command line *)
      val options = parse_args args
      
      (* Execute appropriate command *)
      val _ = 
        case #command options of
          SOME "run" =>
            if length (#command_args options) >= 1 then
              run_playbook(
                hd (#command_args options),
                #inventory_file options,
                #limit_hosts options,
                #tags options,
                false  (* not check mode *)
              )
            else
              (print "Error: Missing playbook file\n";
               print_usage();
               OS.Process.exit OS.Process.failure)
        | SOME "check" =>
            if length (#command_args options) >= 1 then
              run_playbook(
                hd (#command_args options),
                #inventory_file options,
                #limit_hosts options,
                #tags options,
                true  (* check mode *)
              )
            else
              (print "Error: Missing playbook file\n";
               print_usage();
               OS.Process.exit OS.Process.failure)
        | SOME "list-hosts" =>
            if length (#command_args options) >= 1 then
              list_playbook_hosts(
                hd (#command_args options),
                #inventory_file options
              )
            else
              (print "Error: Missing playbook file\n";
               print_usage();
               OS.Process.exit OS.Process.failure)
        | SOME "inventory-list" =>
            list_inventory(#inventory_file options)
        | SOME "module" =>
            if length (#command_args options) >= 1 then
              let
                val module_name = hd (#command_args options)
                val module_args = tl (#command_args options)
              in
                run_module(module_name, module_args)
              end
            else
              (print "Error: Missing module name\n";
               print_usage();
               OS.Process.exit OS.Process.failure)
        | SOME "vault" =>
            if length (#command_args options) >= 2 then
              let
                val action = hd (#command_args options)
                val file = hd (tl (#command_args options))
              in
                handle_vault(action, file)
              end
            else
              (print "Error: Missing vault parameters\n";
               print_usage();
               OS.Process.exit OS.Process.failure)
        | SOME cmd =>
            (print ("Error: Unknown command: " ^ cmd ^ "\n");
             print_usage();
             OS.Process.exit OS.Process.failure)
        | NONE =>
            (print_usage();
             OS.Process.exit OS.Process.success)
    in
      ()
    end
    handle e => 
      (print ("Error: " ^ exnMessage e ^ "\n");
       OS.Process.exit OS.Process.failure)
end

(* Program entry point *)
val _ = Main.main()