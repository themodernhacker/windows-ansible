(* main.sml
 * Main entry point for Windows Ansible Core
 *)

structure Main = struct
  (* Command-line argument parsing *)
  exception InvalidArgument of string
  
  (* Available commands *)
  datatype command = 
    Run of string  (* run playbook *)
  | Check of string (* check playbook - dry run *)
  | ListHosts of string (* list hosts in playbook *)
  | InventoryList of string option (* list hosts in inventory *)
  | Module of string * (string * string) list (* run a single module with args *)
  | Vault of string * string (* encrypt/decrypt a file *)
  | Help (* show help *)
  
  (* Program options *)
  type options = {
    inventory: string option,
    limit: string list option,
    tags: string list option,
    verbose: bool
  }
  
  (* Default options *)
  val default_options = {
    inventory = SOME "hosts.ini",
    limit = NONE,
    tags = NONE,
    verbose = false
  }
  
  (* Show command help *)
  fun show_help () =
    let
      val help_text = 
        "Windows Ansible Core (WAC) - Windows-native Ansible alternative\n" ^
        "\n" ^
        "Usage: wac [options] command [args...]\n" ^
        "\n" ^
        "Commands:\n" ^
        "  run <playbook>        Execute a playbook\n" ^
        "  check <playbook>      Check a playbook without making changes\n" ^
        "  list-hosts <playbook> List hosts that would be targeted by a playbook\n" ^
        "  inventory-list        List hosts in the inventory\n" ^
        "  module <module> <arg>=<val>...  Run a single module\n" ^
        "  vault <encrypt|decrypt> <file>  Encrypt or decrypt a file\n" ^
        "  --help                Show this help message\n" ^
        "\n" ^
        "Options:\n" ^
        "  -i, --inventory <file>  Specify inventory file (default: hosts.ini)\n" ^
        "  -l, --limit <hosts>     Limit execution to specified hosts (comma-separated)\n" ^
        "  -t, --tags <tags>       Only run plays and tasks with specified tags (comma-separated)\n" ^
        "  -v, --verbose           Increase output verbosity\n"
    in
      print help_text
    end
  
  (* Parse command-line arguments *)
  fun parse_args args =
    let
      (* Helper function to parse options *)
      fun parse_opts ([], options, acc_args) = 
            (options, List.rev acc_args)
        | parse_opts ("--help" :: rest, options, _) = 
            (options, ["--help"])
        | parse_opts ("-h" :: rest, options, _) = 
            (options, ["--help"])
        | parse_opts ("-i" :: file :: rest, options, acc_args) = 
            parse_opts (rest, {inventory = SOME file, 
                              limit = #limit options, 
                              tags = #tags options, 
                              verbose = #verbose options}, acc_args)
        | parse_opts ("--inventory" :: file :: rest, options, acc_args) = 
            parse_opts (rest, {inventory = SOME file, 
                              limit = #limit options, 
                              tags = #tags options, 
                              verbose = #verbose options}, acc_args)
        | parse_opts ("-l" :: hosts :: rest, options, acc_args) = 
            let
              val host_list = String.tokens (fn c => c = #",") hosts
            in
              parse_opts (rest, {inventory = #inventory options, 
                                limit = SOME host_list, 
                                tags = #tags options, 
                                verbose = #verbose options}, acc_args)
            end
        | parse_opts ("--limit" :: hosts :: rest, options, acc_args) = 
            let
              val host_list = String.tokens (fn c => c = #",") hosts
            in
              parse_opts (rest, {inventory = #inventory options, 
                                limit = SOME host_list, 
                                tags = #tags options, 
                                verbose = #verbose options}, acc_args)
            end
        | parse_opts ("-t" :: tags :: rest, options, acc_args) = 
            let
              val tag_list = String.tokens (fn c => c = #",") tags
            in
              parse_opts (rest, {inventory = #inventory options, 
                                limit = #limit options, 
                                tags = SOME tag_list, 
                                verbose = #verbose options}, acc_args)
            end
        | parse_opts ("--tags" :: tags :: rest, options, acc_args) = 
            let
              val tag_list = String.tokens (fn c => c = #",") tags
            in
              parse_opts (rest, {inventory = #inventory options, 
                                limit = #limit options, 
                                tags = SOME tag_list, 
                                verbose = #verbose options}, acc_args)
            end
        | parse_opts ("-v" :: rest, options, acc_args) = 
            parse_opts (rest, {inventory = #inventory options, 
                              limit = #limit options, 
                              tags = #tags options, 
                              verbose = true}, acc_args)
        | parse_opts ("--verbose" :: rest, options, acc_args) = 
            parse_opts (rest, {inventory = #inventory options, 
                              limit = #limit options, 
                              tags = #tags options, 
                              verbose = true}, acc_args)
        | parse_opts (arg :: rest, options, acc_args) = 
            parse_opts (rest, options, arg :: acc_args)
      
      (* Parse options *)
      val (options, cmd_args) = parse_opts (args, default_options, [])
      
      (* Parse command *)
      val command = 
        case cmd_args of
          ["--help"] => Help
        | ["run", playbook] => Run playbook
        | ["check", playbook] => Check playbook
        | ["list-hosts", playbook] => ListHosts playbook
        | ["inventory-list"] => InventoryList (#inventory options)
        | "module" :: module_name :: args =>
            let
              (* Parse module arguments in format key=value *)
              fun parse_module_args [] = []
                | parse_module_args (arg :: rest) =
                    case String.tokens (fn c => c = #"=") arg of
                      [key, value] => (key, value) :: parse_module_args rest
                    | _ => raise InvalidArgument("Invalid module argument format: " ^ arg)
            in
              Module (module_name, parse_module_args args)
            end
        | ["vault", action, file] =>
            if action = "encrypt" orelse action = "decrypt" then
              Vault (action, file)
            else
              raise InvalidArgument("Invalid vault action: " ^ action)
        | _ => raise InvalidArgument("Invalid command or missing arguments")
    in
      (options, command)
    end
    handle InvalidArgument msg =>
      (print ("Error: " ^ msg ^ "\n\n"); show_help(); raise Fail "Invalid arguments")
  
  (* Print a result with formatting *)
  fun print_result verbose name results =
    let
      val changed = 
        case List.find (fn (key, _) => key = "changed") results of
          SOME (_, value) => value = "true"
        | NONE => false
        
      val failed = 
        case List.find (fn (key, _) => key = "failed") results of
          SOME (_, value) => value = "true"
        | NONE => false
        
      val msg = 
        case List.find (fn (key, _) => key = "msg") results of
          SOME (_, value) => value
        | NONE => ""
        
      val status_text = 
        if failed then "FAILED"
        else if changed then "CHANGED"
        else "OK"
        
      val color = 
        if failed then "\027[31m" (* Red *)
        else if changed then "\027[33m" (* Yellow *)
        else "\027[32m" (* Green *)
        
      val reset = "\027[0m"
      
      (* Print task header *)
      val _ = print (name ^ " ... " ^ color ^ status_text ^ reset ^ "\n")
      
      (* Print message *)
      val _ = if msg <> "" then print ("  " ^ msg ^ "\n") else ()
      
      (* Print detailed results in verbose mode *)
      val _ = 
        if verbose then
          app (fn (key, value) => 
                if key <> "changed" andalso key <> "failed" andalso key <> "msg" andalso value <> "" then
                  print ("  " ^ key ^ ": " ^ value ^ "\n")
                else ())
              results
        else
          ()
    in
      ()
    end
  
  (* Run a playbook *)
  fun run_playbook options playbook check_mode =
    let
      (* Parse inventory *)
      val inventory = 
        case #inventory options of
          SOME inv_file => Inventory.parse_inventory_file inv_file
        | NONE => Inventory.parse_inventory_file "hosts.ini"
        
      (* Parse playbook *)
      val plays = Playbook.parse_playbook_file playbook
      
      (* Apply host limiting if specified *)
      val filtered_plays = 
        case #limit options of
          SOME hosts => Playbook.filter_plays plays hosts
        | NONE => plays
        
      (* Apply tag filtering if specified *)
      val tagged_plays = 
        case #tags options of
          SOME tags => Playbook.filter_plays_by_tags filtered_plays tags
        | NONE => filtered_plays
        
      (* Execute the playbook *)
      val results = TaskExecutor.execute_playbook tagged_plays inventory check_mode
      
      (* Print results *)
      val _ = 
        app (fn (task_name, task_results) => 
              print_result (#verbose options) task_name task_results)
            results
    in
      if List.exists (fn (_, results) => 
                        case List.find (fn (key, _) => key = "failed") results of
                          SOME (_, "true") => true
                        | _ => false)
                     results then
        OS.Process.failure
      else
        OS.Process.success
    end
    handle e => (print ("Error: " ^ exnMessage e ^ "\n"); OS.Process.failure)
  
  (* List hosts in a playbook *)
  fun list_hosts options playbook =
    let
      (* Parse inventory *)
      val inventory = 
        case #inventory options of
          SOME inv_file => Inventory.parse_inventory_file inv_file
        | NONE => Inventory.parse_inventory_file "hosts.ini"
        
      (* Parse playbook *)
      val plays = Playbook.parse_playbook_file playbook
      
      (* Apply host limiting if specified *)
      val filtered_plays = 
        case #limit options of
          SOME hosts => Playbook.filter_plays plays hosts
        | NONE => plays
        
      (* Get all hosts *)
      val all_hosts = Playbook.get_all_hosts filtered_plays inventory
      
      (* Print hosts *)
      val _ = print "Hosts that would be targeted:\n"
      val _ = app (fn host => print ("  " ^ host ^ "\n")) all_hosts
    in
      OS.Process.success
    end
    handle e => (print ("Error: " ^ exnMessage e ^ "\n"); OS.Process.failure)
  
  (* List hosts in inventory *)
  fun inventory_list options =
    let
      (* Parse inventory *)
      val inventory = 
        case #inventory options of
          SOME inv_file => Inventory.parse_inventory_file inv_file
        | NONE => Inventory.parse_inventory_file "hosts.ini"
        
      (* Get all groups *)
      val groups = Inventory.get_groups inventory
      
      (* Print groups and hosts *)
      val _ = print "Inventory:\n"
      val _ = 
        app (fn group =>
              let
                val hosts = Inventory.get_hosts_in_group inventory group
              in
                print ("  [" ^ group ^ "]:\n");
                app (fn host => print ("    " ^ host ^ "\n")) hosts
              end)
            groups
    in
      OS.Process.success
    end
    handle e => (print ("Error: " ^ exnMessage e ^ "\n"); OS.Process.failure)
  
  (* Run a single module *)
  fun run_module options module_name args =
    let
      (* Convert args to module-specific format *)
      val module_args = 
        (* In real implementation, would convert string args to appropriate record type *)
        args
        
      (* Execute module *)
      val result = WindowsModule.run_module module_name module_args
      
      (* Format result for display *)
      val display_results = [
        ("changed", Bool.toString (#changed result)),
        ("failed", Bool.toString (#failed result)),
        ("msg", case #msg result of SOME m => m | NONE => "")
      ] @ #results result
      
      (* Print result *)
      val _ = print_result (#verbose options) ("Running " ^ module_name) display_results
    in
      if #failed result then
        OS.Process.failure
      else
        OS.Process.success
    end
    handle e => (print ("Error: " ^ exnMessage e ^ "\n"); OS.Process.failure)
  
  (* Encrypt or decrypt a file *)
  fun vault_file options action file =
    let
      val (success, message) = 
        case action of
          "encrypt" => Vault.encrypt_file file
        | "decrypt" => Vault.decrypt_file file
        | _ => raise Fail "Invalid vault action"
        
      val display_results = [
        ("changed", Bool.toString success),
        ("failed", Bool.toString (not success)),
        ("msg", message)
      ]
      
      (* Print result *)
      val _ = print_result (#verbose options) ("Vault " ^ action) display_results
    in
      if success then
        OS.Process.success
      else
        OS.Process.failure
    end
    handle e => (print ("Error: " ^ exnMessage e ^ "\n"); OS.Process.failure)
  
  (* Main entry point *)
  fun main () =
    let
      val args = CommandLine.arguments()
      
      val (options, command) = 
        if List.length args = 0 then
          (default_options, Help)
        else
          parse_args args
          
      val exit_code = 
        case command of
          Help => (show_help(); OS.Process.success)
        | Run playbook => run_playbook options playbook false
        | Check playbook => run_playbook options playbook true
        | ListHosts playbook => list_hosts options playbook
        | InventoryList inv => inventory_list {inventory = inv, 
                                              limit = #limit options, 
                                              tags = #tags options, 
                                              verbose = #verbose options}
        | Module (name, args) => run_module options name args
        | Vault (action, file) => vault_file options action file
    in
      OS.Process.exit exit_code
    end
end

(* Call main if invoked directly *)
val _ = Main.main() handle _ => OS.Process.exit(OS.Process.failure)