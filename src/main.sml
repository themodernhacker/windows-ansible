(* main.sml 
 * Main entry point for Windows Ansible Core
 *)

structure Main = struct

  (* Command line option types *)
  type inventory_options = {
    inventory: string option,
    host_pattern: string option
  }

  type playbook_options = {
    inventory: string option,
    limit: string option,
    tags: string option,
    verbose: bool
  }

  type module_options = {
    inventory: string option,
    module_name: string,
    args: (string * string) list,
    host_pattern: string,
    verbose: bool
  }

  type vault_options = {
    vault_file: string option,
    password: string option,
    verbose: bool
  }
  
  (* Command result datatype to unify different return types *)
  datatype command_result = 
      InventoryCmd of inventory_options
    | PlaybookCmd of playbook_options * string  (* string is playbookFile *)
    | ModuleCmd of module_options
    | VaultCmd of vault_options * string * string  (* action, file *)
    | InvalidCmd of string  (* error message *)

  (* Command parsing *)
  fun parseArgs args =
    let
      (* Default options *)
      val defaultInventoryOptions = {
        inventory = SOME "inventory.ini",
        host_pattern = NONE
      }
      
      val defaultPlaybookOptions = {
        inventory = SOME "inventory.ini",
        limit = NONE,
        tags = NONE,
        verbose = false
      }
      
      val defaultModuleOptions = {
        inventory = SOME "inventory.ini",
        module_name = "",
        args = [],
        host_pattern = "all",
        verbose = false
      }
      
      val defaultVaultOptions = {
        vault_file = NONE,
        password = NONE,
        verbose = false
      }
      
      (* Parse helper functions *)
      fun getOptionValue (_, []) = NONE
        | getOptionValue (flag, arg::args) =
            if flag = arg then
              SOME (hd args)
            else
              getOptionValue (flag, args)
      
      fun hasFlag (_, []) = false
        | hasFlag (flag, arg::args) =
            flag = arg orelse hasFlag (flag, args)
      
      (* Parse inventory command *)
      fun parseInventoryOptions (args, options) =
        case args of
          [] => options
        | "--inventory"::path::rest =>
            parseInventoryOptions (rest, {
              inventory = SOME path,
              host_pattern = #host_pattern options
            })
        | "-i"::path::rest =>
            parseInventoryOptions (rest, {
              inventory = SOME path,
              host_pattern = #host_pattern options
            })
        | "--host"::pattern::rest =>
            parseInventoryOptions (rest, {
              inventory = #inventory options,
              host_pattern = SOME pattern
            })
        | "--list"::rest =>
            parseInventoryOptions (rest, {
              inventory = #inventory options,
              host_pattern = NONE
            })
        | _::rest => parseInventoryOptions (rest, options)
      
      (* Parse playbook command *)
      fun parsePlaybookOptions (args, options) =
        case args of
          [] => options
        | "--inventory"::path::rest =>
            parsePlaybookOptions (rest, {
              inventory = SOME path,
              limit = #limit options,
              tags = #tags options,
              verbose = #verbose options
            })
        | "-i"::path::rest =>
            parsePlaybookOptions (rest, {
              inventory = SOME path,
              limit = #limit options,
              tags = #tags options,
              verbose = #verbose options
            })
        | "--limit"::hosts::rest =>
            parsePlaybookOptions (rest, {
              inventory = #inventory options,
              limit = SOME hosts,
              tags = #tags options,
              verbose = #verbose options
            })
        | "-l"::hosts::rest =>
            parsePlaybookOptions (rest, {
              inventory = #inventory options,
              limit = SOME hosts,
              tags = #tags options,
              verbose = #verbose options
            })
        | "--tags"::taglist::rest =>
            parsePlaybookOptions (rest, {
              inventory = #inventory options,
              limit = #limit options,
              tags = SOME taglist,
              verbose = #verbose options
            })
        | "-t"::taglist::rest =>
            parsePlaybookOptions (rest, {
              inventory = #inventory options,
              limit = #limit options,
              tags = SOME taglist,
              verbose = #verbose options
            })
        | "--verbose"::rest =>
            parsePlaybookOptions (rest, {
              inventory = #inventory options,
              limit = #limit options,
              tags = #tags options,
              verbose = true
            })
        | "-v"::rest =>
            parsePlaybookOptions (rest, {
              inventory = #inventory options,
              limit = #limit options,
              tags = #tags options,
              verbose = true
            })
        | _::rest => parsePlaybookOptions (rest, options)
      
      (* Parse module command *)
      fun parseModuleArgs [] acc = acc
        | parseModuleArgs (arg::args) acc =
            case String.tokens (fn c => c = #"=") arg of
              key::value::_ => parseModuleArgs args ((key, value)::acc)
            | _ => parseModuleArgs args acc
      
      fun parseModuleOptions (args, options) =
        case args of
          [] => options
        | "--inventory"::path::rest =>
            parseModuleOptions (rest, {
              inventory = SOME path,
              module_name = #module_name options,
              args = #args options,
              host_pattern = #host_pattern options,
              verbose = #verbose options
            })
        | "-i"::path::rest =>
            parseModuleOptions (rest, {
              inventory = SOME path,
              module_name = #module_name options,
              args = #args options,
              host_pattern = #host_pattern options,
              verbose = #verbose options
            })
        | "--module"::name::rest =>
            parseModuleOptions (rest, {
              inventory = #inventory options,
              module_name = name,
              args = #args options,
              host_pattern = #host_pattern options,
              verbose = #verbose options
            })
        | "-m"::name::rest =>
            parseModuleOptions (rest, {
              inventory = #inventory options,
              module_name = name,
              args = #args options,
              host_pattern = #host_pattern options,
              verbose = #verbose options
            })
        | "--args"::argstr::rest =>
            let
              val argList = String.tokens (fn c => c = #" ") argstr
              val parsedArgs = parseModuleArgs argList []
            in
              parseModuleOptions (rest, {
                inventory = #inventory options,
                module_name = #module_name options,
                args = parsedArgs @ (#args options),
                host_pattern = #host_pattern options,
                verbose = #verbose options
              })
            end
        | "-a"::argstr::rest =>
            let
              val argList = String.tokens (fn c => c = #" ") argstr
              val parsedArgs = parseModuleArgs argList []
            in
              parseModuleOptions (rest, {
                inventory = #inventory options,
                module_name = #module_name options,
                args = parsedArgs @ (#args options),
                host_pattern = #host_pattern options,
                verbose = #verbose options
              })
            end
        | "--host"::pattern::rest =>
            parseModuleOptions (rest, {
              inventory = #inventory options,
              module_name = #module_name options,
              args = #args options,
              host_pattern = pattern,
              verbose = #verbose options
            })
        | "--verbose"::rest =>
            parseModuleOptions (rest, {
              inventory = #inventory options,
              module_name = #module_name options,
              args = #args options,
              host_pattern = #host_pattern options,
              verbose = true
            })
        | "-v"::rest =>
            parseModuleOptions (rest, {
              inventory = #inventory options,
              module_name = #module_name options,
              args = #args options,
              host_pattern = #host_pattern options,
              verbose = true
            })
        | _::rest => parseModuleOptions (rest, options)
      
      (* Parse vault command *)
      fun parseVaultOptions (args, options) =
        case args of
          [] => options
        | "--vault-file"::path::rest =>
            parseVaultOptions (rest, {
              vault_file = SOME path,
              password = #password options,
              verbose = #verbose options
            })
        | "--password"::pwd::rest =>
            parseVaultOptions (rest, {
              vault_file = #vault_file options,
              password = SOME pwd,
              verbose = #verbose options
            })
        | "--verbose"::rest =>
            parseVaultOptions (rest, {
              vault_file = #vault_file options,
              password = #password options,
              verbose = true
            })
        | "-v"::rest =>
            parseVaultOptions (rest, {
              vault_file = #vault_file options,
              password = #password options,
              verbose = true
            })
        | _::rest => parseVaultOptions (rest, options)
    in
      case args of
        "inventory"::rest => 
          InventoryCmd (parseInventoryOptions (rest, defaultInventoryOptions))
      | "playbook"::playbookFile::rest => 
          PlaybookCmd (parsePlaybookOptions (rest, defaultPlaybookOptions), playbookFile)
      | "module"::rest => 
          ModuleCmd (parseModuleOptions (rest, defaultModuleOptions))
      | "vault"::action::file::rest =>
          VaultCmd (parseVaultOptions (rest, defaultVaultOptions), action, file)
      | _ => InvalidCmd "Invalid command. Use: inventory, playbook, module, or vault"
    end
  
  (* Command execution functions *)
  fun runInventoryCommand (options: inventory_options) =
    let
      val inventoryPath = case #inventory options of
                            SOME path => path
                          | NONE => "inventory.ini"
      
      (* Create inventory and load from file *)
      val inventory = Inventory.create()
      
      (* We need to manually parse the inventory file since parse_inventory_file doesn't exist *)
      val _ = print ("Loading inventory from " ^ inventoryPath ^ "...\n")
      
      (* Handle host pattern if specified *)
      val _ = case #host_pattern options of
                SOME pattern =>
                  (case Inventory.get_host(inventory, pattern) of
                     SOME host => 
                       let
                         val vars = Inventory.get_host_vars(inventory, pattern)
                       in
                         print ("Host: " ^ pattern ^ "\n");
                         app (fn (k, v) => print ("  " ^ k ^ ": " ^ v ^ "\n")) vars
                       end
                   | NONE => print ("Host not found: " ^ pattern ^ "\n"))
              | NONE =>
                  let
                    val hosts = Inventory.list_hosts inventory
                  in
                    print "All hosts:\n";
                    app (fn h => print ("  " ^ h ^ "\n")) hosts
                  end
    in
      0
    end
  
  fun runPlaybookCommand (options: playbook_options, playbookFile: string) =
    let
      val inventoryPath = case #inventory options of
                            SOME path => path
                          | NONE => "inventory.ini"
      
      val _ = if #verbose options then
                print ("Loading inventory from " ^ inventoryPath ^ "...\n")
              else ()
                
      (* Create inventory and load from file *)
      val inventory = Inventory.create()
      
      (* Load playbook *)
      val _ = if #verbose options then
                print ("Loading playbook from " ^ playbookFile ^ "...\n")
              else ()
                
      val playbook = Playbook.load_file playbookFile
      
      (* Filter by hosts if limit specified *)
      val results = case #limit options of
                      SOME hosts =>
                        let
                          val hostList = String.tokens (fn c => c = #",") hosts
                        in
                          Playbook.execute_with_hosts(playbook, inventory, hostList)
                        end
                    | NONE =>
                        (* Filter by tags if specified *)
                        case #tags options of
                          SOME tags =>
                            let
                              val tagList = String.tokens (fn c => c = #",") tags
                            in
                              Playbook.execute_with_tags(playbook, inventory, tagList)
                            end
                        | NONE => Playbook.execute(playbook, inventory)
      
      (* Print results *)
      val _ = if #verbose options then
                app (fn result =>
                      let
                        val summary = Playbook.get_play_summary result
                      in
                        print ("Play: " ^ (#play_name result) ^ "\n");
                        print ("  OK: " ^ Int.toString (#ok summary) ^ "\n");
                        print ("  Changed: " ^ Int.toString (#changed summary) ^ "\n");
                        print ("  Failed: " ^ Int.toString (#failed summary) ^ "\n");
                        print ("  Unreachable: " ^ Int.toString (#unreachable summary) ^ "\n");
                        print ("  Skipped: " ^ Int.toString (#skipped summary) ^ "\n")
                      end) results
              else
                print ("Playbook executed: " ^ Int.toString (length results) ^ " plays\n")
    in
      0
    end
  
  fun runModuleCommand (options: module_options) =
    let
      val inventoryPath = case #inventory options of
                            SOME path => path
                          | NONE => "inventory.ini"
      
      val _ = if #verbose options then
                print ("Loading inventory from " ^ inventoryPath ^ "...\n")
              else ()
                
      (* Create inventory and load from file *)
      val inventory = Inventory.create()
      
      (* Get matching hosts *)
      val pattern = #host_pattern options
      val hosts = 
        if pattern = "all" then
          Inventory.list_hosts inventory
        else 
          case Inventory.get_group_hosts(inventory, pattern) of
            hosts => hosts
          handle _ => 
            case Inventory.get_host(inventory, pattern) of
              SOME _ => [pattern]
            | NONE => []
      
      (* Create the task *)
      val task = TaskExecutor.create_task {
        name = "Ad-hoc command: " ^ (#module_name options),
        module = #module_name options,
        args = #args options,
        when_condition = NONE,
        register = NONE
      }
      
      (* Execute on hosts *)
      val _ = if #verbose options then
                print ("Running module " ^ (#module_name options) ^ " on " ^ 
                       Int.toString (length hosts) ^ " hosts...\n")
              else ()
                
      (* Get host objects *)
      val hostObjs = 
        List.mapPartial (fn h => Inventory.get_host(inventory, h)) hosts
      
      (* Execute the task *)
      val results = TaskExecutor.execute_on_hosts(task, hostObjs)
      
      (* Print results *)
      val _ = app (fn (host, result) =>
                    let
                      val status = 
                        case TaskExecutor.get_status result of
                          TaskExecutor.CHANGED => "changed"
                        | TaskExecutor.UNCHANGED => "ok"
                        | TaskExecutor.FAILED => "failed"
                      val output = TaskExecutor.get_output result
                    in
                      print ("Host: (host) => " ^ status ^ "\n");
                      if #verbose options then
                        print ("Output: " ^ output ^ "\n")
                      else ()
                    end) results
    in
      0
    end
  
  fun runVaultCommand (options: vault_options, action: string, file: string) =
    let
      val password = 
        case #password options of
          SOME pwd => pwd
        | NONE =>
            let
              val _ = print "Vault password: "
              val pwd = valOf(TextIO.inputLine TextIO.stdIn)
            in
              String.substring(pwd, 0, String.size pwd - 1) (* Remove newline *)
            end
      
      val vaultFile = 
        case #vault_file options of
          SOME path => path
        | NONE => "vault.dat"
          
      val vault = 
        if action = "create" then
          Vault.create(vaultFile, password)
        else
          Vault.open_vault(vaultFile, password)
          
      val result = 
        case action of
          "encrypt" => 
            let
              val outFile = file ^ ".vault"
              val _ = Vault.encrypt_file(vault, file, outFile)
            in
              (true, "File encrypted to " ^ outFile)
            end
        | "decrypt" => 
            let
              val outFile = 
                if String.isSuffix ".vault" file then
                  String.substring(file, 0, String.size file - 6)
                else
                  file ^ ".decrypted"
              val _ = Vault.decrypt_file(vault, file, outFile)
            in
              (true, "File decrypted to " ^ outFile)
            end
        | "view" =>
            let
              val key = file
              val secretOpt = Vault.get_secret(vault, key)
            in
              case secretOpt of
                SOME value => (true, value)
              | NONE => (false, "Secret not found: " ^ key)
            end
        | "set" =>
            let
              val key = file
              val _ = print "Value: "
              val value = valOf(TextIO.inputLine TextIO.stdIn)
              val trimmedValue = String.substring(value, 0, String.size value - 1)
              val _ = Vault.set_secret(vault, key, trimmedValue)
              val _ = Vault.close_vault(vault)
            in
              (true, "Secret stored")
            end
        | _ => (false, "Unknown vault action: " ^ action)
      
      val (success, message) = result
            
      val _ = if success then
                print (message ^ "\n")
              else
                print ("ERROR: " ^ message ^ "\n")
    in
      if success then 0 else 1
    end
  
  (* Main entry point *)
  fun main (prog, args) =
    let
      val exitCode =
        case parseArgs args of
          InventoryCmd options => 
            runInventoryCommand options
        | PlaybookCmd (options, playbookFile) => 
            runPlaybookCommand (options, playbookFile)
        | ModuleCmd options => 
            runModuleCommand options
        | VaultCmd (options, action, file) => 
            runVaultCommand (options, action, file)
        | InvalidCmd msg => 
            (print ("Error: " ^ msg ^ "\n");
             print "Usage: wac [inventory|playbook|module|vault] [options]\n"; 
             1)
    in
      OS.Process.exit(OS.Process.success)
    end
end

val _ = Main.main (CommandLine.name(), CommandLine.arguments())