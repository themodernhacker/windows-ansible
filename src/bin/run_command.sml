(* Load the entire WAC system *)
use "vault.sml";
use "active_directory.sml";
use "windows_module.sml"; 
use "inventory.sml";
use "task_executor.sml";
use "playbook.sml";
use "main.sml";

(* This will make Main structure available *)
structure CommandRunner = struct
  fun run [] = print "No arguments provided\n"
    | run (cmd::args) = 
        case cmd of
          "inventory" => Main.runInventoryCommand {
            inventory = SOME (if null args then "inventory.ini" else hd args),
            host_pattern = if length args < 2 then NONE else SOME (hd (tl args))
          }
        | "playbook" => if null args 
            then print "Missing playbook file\n"
            else Main.runPlaybookCommand ({
              inventory = SOME (if length args < 2 then "inventory.ini" else hd (tl args)),
              limit = NONE,
              tags = NONE,
              verbose = true
            }, hd args)
        | "vault" => if length args < 2
            then print "Missing vault action and file\n"
            else Main.runVaultCommand ({
              vault_file = NONE,
              password = NONE,
              verbose = false
            }, hd args, hd (tl args))
        | _ => print ("Unknown command: " ^ cmd ^ "\n")
  
  (* Run with specified arguments *)
  val _ = run ["inventory", "--list"]
end;