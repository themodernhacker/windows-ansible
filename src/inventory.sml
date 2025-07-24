(* inventory.sml
 * Implementation of Ansible-compatible inventory system
 *)

structure Inventory : INVENTORY = struct
  (* Host and group types *)
  type host = {
    name: string,
    vars: (string * string) list ref
  }
  
  type group = {
    name: string,
    vars: (string * string) list ref,
    hosts: string list ref
  }
  
  type inventory = {
    hosts: (string, host) HashTable.hash_table,
    groups: (string, group) HashTable.hash_table,
    dynamic_sources: (unit -> (string * (string * string) list) list) list ref
  }
  
  type host_vars = (string * string) list
  type group_vars = (string * string) list
  
  exception InventoryError of string
  
  (* Create an empty inventory *)
  fun create () = {
    hosts = HashTable.mkTable (HashString.hashString, op=) (101, InventoryError "Host not found"),
    groups = HashTable.mkTable (HashString.hashString, op=) (101, InventoryError "Group not found"),
    dynamic_sources = ref []
  }
  
  (* Parse simple INI-style inventory files (simplified version) *)
  fun load_file filename = 
    let
      val inventory = create()
      val file = TextIO.openIn filename
      
      fun process_line (line, current_group) =
        if String.size line = 0 orelse String.sub(line, 0) = #"#"
        then current_group  (* Skip empty lines and comments *)
        else if String.sub(line, 0) = #"[" andalso 
                String.sub(line, String.size line - 1) = #"]"
        then  (* Group header *)
          let
            val group_name = String.substring(line, 1, String.size line - 2)
            val _ = add_group(inventory, group_name, [])
          in
            SOME group_name
          end
        else  (* Host line *)
          let
            val tokens = String.tokens (fn c => c = #" ") line
            val hostname = hd tokens
            
            (* Process any variables on the line *)
            val vars = 
              if length tokens > 1
              then 
                let
                  val var_strings = tl tokens
                  fun parse_var str =
                    case String.fields (fn c => c = #"=") str of
                      [key, value] => (key, value)
                    | _ => raise InventoryError("Invalid variable format: " ^ str)
                in
                  map parse_var var_strings
                end
              else []
              
            (* Add the host *)
            val host = add_host(inventory, hostname, vars)
            
            (* Add to current group if any *)
            val _ = case current_group of
                      SOME group => add_host_to_group(inventory, hostname, group)
                    | NONE => ()
          in
            current_group
          end
      
      (* Process all lines in the file *)
      fun process_lines current_group =
        case TextIO.inputLine file of
          SOME line => 
            let
              val line = String.trimr 1 line  (* Remove newline *)
              val new_group = process_line(line, current_group)
            in
              process_lines new_group
            end
        | NONE => ()
        
      val _ = process_lines NONE
      val _ = TextIO.closeIn file
    in
      inventory
    end
    handle IO.Io {name, ...} => 
      raise InventoryError("Failed to load inventory file: " ^ name)
  
  (* Save inventory to a file in INI format *)
  fun save_file (inventory, filename) =
    let
      val file = TextIO.openOut filename
      
      (* Write all hosts not in any group *)
      val all_hosts = list_hosts inventory
      val all_groups = list_groups inventory
      
      fun is_in_any_group hostname =
        List.exists (fn group => 
          List.exists (fn h => h = hostname) 
                      (get_group_hosts(inventory, group)))
                    all_groups
      
      val ungrouped = List.filter (fn h => not (is_in_any_group h)) all_hosts
      
      (* Write ungrouped hosts *)
      val _ = TextIO.output(file, "[ungrouped]\n")
      val _ = List.app (fn h => 
                  let
                    val host_vars = get_host_vars(inventory, h)
                    val var_str = String.concatWith " " 
                                   (map (fn (k, v) => k ^ "=" ^ v) host_vars)
                  in
                    TextIO.output(file, h ^ " " ^ var_str ^ "\n")
                  end) ungrouped
      
      (* Write each group *)
      val _ = List.app (fn group => 
                let
                  (* Write group header *)
                  val _ = TextIO.output(file, "\n[" ^ group ^ "]\n")
                  
                  (* Write group variables as [group:vars] section *)
                  val group_vars = get_group_vars(inventory, group)
                  val _ = if not (null group_vars) then
                            (TextIO.output(file, "\n[" ^ group ^ ":vars]\n");
                             List.app (fn (k, v) => 
                               TextIO.output(file, k ^ "=" ^ v ^ "\n")) group_vars)
                          else ()
                  
                  (* Write hosts in this group *)
                  val hosts = get_group_hosts(inventory, group)
                  val _ = List.app (fn h =>
                            let
                              val host_vars = get_host_vars(inventory, h)
                              val var_str = String.concatWith " " 
                                            (map (fn (k, v) => k ^ "=" ^ v) host_vars)
                            in
                              TextIO.output(file, h ^ " " ^ var_str ^ "\n")
                            end) hosts
                in
                  ()
                end) all_groups
                
      val _ = TextIO.closeOut file
    in
      ()
    end
    handle IO.Io {name, ...} => 
      raise InventoryError("Failed to save inventory file: " ^ name)
  
  (* Host management functions *)
  fun add_host (inventory, hostname, vars) =
    let
      val host = {
        name = hostname,
        vars = ref vars
      }
    in
      (HashTable.insert (#hosts inventory) (hostname, host); host)
    end
  
  fun remove_host (inventory, hostname) =
    (HashTable.remove (#hosts inventory) hostname;
     
     (* Also remove from all groups *)
     HashTable.appi (fn (group_name, group) => 
       let
         val hosts = !(#hosts group)
         val new_hosts = List.filter (fn h => h <> hostname) hosts
       in
         #hosts group := new_hosts
       end) (#groups inventory))
  
  fun get_host (inventory, hostname) =
    SOME (HashTable.lookup (#hosts inventory) hostname)
    handle _ => NONE
  
  fun list_hosts inventory =
    HashTable.foldi (fn (name, _, acc) => name :: acc) [] (#hosts inventory)
  
  (* Group management functions *)
  fun add_group (inventory, group_name, vars) =
    let
      val group = {
        name = group_name,
        vars = ref vars,
        hosts = ref []
      }
    in
      (HashTable.insert (#groups inventory) (group_name, group); group)
    end
  
  fun remove_group (inventory, group_name) =
    HashTable.remove (#groups inventory) group_name
  
  fun get_group (inventory, group_name) =
    SOME (HashTable.lookup (#groups inventory) group_name)
    handle _ => NONE
  
  fun list_groups inventory =
    HashTable.foldi (fn (name, _, acc) => name :: acc) [] (#groups inventory)
  
  (* Membership management *)
  fun add_host_to_group (inventory, hostname, group_name) =
    case get_group (inventory, group_name) of
      SOME group => 
        let
          val hosts = !(#hosts group)
        in
          if List.exists (fn h => h = hostname) hosts
          then () (* Already in group *)
          else #hosts group := hostname :: hosts
        end
    | NONE => raise InventoryError("Group not found: " ^ group_name)
  
  fun remove_host_from_group (inventory, hostname, group_name) =
    case get_group (inventory, group_name) of
      SOME group => 
        let
          val hosts = !(#hosts group)
          val new_hosts = List.filter (fn h => h <> hostname) hosts
        in
          #hosts group := new_hosts
        end
    | NONE => raise InventoryError("Group not found: " ^ group_name)
  
  fun get_group_hosts (inventory, group_name) =
    case get_group (inventory, group_name) of
      SOME group => !(#hosts group)
    | NONE => raise InventoryError("Group not found: " ^ group_name)
  
  fun get_host_groups (inventory, hostname) =
    HashTable.foldi (fn (group_name, group, acc) => 
      if List.exists (fn h => h = hostname) (!(#hosts group))
      then group_name :: acc
      else acc) [] (#groups inventory)
  
  (* Variable management *)
  fun get_host_vars (inventory, hostname) =
    case get_host (inventory, hostname) of
      SOME host => !(#vars host)
    | NONE => raise InventoryError("Host not found: " ^ hostname)
  
  fun set_host_var (inventory, hostname, key, value) =
    case get_host (inventory, hostname) of
      SOME host => 
        let
          val vars = !(#vars host)
          val vars' = (key, value) :: 
                      List.filter (fn (k, _) => k <> key) vars
        in
          #vars host := vars'
        end
    | NONE => raise InventoryError("Host not found: " ^ hostname)
  
  fun get_group_vars (inventory, group_name) =
    case get_group (inventory, group_name) of
      SOME group => !(#vars group)
    | NONE => raise InventoryError("Group not found: " ^ group_name)
  
  fun set_group_var (inventory, group_name, key, value) =
    case get_group (inventory, group_name) of
      SOME group => 
        let
          val vars = !(#vars group)
          val vars' = (key, value) :: 
                      List.filter (fn (k, _) => k <> key) vars
        in
          #vars group := vars'
        end
    | NONE => raise InventoryError("Group not found: " ^ group_name)
  
  (* Calculate effective variables with Ansible-like precedence *)
  fun get_effective_vars (inventory, hostname) =
    let
      (* Start with empty vars *)
      val result = ref []
      
      (* Helper to merge variables with precedence *)
      fun merge_vars vars =
        List.app (fn (k, v) => 
          result := (k, v) :: 
                   List.filter (fn (k', _) => k' <> k) (!result)) vars
      
      (* Get all groups for this host *)
      val host_groups = get_host_groups(inventory, hostname)
      
      (* Apply group variables in alphabetical order (Ansible behavior) *)
      val sorted_groups = ListMergeSort.sort (fn (a, b) => a > b) host_groups
      val _ = List.app (fn g => 
                merge_vars (get_group_vars(inventory, g))) sorted_groups
      
      (* Finally apply host variables (highest precedence) *)
      val _ = merge_vars (get_host_vars(inventory, hostname))
    in
      !result
    end
  
  (* Dynamic inventory support *)
  fun add_dynamic_source (inventory, source_fn) =
    #dynamic_sources inventory := source_fn :: !(#dynamic_sources inventory)
  
  fun refresh inventory =
    let
      (* Execute all dynamic sources *)
      val sources = !(#dynamic_sources inventory)
      
      (* Process each source *)
      fun process_source source_fn =
        let
          val hosts = source_fn()
        in
          List.app (fn (hostname, vars) =>
            case get_host(inventory, hostname) of
              SOME host => #vars host := vars  (* Update existing host *)
            | NONE => ignore (add_host(inventory, hostname, vars)))  (* Add new host *)
          hosts
        end
    in
      List.app process_source sources
    end
end