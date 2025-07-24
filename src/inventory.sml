(* inventory.sml
 * Ansible-compatible inventory management
 *)

structure Inventory : INVENTORY = struct
  (* Type definitions *)
  type host_vars = (string * string) list
  type group_vars = (string * string) list

  type host = {
    name: string,
    vars: host_vars ref
  }
  
  type group = {
    name: string,
    hosts: string list ref,
    vars: group_vars ref,
    children: string list ref
  }
  
  type inventory = {
    hosts: (string, host) HashTable.hash_table,
    groups: (string, group) HashTable.hash_table,
    dynamic_sources: (unit -> (string * host_vars) list) list ref
  }
  
  (* Exceptions *)
  exception InventoryError of string
  
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
  
  (* Create a new empty inventory *)
  fun create () = {
    hosts = HashTable.mkTable (HashString.hashString, op=) (50, Fail "Host not found"),
    groups = HashTable.mkTable (HashString.hashString, op=) (20, Fail "Group not found"),
    dynamic_sources = ref []
  }
  
  (* Add a group to inventory *)
  fun add_group (inv: inventory, name, init_vars) =
    let
      val group = {
        name = name,
        hosts = ref [],
        vars = ref init_vars,
        children = ref []
      }
    in
      HashTable.insert (#groups inv) (name, group);
      group
    end
  
  (* Get a group from inventory *)
  fun get_group (inv: inventory, name) =
    HashTable.find (#groups inv) name
  
  (* Remove a group - FIXED to return unit *)
  fun remove_group (inv: inventory, name) =
    let
      (* Remove group from children lists of all other groups *)
      val _ = HashTable.appi 
                (fn (_, group) => 
                   (#children group) := 
                     List.filter (fn child => child <> name) (!(#children group)))
                (#groups inv)
    in
      HashTable.remove (#groups inv) name
      handle _ => raise InventoryError ("Group not found: " ^ name)
    end
  
  (* Add a host to inventory *)
  fun add_host (inv: inventory, name, init_vars) =
    let
      val host = {
        name = name,
        vars = ref init_vars
      }
    in
      HashTable.insert (#hosts inv) (name, host);
      host
    end
  
  (* Get a host from inventory *)
  fun get_host (inv: inventory, name) =
    HashTable.find (#hosts inv) name
  
  (* Remove a host - FIXED to return unit *)
  fun remove_host (inv: inventory, name) =
    let
      (* Remove host from all groups *)
      val _ = HashTable.appi 
                (fn (_, group) => 
                   (#hosts group) := 
                     List.filter (fn host => host <> name) (!(#hosts group)))
                (#groups inv)
    in
      HashTable.remove (#hosts inv) name
      handle _ => raise InventoryError ("Host not found: " ^ name)
    end
  
  (* List all hosts *)
  fun list_hosts (inv: inventory) =
    let
      val host_list = ref []
      val _ = HashTable.appi (fn (name, _) => host_list := name :: !host_list) (#hosts inv)
    in
      !host_list
    end
  
  (* List all groups *)
  fun list_groups (inv: inventory) =
    let
      val group_list = ref []
      val _ = HashTable.appi (fn (name, _) => group_list := name :: !group_list) (#groups inv)
    in
      !group_list
    end
  
  (* Add host to group *)
  fun add_host_to_group (inv: inventory, hostname, groupname) =
    case get_group (inv, groupname) of
      SOME group =>
        let
          val host_exists = case get_host (inv, hostname) of
                              SOME _ => true
                            | NONE => false
        in
          if not host_exists then
            ignore(add_host (inv, hostname, []))
          else
            ();
            
          if not (List.exists (fn h => h = hostname) (!(#hosts group))) then
            (#hosts group) := hostname :: (!(#hosts group))
          else
            ()
        end
    | NONE => raise InventoryError ("Group not found: " ^ groupname)
  
  (* Remove host from group *)
  fun remove_host_from_group (inv: inventory, hostname, groupname) =
    case get_group (inv, groupname) of
      SOME group =>
        (#hosts group) := List.filter (fn h => h <> hostname) (!(#hosts group))
    | NONE => raise InventoryError ("Group not found: " ^ groupname)
  
  (* Get hosts in a group *)
  fun get_group_hosts (inv: inventory, group_name) =
    case get_group (inv, group_name) of
      SOME group => !(#hosts group)
    | NONE => raise InventoryError ("Group not found: " ^ group_name)
  
  (* Get groups a host belongs to *)
  fun get_host_groups (inv: inventory, host_name) =
    let
      val groups = ref []
      val _ = HashTable.appi 
                (fn (name, group) => 
                   if List.exists (fn h => h = host_name) (!(#hosts group))
                   then groups := name :: !groups
                   else ())
                (#groups inv)
    in
      !groups
    end
  
  (* Get host variables *)
  fun get_host_vars (inv: inventory, host_name) =
    case get_host (inv, host_name) of
      SOME host => !(#vars host)
    | NONE => raise InventoryError ("Host not found: " ^ host_name)
  
  (* Set host variable *)
  fun set_host_var (inv: inventory, host_name, key, value) =
    case get_host (inv, host_name) of
      SOME host => 
        let
          val current_vars = !(#vars host)
          val new_vars = 
            case List.find (fn (k, _) => k = key) current_vars of
              SOME _ => 
                map (fn (k, v) => 
                       if k = key then 
                         (k, value)
                       else 
                         (k, v))
                    current_vars
            | NONE => (key, value) :: current_vars
        in
          (#vars host) := new_vars
        end
    | NONE => raise InventoryError ("Host not found: " ^ host_name)
  
  (* Get group variables *)
  fun get_group_vars (inv: inventory, group_name) =
    case get_group (inv, group_name) of
      SOME group => !(#vars group)
    | NONE => raise InventoryError ("Group not found: " ^ group_name)
  
  (* Set group variable *)
  fun set_group_var (inv: inventory, group_name, key, value) =
    case get_group (inv, group_name) of
      SOME group => 
        let
          val current_vars = !(#vars group)
          val new_vars = 
            case List.find (fn (k, _) => k = key) current_vars of
              SOME _ => 
                map (fn (k, v) => 
                       if k = key then 
                         (k, value)
                       else 
                         (k, v))
                    current_vars
            | NONE => (key, value) :: current_vars
        in
          (#vars group) := new_vars
        end
    | NONE => raise InventoryError ("Group not found: " ^ group_name)
  
  (* Get effective variables for a host with proper precedence *)
  fun get_effective_vars (inv: inventory, host_name) =
    let
      (* First get the host vars *)
      val host_vars = 
        case get_host (inv, host_name) of
          SOME host => !(#vars host)
        | NONE => raise InventoryError ("Host not found: " ^ host_name)
      
      (* Get all groups the host belongs to *)
      val host_groups = get_host_groups (inv, host_name)
      
      (* Accumulate all group vars *)
      fun collect_group_vars [] vars = vars
        | collect_group_vars (g::gs) vars =
            let
              val group_vars = get_group_vars (inv, g)
            in
              collect_group_vars gs (vars @ group_vars)
            end
                
      val all_group_vars = collect_group_vars host_groups []
      
      (* Combine, giving host vars precedence *)
      fun merge_vars [] host_specific = host_specific
        | merge_vars ((k,v)::rest) host_specific =
            if List.exists (fn (k',_) => k = k') host_specific then
              merge_vars rest host_specific
            else
              merge_vars rest ((k,v)::host_specific)
    in
      merge_vars all_group_vars host_vars
    end
  
  (* Add a dynamic inventory source *)
  fun add_dynamic_source (inv: inventory, source_fn) =
    (#dynamic_sources inv) := source_fn :: (!(#dynamic_sources inv))
  
  (* Refresh the inventory using dynamic sources *)
  fun refresh (inv: inventory) =
    let
      fun process_source source_fn =
        let
          val hosts_with_vars = source_fn()
          
          fun process_host (hostname, host_vars) =
            case get_host (inv, hostname) of
              SOME host => 
                (* Update vars for existing host *)
                app (fn (k, v) => set_host_var (inv, hostname, k, v)) host_vars
            | NONE => 
                (* Add new host *)
                ignore(add_host (inv, hostname, host_vars))
        in
          app process_host hosts_with_vars
        end
    in
      app process_source (!(#dynamic_sources inv))
    end
  
  (* Load inventory from a file *)
  fun load_file filename =
    let
      val inv = create()
      
      val file = TextIO.openIn filename
            handle _ => raise InventoryError ("Cannot open inventory file: " ^ filename)
      
      fun read_lines current_group =
        case TextIO.inputLine file of
          NONE => ()
        | SOME line => 
            if String.size line = 0 orelse String.sub(line, 0) = #"#" then
              (* Skip empty lines and comments *)
              read_lines current_group
            else if String.sub(line, 0) = #"[" then
              (* Group definition *)
              let
                val group_line = string_trim line
                val group_name = 
                  if String.isSubstring "]" group_line then
                    String.substring(group_line, 1, 
                                    String.size group_line - 2)
                  else
                    String.substring(group_line, 1, 
                                    String.size group_line - 1)
              in
                ignore(add_group (inv, group_name, []));
                read_lines (SOME group_name)
              end
            else
              (* Host or variable definition *)
              let
                val line_trimmed = string_trim line
              in
                if String.isSubstring "=" line_trimmed then
                  (* Variable definition *)
                  let
                    val parts = String.tokens (fn c => c = #"=") line_trimmed
                    val key = string_trim (hd parts)
                    val value = string_trim (String.concatWith "=" (tl parts))
                  in
                    case current_group of
                      SOME group => set_group_var (inv, group, key, value)
                    | NONE => ();
                    read_lines current_group
                  end
                else if current_group <> NONE then
                  (* Host definition *)
                  let
                    val host_parts = String.tokens (fn c => c = #" " orelse c = #"\t") line_trimmed
                    val host_name = string_trim (hd host_parts)
                    
                    (* Check for host vars in "hostname var1=val1 var2=val2" format *)
                    val host_vars = ref []
                    val _ = 
                      app (fn part => 
                            if String.isSubstring "=" part then
                              let
                                val var_parts = String.tokens (fn c => c = #"=") part
                                val var_name = string_trim (hd var_parts)
                                val var_value = string_trim (String.concatWith "=" (tl var_parts))
                              in
                                host_vars := (var_name, var_value) :: !host_vars
                              end
                            else ())
                          (tl host_parts)
                  in
                    ignore(add_host (inv, host_name, !host_vars));
                    add_host_to_group (inv, host_name, valOf current_group);
                    read_lines current_group
                  end
                else
                  (* Unknown line format - skip *)
                  read_lines current_group
              end
    in
      read_lines NONE;
      TextIO.closeIn file;
      inv
    end
  
  (* Save inventory to a file *)
  fun save_file (inv: inventory, filename) =
    let
      val out = TextIO.openOut filename
               handle _ => raise InventoryError ("Cannot open output file: " ^ filename)
      
      (* Write all groups *)
      val groups = list_groups inv
      
      fun write_group group_name =
        let
          val _ = TextIO.output(out, "[" ^ group_name ^ "]\n")
          
          (* Write hosts in this group *)
          val hosts = get_group_hosts (inv, group_name)
          val _ = app (fn host => TextIO.output(out, host ^ "\n")) hosts
          
          (* Write group vars *)
          val vars = get_group_vars (inv, group_name)
          val _ = app (fn (k,v) => TextIO.output(out, k ^ "=" ^ v ^ "\n")) vars
          
          (* Add spacing *)
          val _ = TextIO.output(out, "\n")
        in
          ()
        end
    in
      app write_group groups;
      TextIO.closeOut out
    end
end