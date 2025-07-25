(* inventory.sig
 * Ansible-compatible inventory management
 *)

signature INVENTORY = sig
  (* Host and group types *)
  type host
  type group
  type inventory
  
  (* Variables and facts *)
  type host_vars = (string * string) list
  type group_vars = (string * string) list
  
  (* Exceptions *)
  exception InventoryError of string
  
  (* Inventory creation *)
  val create : unit -> inventory
  val load_file : string -> inventory
  val save_file : inventory * string -> unit
  
  (* Host management *)
  val add_host : inventory * string * host_vars -> host
  val remove_host : inventory * string -> unit
  val get_host : inventory * string -> host option
  val list_hosts : inventory -> string list
  
  (* Group management *)
  val add_group : inventory * string * group_vars -> group
  val remove_group : inventory * string -> unit
  val get_group : inventory * string -> group option
  val list_groups : inventory -> string list
  
  (* Membership management *)
  val add_host_to_group : inventory * string * string -> unit
  val remove_host_from_group : inventory * string * string -> unit
  val get_group_hosts : inventory * string -> string list
  val get_host_groups : inventory * string -> string list
  
  (* Variable management - Ansible compatible *)
  val get_host_vars : inventory * string -> host_vars
  val set_host_var : inventory * string * string * string -> unit
  val get_group_vars : inventory * string -> group_vars
  val set_group_var : inventory * string * string * string -> unit
  
  (* Combined variable resolution with proper precedence - like Ansible *)
  val get_effective_vars : inventory * string -> (string * string) list
  
  (* Dynamic inventory support *)
  val add_dynamic_source : inventory * (unit -> (string * host_vars) list) -> unit
  val refresh : inventory -> unit
end