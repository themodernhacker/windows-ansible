(* task_executor.sig
 * Core task execution system like Ansible
 *)

signature TASK_EXECUTOR = sig
  structure Inventory : INVENTORY
  
  (* Task definition types *)
  type task
  type task_module = string
  type module_args = (string * string) list
  type task_result
  
  (* Task state for idempotency *)
  datatype change_status = UNCHANGED | CHANGED | FAILED
  
  (* Exceptions *)
  exception TaskError of string * string
  exception ModuleNotFound of string
  
  (* Task creation *)
  val create_task : {
    name: string,
    module: task_module,
    args: module_args,
    when_condition: string option,
    register: string option
  } -> task
  
  (* Task execution *)
  val execute : task * Inventory.host -> task_result
  val execute_on_hosts : task * Inventory.host list -> (Inventory.host * task_result) list
  
  (* Task results *)
  val get_status : task_result -> change_status
  val get_output : task_result -> string
  val get_facts : task_result -> (string * string) list
  val is_failed : task_result -> bool
  val is_changed : task_result -> bool
  val is_skipped : task_result -> bool
  
  (* Module registration - extensible like Ansible *)
  val register_module : task_module * (module_args -> task_result) -> unit
  val list_available_modules : unit -> task_module list
  
  (* Support for common Ansible modules *)
  val win_command : string -> task
  val win_copy : {src: string, dest: string} -> task
  val win_service : {name: string, state: string} -> task
  val win_feature : {name: string, state: string} -> task
  val win_regedit : {path: string, name: string, data: string, type_: string} -> task
end