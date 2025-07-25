(* playbook.sig
 * Ansible-compatible playbook structure
 *)

signature PLAYBOOK = sig
  structure Inventory : INVENTORY
  structure TaskExecutor : TASK_EXECUTOR
  
  (* Playbook types *)
  type playbook
  type play
  type handler
  type play_result
  
  (* Variable scope types - Ansible compatible *)
  type var_scope
  type vars = (string * string) list
  
  (* Exceptions *)
  exception PlaybookError of string
  exception PlayError of string * string  (* play name, error message *)
  
  (* Playbook operations *)
  val load_file : string -> playbook
  val save_file : playbook * string -> unit
  
  (* Play creation - similar to Ansible play structure *)
  val create_play : {
    name: string,
    hosts: string,  (* host pattern *)
    tasks: TaskExecutor.task list,
    handlers: handler list,
    vars: vars,
    tags: string list
  } -> play
  
  (* Handler creation - analogous to Ansible handlers *)
  val create_handler : {
    name: string,
    task: TaskExecutor.task
  } -> handler
  
  (* Playbook building *)
  val create_playbook : string -> playbook
  val add_play : playbook * play -> playbook
  
  (* Playbook execution *)
  val execute : playbook * Inventory.inventory -> play_result list
  val execute_play : play * Inventory.inventory -> play_result
  
  (* Limiting execution by tags or hosts - like Ansible *)
  val execute_with_tags : playbook * Inventory.inventory * string list -> play_result list
  val execute_with_hosts : playbook * Inventory.inventory * string list -> play_result list
  
  (* Result analysis *)
  val get_play_summary : play_result -> {
    ok: int,
    changed: int,
    unreachable: int,
    failed: int,
    skipped: int
  }
  val get_task_results : play_result -> (string * TaskExecutor.change_status) list
end