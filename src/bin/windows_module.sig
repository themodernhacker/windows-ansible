(* windows_module.sig
 * Windows-specific Ansible modules
 *)

signature WINDOWS_MODULE = sig
  type module_result = {
    changed: bool,
    failed: bool,
    msg: string,
    rc: int option,
    stdout: string option,
    stderr: string option,
    output: (string * string) list
  }
  
  (* Common Windows modules - like Ansible's windows modules *)
  val win_copy : {
    src: string,
    dest: string,
    backup: bool
  } -> module_result
  
  val win_command : {
    command: string,
    chdir: string option,
    creates: string option,
    removes: string option
  } -> module_result
  
  val win_shell : {
    command: string,
    chdir: string option,
    creates: string option,
    removes: string option
  } -> module_result
  
  val win_service : {
    name: string,
    display_name: string option,
    path: string option,
    state: string option,
    startup_mode: string option
  } -> module_result
  
  val win_regedit : {
    path: string,
    name: string,
    data: string option,
    type_: string option,
    state: string
  } -> module_result
  
  val win_feature : {
    name: string,
    state: string,
    restart_if_required: bool
  } -> module_result
  
  (* File and directory management *)
  val win_file : {
    path: string,
    state: string,
    attributes: (string * bool) list option
  } -> module_result
  
  (* ACL management *)
  val win_acl : {
    path: string,
    user: string,
    rights: string,
    type_: string,
    state: string
  } -> module_result
  
  (* Windows updates *)
  val win_updates : {
    category_names: string list option,
    log_path: string option,
    state: string
  } -> module_result
end