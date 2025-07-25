(* permission.sig
 * Permission system for security enforcement
 *)

signature PERMISSION = sig
  (* Abstract permission type *)
  type permission
  
  (* Basic permission constants *)
  val none : permission
  val read : permission
  val write : permission
  val execute : permission
  val admin : permission
  
  (* Permission operations *)
  val combine : permission * permission -> permission
  val is_sufficient : permission * permission -> bool
  
  (* Permission checks *)
  val has_read : permission -> bool
  val has_write : permission -> bool
  val has_execute : permission -> bool
  val has_admin : permission -> bool
  
  (* Permission conversion *)
  val from_string : string -> permission
  val to_string : permission -> string
end