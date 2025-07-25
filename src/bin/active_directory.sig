(* active_directory.sig
 * Windows Active Directory integration
 *)

signature ACTIVE_DIRECTORY = sig
  type ad_connection
  type ad_object
  
  (* Object types *)
  datatype object_type = USER | GROUP | OU | COMPUTER | ANY
  
  (* Exceptions *)
  exception ADError of string
  
  (* Connection management *)
  val connect : string option -> ad_connection (* domain controller *)
  val disconnect : ad_connection -> unit
  
  (* Object operations *)
  val get_object : ad_connection * string * object_type -> ad_object
  val create_user : ad_connection * {
    name: string,
    sam_account_name: string,
    password: string option,
    description: string option,
    container: string (* e.g., "CN=Users,DC=example,DC=com" *)
  } -> ad_object
  
  val create_group : ad_connection * {
    name: string,
    description: string option,
    group_type: string, (* "Security" or "Distribution" *)
    scope: string, (* "Global", "Universal", or "DomainLocal" *)
    container: string
  } -> ad_object
  
  val delete_object : ad_connection * ad_object -> unit
  val move_object : ad_connection * ad_object * string -> unit (* object, new container *)
  
  (* Group membership *)
  val add_to_group : ad_connection * ad_object * ad_object -> unit
  val remove_from_group : ad_connection * ad_object * ad_object -> unit
  val is_member_of : ad_connection * ad_object * ad_object -> bool
  val get_group_members : ad_connection * ad_object -> ad_object list
  
  (* Property operations *)
  val get_property : ad_object * string -> string option
  val set_property : ad_connection * ad_object * string * string -> unit
  val get_properties : ad_object -> (string * string) list
  
  (* Search operations *)
  val find_objects : ad_connection * string * object_type -> ad_object list
  val find_users : ad_connection * string -> ad_object list (* search pattern *)
  val find_groups : ad_connection * string -> ad_object list (* search pattern *)
end