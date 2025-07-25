(* template.sig
 * Ansible-like templating system
 *)

signature TEMPLATE = sig
  (* Variable resolution *)
  type vars = (string * string) list
  
  (* Exceptions *)
  exception TemplateError of string
  exception UndefinedVariable of string
  
  (* Template processing - like Ansible's Jinja2 templates *)
  val process_string : string * vars -> string
  val process_file : string * vars * string -> unit  (* input_file, vars, output_file *)
  
  (* Template features - similar to common Jinja2/Ansible features *)
  val has_variable : string * string -> bool  (* template, var_name *)
  val extract_variables : string -> string list  (* Get all variables in template *)
  
  (* Filters - like Ansible's Jinja2 filters *)
  val register_filter : string * (string -> string) -> unit
  val get_filter : string -> (string -> string) option
  val list_filters : unit -> string list
  
  (* Default filters - similar to common Ansible filters *)
  val default_filters : unit -> (string * (string -> string)) list
end