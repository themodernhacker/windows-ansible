(* vault.sig
 * Secret management for secure storage
 *)

signature VAULT = sig
  type vault
  type secret = string
  
  exception VaultError of string
  exception AuthenticationError of string
  
  (* Vault operations *)
  val create : string * string -> vault  (* path, password *)
  val open_vault : string * string -> vault (* path, password *)
  val close_vault : vault -> unit
  
  (* Secret management *)
  val encrypt_string : vault * string -> string
  val decrypt_string : vault * string -> string
  val encrypt_file : vault * string * string -> unit (* vault, input_path, output_path *)
  val decrypt_file : vault * string * string -> unit (* vault, input_path, output_path *)
  
  (* Variable operations - for integration with playbooks *)
  val get_secret : vault * string -> secret option (* vault, key *)
  val set_secret : vault * string * string -> unit (* vault, key, value *)
  val list_secrets : vault -> string list
  
  (* Convert between vault format and plain text *)
  val is_vault_encrypted : string -> bool
  val get_vault_id : string -> string option
end