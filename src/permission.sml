(* permission.sml
 * Implementation of security permissions
 *)

structure Permission : PERMISSION = struct
  (* Permission flags as bitwise values *)
  val READ_FLAG = 0x1
  val WRITE_FLAG = 0x2
  val EXECUTE_FLAG = 0x4
  val ADMIN_FLAG = 0x8
  
  (* Permission type as a bitfield *)
  type permission = int
  
  (* Basic permission constants *)
  val none = 0
  val read = READ_FLAG
  val write = WRITE_FLAG
  val execute = EXECUTE_FLAG
  val admin = ADMIN_FLAG
  
  (* Permission operations *)
  fun combine (perm1, perm2) = Word.toInt(Word.orb(Word.fromInt perm1, Word.fromInt perm2))
  
  fun is_sufficient (actual, required) = 
    (* Admin can do anything *)
    (actual = admin) orelse
    (* Check if actual permission contains all required bits *)
    (Word.andb(Word.fromInt actual, Word.fromInt required) = Word.fromInt required)
  
  (* Permission checks *)
  fun has_read perm = is_sufficient(perm, read)
  fun has_write perm = is_sufficient(perm, write)
  fun has_execute perm = is_sufficient(perm, execute)
  fun has_admin perm = is_sufficient(perm, admin)
  
  (* Permission conversion *)
  fun from_string str =
    case str of
      "none" => none
    | "read" => read
    | "write" => write
    | "execute" => execute
    | "admin" => admin
    | "read,write" => combine(read, write)
    | "read,execute" => combine(read, execute)
    | "write,execute" => combine(write, execute)
    | "read,write,execute" => combine(combine(read, write), execute)
    | _ => none
  
  fun to_string perm =
    let
      val parts = []
      val parts = if has_read perm then "read"::parts else parts
      val parts = if has_write perm then "write"::parts else parts
      val parts = if has_execute perm then "execute"::parts else parts
      val parts = if has_admin perm then "admin"::parts else parts
      val parts = if null parts then ["none"] else parts
    in
      String.concatWith "," parts
    end
end