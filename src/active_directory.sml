(* active_directory.sml
 * Implementation of Windows Active Directory integration
 *)

structure ActiveDirectory : ACTIVE_DIRECTORY = struct
  (* Type definitions *)
  datatype object_type = USER | GROUP | OU | COMPUTER | ANY
  
  type ad_connection = {
    domain_controller: string option,
    domain: string,
    connected: bool ref
  }
  
  type ad_object = {
    distinguished_name: string,
    object_type: object_type,
    properties: (string * string) list ref
  }
  
  exception ADError of string
  
  (* Helper function to execute PowerShell commands *)
  fun execute_powershell cmd =
    let
      val temp_out = OS.FileSys.tmpName()
      val temp_err = OS.FileSys.tmpName()
      
      (* Construct the command to execute with output redirection *)
      val ps_cmd = "powershell -NonInteractive -NoProfile -ExecutionPolicy Bypass -Command \"" ^ 
                   cmd ^ "\" > \"" ^ temp_out ^ "\" 2> \"" ^ temp_err ^ "\""
      
      (* Execute the command and capture return code *)
      val process_status = OS.Process.system ps_cmd
      val rc = if OS.Process.isSuccess process_status then 0 else 1
      
      (* Read the output and error files *)
      val stdout = TextIO.inputAll(TextIO.openIn temp_out)
                   handle _ => ""
      val stderr = TextIO.inputAll(TextIO.openIn temp_err)
                   handle _ => ""
                   
      (* Clean up temporary files *)
      val _ = (OS.FileSys.remove temp_out; OS.FileSys.remove temp_err)
               handle _ => ()
    in
      (rc, stdout, stderr)
    end
  
  (* Connect to Active Directory *)
  fun connect domain_controller =
    let
      (* Get current domain if not specified *)
      val (domain_rc, domain_out, _) = 
        execute_powershell "(Get-WmiObject Win32_ComputerSystem).Domain"
      
      val domain = 
        if domain_rc = 0 andalso String.size domain_out > 0 then
          String.substring(domain_out, 0, String.size domain_out - 1) (* Remove newline *)
        else
          raise ADError("Failed to determine domain")
          
      (* Test connection to domain *)
      val (test_rc, _, _) = 
        execute_powershell "try { Get-ADDomain -ErrorAction Stop; $true } catch { $false }"
        
      val _ = 
        if test_rc <> 0 then
          raise ADError("Failed to connect to Active Directory. Make sure AD PowerShell module is installed.")
        else
          ()
    in
      {
        domain_controller = domain_controller,
        domain = domain,
        connected = ref true
      }
    end
    
  (* Disconnect from AD - cleanup resources if needed *)
  fun disconnect connection =
    #connected connection := false
  
  (* Convert AD object type to PowerShell filter *)
  fun object_type_to_filter obj_type =
    case obj_type of
      USER => "ObjectClass -eq 'user'"
    | GROUP => "ObjectClass -eq 'group'"
    | OU => "ObjectClass -eq 'organizationalUnit'"
    | COMPUTER => "ObjectClass -eq 'computer'"
    | ANY => "" (* No filter *)
  
  (* Get AD object by distinguished name *)
  fun get_object (connection, dn, obj_type) =
    let
      val type_filter = object_type_to_filter obj_type
      
      (* Build command based on object type *)
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Get-ADObject -Identity '" ^ dn ^ "'" ^
        (if type_filter <> "" then " -Filter '" ^ type_filter ^ "'" else "") ^
        " -Properties * | ConvertTo-Json"
        
      val (rc, stdout, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to get AD object: " ^ stderr)
              else
                ()
                
      (* Very simplified parsing - would use a proper JSON parser in real implementation *)
      val properties = 
        let
          val lines = String.tokens (fn c => c = #"\n") stdout
          
          (* Very naive property extraction - just for demonstration *)
          fun extract_prop line =
            let
              val parts = String.tokens (fn c => c = #":") line
            in
              if length parts >= 2 then
                let
                  val key = String.trim(hd parts)
                  val value = String.trim(String.concatWith ":" (tl parts))
                  
                  (* Remove quotes *)
                  val key = 
                    if String.size key >= 2 andalso 
                       String.sub(key, 0) = #"\"" andalso
                       String.sub(key, String.size key - 1) = #"\"" then
                      String.substring(key, 1, String.size key - 2)
                    else
                      key
                      
                  val value = 
                    if String.size value >= 2 andalso 
                       String.sub(value, 0) = #"\"" andalso
                       String.sub(value, String.size value - 1) = #"\"" then
                      String.substring(value, 1, String.size value - 2)
                    else
                      value
                in
                  SOME (key, value)
                end
              else
                NONE
            end
            
          val props = List.mapPartial extract_prop lines
          
          (* Extract distinguished name *)
          val dn = 
            case List.find (fn (k, _) => k = "DistinguishedName") props of
              SOME (_, v) => v
            | NONE => dn (* Use provided DN if not found *)
        in
          props
        end
    in
      {
        distinguished_name = dn,
        object_type = obj_type,
        properties = ref properties
      }
    end
    handle e => raise ADError("Error getting AD object: " ^ exnMessage e)
  
  (* Create a new user *)
  fun create_user (connection, {name, sam_account_name, password, description, container}) =
    let
      (* Build command to create user *)
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "New-ADUser -Name '" ^ name ^ "'" ^
        " -SamAccountName '" ^ sam_account_name ^ "'" ^
        " -Path '" ^ container ^ "'" ^
        (case description of 
           SOME desc => " -Description '" ^ desc ^ "'"
         | NONE => "") ^
        (case password of
           SOME pwd => " -AccountPassword (ConvertTo-SecureString -String '" ^ 
                       pwd ^ "' -AsPlainText -Force) -Enabled $true"
         | NONE => "") ^
        " -PassThru | Select-Object -ExpandProperty DistinguishedName"
        
      val (rc, stdout, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to create user: " ^ stderr)
              else
                ()
                
      (* Get the DN of the new user *)
      val dn = String.substring(stdout, 0, String.size stdout - 1) (* Remove newline *)
    in
      get_object(connection, dn, USER)
    end
    handle e => raise ADError("Error creating user: " ^ exnMessage e)
  
  (* Create a new group *)
  fun create_group (connection, {name, description, group_type, scope, container}) =
    let
      (* Convert group scope to PowerShell parameter *)
      val scope_param = 
        case scope of
          "Global" => "Global"
        | "Universal" => "Universal"
        | "DomainLocal" => "DomainLocal"
        | _ => raise ADError("Invalid group scope: " ^ scope)
        
      (* Convert group type to PowerShell parameter *)
      val type_param = 
        case group_type of
          "Security" => "Security"
        | "Distribution" => "Distribution"
        | _ => raise ADError("Invalid group type: " ^ group_type)
      
      (* Build command to create group *)
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "New-ADGroup -Name '" ^ name ^ "'" ^
        " -GroupCategory '" ^ type_param ^ "'" ^
        " -GroupScope '" ^ scope_param ^ "'" ^
        " -Path '" ^ container ^ "'" ^
        (case description of 
           SOME desc => " -Description '" ^ desc ^ "'"
         | NONE => "") ^
        " -PassThru | Select-Object -ExpandProperty DistinguishedName"
        
      val (rc, stdout, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to create group: " ^ stderr)
              else
                ()
                
      (* Get the DN of the new group *)
      val dn = String.substring(stdout, 0, String.size stdout - 1) (* Remove newline *)
    in
      get_object(connection, dn, GROUP)
    end
    handle e => raise ADError("Error creating group: " ^ exnMessage e)
  
  (* Delete an AD object *)
  fun delete_object (connection, obj) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Remove-ADObject -Identity '" ^ (#distinguished_name obj) ^ "' -Confirm:$false"
        
      val (rc, _, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to delete object: " ^ stderr)
              else
                ()
    in
      ()
    end
    handle e => raise ADError("Error deleting object: " ^ exnMessage e)
  
  (* Move an AD object to a new container *)
  fun move_object (connection, obj, new_container) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Move-ADObject -Identity '" ^ (#distinguished_name obj) ^ 
        "' -TargetPath '" ^ new_container ^ "'"
        
      val (rc, _, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to move object: " ^ stderr)
              else
                ()
    in
      ()
    end
    handle e => raise ADError("Error moving object: " ^ exnMessage e)
  
  (* Add a member to a group *)
  fun add_to_group (connection, member, group) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Add-ADGroupMember -Identity '" ^ (#distinguished_name group) ^ 
        "' -Members '" ^ (#distinguished_name member) ^ "'"
        
      val (rc, _, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to add to group: " ^ stderr)
              else
                ()
    in
      ()
    end
    handle e => raise ADError("Error adding to group: " ^ exnMessage e)
  
  (* Remove a member from a group *)
  fun remove_from_group (connection, member, group) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Remove-ADGroupMember -Identity '" ^ (#distinguished_name group) ^ 
        "' -Members '" ^ (#distinguished_name member) ^ "' -Confirm:$false"
        
      val (rc, _, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to remove from group: " ^ stderr)
              else
                ()
    in
      ()
    end
    handle e => raise ADError("Error removing from group: " ^ exnMessage e)
  
  (* Check if an object is a member of a group *)
  fun is_member_of (connection, member, group) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "if (Get-ADGroupMember -Identity '" ^ (#distinguished_name group) ^ 
        "' | Where-Object { $_.DistinguishedName -eq '" ^ (#distinguished_name member) ^ 
        "' }) { 'True' } else { 'False' }"
        
      val (rc, stdout, _) = execute_powershell cmd
      
      val result = 
        if rc = 0 andalso String.isSubstring "True" stdout then
          true
        else
          false
    in
      result
    end
    handle _ => false
  
  (* Get all members of a group *)
  fun get_group_members (connection, group) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Get-ADGroupMember -Identity '" ^ (#distinguished_name group) ^ 
        "' | Select-Object -ExpandProperty DistinguishedName"
        
      val (rc, stdout, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to get group members: " ^ stderr)
              else
                ()
                
      val member_dns = String.tokens (fn c => c = #"\n") stdout
      
      (* Create AD objects for each member *)
      val members = 
        List.map (fn dn => 
                   let
                     (* Check object type - simplified version *)
                     val obj_type = 
                       if String.isSubstring "CN=Users" dn then USER
                       else if String.isSubstring "OU=" dn then OU
                       else if String.isSubstring "CN=Computers" dn then COMPUTER
                       else ANY
                   in
                     get_object(connection, dn, obj_type)
                   end)
                 member_dns
    in
      members
    end
    handle e => raise ADError("Error getting group members: " ^ exnMessage e)
  
  (* Get a property from an AD object *)
  fun get_property (obj, property_name) =
    let
      val properties = !(#properties obj)
    in
      case List.find (fn (name, _) => name = property_name) properties of
        SOME (_, value) => SOME value
      | NONE => NONE
    end
  
  (* Set a property on an AD object *)
  fun set_property (connection, obj, property_name, property_value) =
    let
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Set-ADObject -Identity '" ^ (#distinguished_name obj) ^ 
        "' -Replace @{" ^ property_name ^ "='" ^ property_value ^ "'}"
        
      val (rc, _, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to set property: " ^ stderr)
              else
                (* Update the local property cache *)
                let
                  val current_props = !(#properties obj)
                  val new_props = 
                    (property_name, property_value) ::
                    List.filter (fn (name, _) => name <> property_name) current_props
                in
                  #properties obj := new_props
                end
    in
      ()
    end
    handle e => raise ADError("Error setting property: " ^ exnMessage e)
  
  (* Get all properties of an AD object *)
  fun get_properties obj =
    !(#properties obj)
  
  (* Find AD objects by search criteria *)
  fun find_objects (connection, search_filter, obj_type) =
    let
      val type_filter = object_type_to_filter obj_type
      
      (* Combine filters *)
      val combined_filter = 
        if type_filter <> "" andalso search_filter <> "" then
          "(" ^ type_filter ^ ") -and (" ^ search_filter ^ ")"
        else if type_filter <> "" then
          type_filter
        else if search_filter <> "" then
          search_filter
        else
          "*"
          
      (* Build search command *)
      val cmd = 
        "Import-Module ActiveDirectory; " ^
        "Get-ADObject -Filter '" ^ combined_filter ^ 
        "' -Properties DistinguishedName | " ^
        "Select-Object -ExpandProperty DistinguishedName"
        
      val (rc, stdout, stderr) = execute_powershell cmd
      
      val _ = if rc <> 0 then
                raise ADError("Failed to find objects: " ^ stderr)
              else
                ()
                
      val object_dns = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
      
      (* Create AD objects for each result *)
      val objects = 
        List.map (fn dn => 
                   if String.size dn > 0 then
                     get_object(connection, dn, obj_type)
                   else
                     raise ADError("Empty distinguished name returned"))
                 (List.filter (fn dn => String.size dn > 0) object_dns)
    in
      objects
    end
    handle e => raise ADError("Error finding objects: " ^ exnMessage e)
  
  (* Find users by search pattern *)
  fun find_users (connection, search_pattern) =
    let
      (* Build search filter for users *)
      val filter = 
        if String.isSubstring "=" search_pattern then
          (* Assume it's already a proper LDAP filter *)
          search_pattern
        else
          (* Simple name search *)
          "Name -like '*" ^ search_pattern ^ "*'"
    in
      find_objects(connection, filter, USER)
    end
  
  (* Find groups by search pattern *)
  fun find_groups (connection, search_pattern) =
    let
      (* Build search filter for groups *)
      val filter = 
        if String.isSubstring "=" search_pattern then
          (* Assume it's already a proper LDAP filter *)
          search_pattern
        else
          (* Simple name search *)
          "Name -like '*" ^ search_pattern ^ "*'"
    in
      find_objects(connection, filter, GROUP)
    end
end