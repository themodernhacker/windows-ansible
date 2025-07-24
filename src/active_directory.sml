(* active_directory.sml
 * Active Directory integration for Windows Ansible Core
 *)

structure ActiveDirectory : ACTIVE_DIRECTORY = struct
  (* Type definitions *)
  type connection = {
    server: string,
    port: int,
    use_ssl: bool,
    domain: string,
    username: string,
    connected: bool,
    connection_string: string
  }
  
  type ad_object = {
    distinguished_name: string,
    object_class: string,
    properties: (string * string) list ref
  }
  
  (* Exceptions *)
  exception ConnectionError of string
  exception AuthenticationError of string
  exception QueryError of string
  exception ObjectNotFound of string
  
  (* Default connection *)
  val default_connection = {
    server = "",
    port = 389,
    use_ssl = false,
    domain = "",
    username = "",
    connected = false,
    connection_string = ""
  }
  
  (* Current connection state *)
  val current_connection = ref default_connection
  
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

  (* Trim function since String.trim is not available *)
  fun string_trim s =
    let
      val chars = explode s
      
      (* Skip leading whitespace *)
      fun skipLeading [] = []
        | skipLeading (c::cs) = 
            if Char.isSpace c then skipLeading cs else c::cs
            
      (* Skip trailing whitespace *)
      fun skipTrailing [] = []
        | skipTrailing lst =
            case List.rev lst of
              [] => []
            | (c::cs) => if Char.isSpace c 
                         then List.rev (skipTrailing (List.rev cs))
                         else lst
    in
      implode (skipTrailing (skipLeading chars))
    end

  (* Connect to Active Directory *)
  fun connect {server, port, use_ssl, domain, username, password} =
    let
      (* Build connection string *)
      val protocol = if use_ssl then "ldaps" else "ldap"
      val conn_string = protocol ^ "://" ^ server ^ ":" ^ Int.toString port
      
      (* Test connection with PowerShell *)
      val test_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString '" ^ password ^ "' -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  domain ^ "\\" ^ username ^ "', $secpasswd);" ^
        "  $ldap = New-Object System.DirectoryServices.DirectoryEntry('" ^ 
                conn_string ^ "', $creds.UserName, $($creds.GetNetworkCredential().Password));" ^
        "  if ($ldap.Name -ne $null) {" ^
        "    Write-Output 'Connection successful'" ^
        "  } else {" ^
        "    Write-Error 'Connection failed'" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Connection error: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell test_cmd
    in
      if rc = 0 andalso String.isSubstring "Connection successful" stdout then
        (* Update current connection *)
        let
          val new_connection = {
            server = server,
            port = port,
            use_ssl = use_ssl,
            domain = domain,
            username = username,
            connected = true,
            connection_string = conn_string
          }
        in
          current_connection := new_connection;
          true
        end
      else
        raise ConnectionError (
          if stderr = "" then 
            "Failed to connect to Active Directory"
          else 
            "Connection error: " ^ stderr
        )
    end
    
  (* Check if we're connected *)
  fun is_connected () = #connected (!current_connection)
  
  (* Get the current connection *)
  fun get_connection () = !current_connection
  
  (* Disconnect *)
  fun disconnect () = 
    (current_connection := default_connection; 
     true)
  
  (* Search AD objects *)
  fun search_objects base_dn filter attributes =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
              
      (* Parse attributes list *)
      val attrs_str = 
        if List.length attributes = 0 then 
          ""
        else
          "," ^ String.concatWith "," attributes
              
      (* Build search command *)
      val conn = !current_connection
      val domain_parts = String.tokens (fn c => c = #".") (#domain conn)
      val root_dn = if base_dn = "" then
                      String.concatWith "," 
                        (map (fn part => "DC=" ^ part) domain_parts)
                    else
                      base_dn
                     
      val search_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain conn) ^ "\\" ^ (#username conn) ^ "', $secpasswd);" ^
        "  $root = New-Object System.DirectoryServices.DirectoryEntry('" ^ 
                (#connection_string conn) ^ "/" ^ root_dn ^ 
                "', $creds.UserName, $($creds.GetNetworkCredential().Password));" ^
        "  $searcher = New-Object System.DirectoryServices.DirectorySearcher($root);" ^
        "  $searcher.Filter = '" ^ filter ^ "';" ^
        "  $searcher.SearchScope = 'Subtree';" ^
        "  $results = $searcher.FindAll();" ^
        "  foreach ($result in $results) {" ^
        "    $dn = $result.Properties['distinguishedname'][0];" ^
        "    Write-Output \"DN:$dn\";" ^
        "    foreach ($prop in $result.Properties.Keys) {" ^
        "      $values = $result.Properties[$prop];" ^
        "      foreach ($value in $values) {" ^
        "        Write-Output \"$prop=$value\";" ^
        "      }" ^
        "    }" ^
        "    Write-Output \"---\";" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Search error: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell search_cmd
      
      (* Parse results *)
      val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
      
      (* Group lines by object *)
      fun parse_lines [] current_obj objects = 
            if current_obj = [] then objects
            else List.rev current_obj :: objects
        | parse_lines ("---"::rest) current_obj objects =
            parse_lines rest [] (List.rev current_obj :: objects)
        | parse_lines (line::rest) current_obj objects =
            parse_lines rest (line :: current_obj) objects
            
      val grouped_lines = List.rev (parse_lines result_lines [] [])
      
      (* Parse each object *)
      fun parse_object lines =
        let
          (* Extract DN and class *)
          val dn_line = 
            case List.find (fn l => String.isPrefix "DN:" l) lines of
              SOME l => string_trim (String.extract(l, 3, NONE))
            | NONE => raise QueryError "Invalid object format: no DN found"
            
          val class_line = 
            case List.find (fn l => String.isPrefix "objectClass=" l) lines of
              SOME l => string_trim (String.extract(l, 12, NONE))
            | NONE => "unknown"
            
          (* Parse properties *)
          fun parse_prop line =
            case String.tokens (fn c => c = #"=") line of
              [name, value] => (string_trim name, string_trim value)
            | _ => ("unknown", line)
            
          val props = 
            List.filter (fn l => String.isSubstring "=" l andalso 
                                not (String.isPrefix "DN:" l))
                        lines
          val properties = map parse_prop props
        in
          {
            distinguished_name = dn_line,
            object_class = class_line,
            properties = ref properties
          }
        end
    in
      if rc <> 0 then
        raise QueryError ("Search failed: " ^ stderr)
      else
        map parse_object grouped_lines
    end
    
  (* Get a single AD object by DN *)
  fun get_object dn =
    let
      val objects = search_objects "" ("(distinguishedName=" ^ dn ^ ")") []
    in
      case objects of
        [] => raise ObjectNotFound ("Object not found: " ^ dn)
      | (obj::_) => obj
    end
  
  (* Get a user object *)
  fun get_user username =
    let
      val objects = search_objects "" ("(sAMAccountName=" ^ username ^ ")") []
    in
      case objects of
        [] => raise ObjectNotFound ("User not found: " ^ username)
      | (obj::_) => obj
    end

  (* Get a group object *)
  fun get_group groupname =
    let
      val objects = search_objects "" 
                    ("(&(objectClass=group)(cn=" ^ groupname ^ "))") 
                    []
    in
      case objects of
        [] => raise ObjectNotFound ("Group not found: " ^ groupname)
      | (obj::_) => obj
    end

  (* Get a computer object *)
  fun get_computer computername =
    let
      val objects = search_objects "" 
                    ("(&(objectClass=computer)(cn=" ^ computername ^ "))") 
                    []
    in
      case objects of
        [] => raise ObjectNotFound ("Computer not found: " ^ computername)
      | (obj::_) => obj
    end

  (* Check if user is member of group *)
  fun is_user_in_group {user_dn, group_dn} =
    let
      (* Get the group object *)
      val group = get_object group_dn
      
      (* Check group membership *)
      val member_check_cmd =
        "try {" ^
        "  $conn = [ADSI]\"" ^ (#connection_string (!current_connection)) ^ "/" ^ group_dn ^ "\";" ^
        "  $user = [ADSI]\"" ^ (#connection_string (!current_connection)) ^ "/" ^ user_dn ^ "\";" ^
        "  $members = $conn.Properties['member'];" ^
        "  if ($members -contains $user.distinguishedName) {" ^
        "    Write-Output 'TRUE'" ^
        "  } else {" ^
        "    Write-Output 'FALSE'" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Membership check failed: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell member_check_cmd
    in
      rc = 0 andalso String.isSubstring "TRUE" stdout
    end

  (* Create user *)
  fun create_user {parent_ou, username, display_name, 
                   password=pw, must_change_password=must_change} =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
              
      (* Build command *)
      val create_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  $userpass = ConvertTo-SecureString '" ^ pw ^ "' -AsPlainText -Force;" ^
        "  New-ADUser -Credential $creds -SamAccountName '" ^ username ^ "'" ^
        "    -Name '" ^ display_name ^ "' -DisplayName '" ^ display_name ^ "'" ^
        "    -Path '" ^ parent_ou ^ "' -AccountPassword $userpass" ^
        "    -ChangePasswordAtLogon $" ^ (if must_change then "true" else "false") ^ ";" ^
        "  Write-Output 'User created successfully'" ^
        "} catch {" ^
        "  Write-Error ('User creation failed: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell create_cmd
    in
      if rc = 0 andalso String.isSubstring "created successfully" stdout then
        true
      else
        raise QueryError ("Failed to create user: " ^ stderr)
    end

  (* Create group - Fixed "type" parameter name to "group_type" *)
  fun create_group {parent_ou, groupname, description, scope="Global", group_type="Security"} =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
              
      (* Build command *)
      val create_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  New-ADGroup -Credential $creds -Name '" ^ groupname ^ "'" ^
        "    -Path '" ^ parent_ou ^ "' -Description '" ^ description ^ "'" ^
        "    -GroupScope " ^ scope ^ " -GroupCategory " ^ group_type ^ ";" ^
        "  Write-Output 'Group created successfully'" ^
        "} catch {" ^
        "  Write-Error ('Group creation failed: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell create_cmd
    in
      if rc = 0 andalso String.isSubstring "created successfully" stdout then
        true
      else
        raise QueryError ("Failed to create group: " ^ stderr)
    end

  (* Create computer *)
  fun create_computer {parent_ou, computername, description=""} =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
              
      (* Build command *)
      val create_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  New-ADComputer -Credential $creds -Name '" ^ computername ^ "'" ^
        "    -Path '" ^ parent_ou ^ "'" ^
        (if description <> "" then " -Description '" ^ description ^ "'" else "") ^ ";" ^
        "  Write-Output 'Computer created successfully'" ^
        "} catch {" ^
        "  Write-Error ('Computer creation failed: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell create_cmd
    in
      if rc = 0 andalso String.isSubstring "created successfully" stdout then
        true
      else
        raise QueryError ("Failed to create computer: " ^ stderr)
    end

  (* Create OU *)
  fun create_ou {parent_ou, name, description=""} =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
              
      (* Build command *)
      val create_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  New-ADOrganizationalUnit -Credential $creds -Name '" ^ name ^ "'" ^
        "    -Path '" ^ parent_ou ^ "'" ^
        (if description <> "" then " -Description '" ^ description ^ "'" else "") ^ ";" ^
        "  Write-Output 'OU created successfully'" ^
        "} catch {" ^
        "  Write-Error ('OU creation failed: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell create_cmd
    in
      if rc = 0 andalso String.isSubstring "created successfully" stdout then
        true
      else
        raise QueryError ("Failed to create OU: " ^ stderr)
    end

  (* Add user to group *)
  fun add_user_to_group {user_dn, group_dn} =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
              
      (* Build command *)
      val add_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  Add-ADGroupMember -Credential $creds -Identity '" ^ group_dn ^ "'" ^
        "    -Members '" ^ user_dn ^ "';" ^
        "  Write-Output 'User added to group successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to add user to group: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell add_cmd
    in
      if rc = 0 andalso String.isSubstring "successfully" stdout then
        true
      else
        raise QueryError ("Failed to add user to group: " ^ stderr)
    end

  (* Get property from AD object *)
  fun get_property obj property_name =
    let
      val props = !(#properties obj)
      val prop = List.find (fn (name, _) => name = property_name) props
    in
      case prop of
        SOME (_, value) => SOME value
      | NONE => NONE
    end

  (* Set property on AD object *)
  fun set_property obj property_name property_value =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
                
      (* Build command *)
      val set_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  Set-ADObject -Credential $creds -Identity '" ^ (#distinguished_name obj) ^ "'" ^
        "    -Replace @{'" ^ property_name ^ "'='" ^ property_value ^ "'};" ^
        "  Write-Output 'Property updated successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to set property: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell set_cmd
    in
      if rc = 0 andalso String.isSubstring "successfully" stdout then
        let
          (* Update local cache of properties *)
          val current_props = !(#properties obj)
          val new_props = 
            case List.find (fn (name, _) => name = property_name) current_props of
              SOME _ => 
                map (fn (name, value) => 
                       if name = property_name then 
                         (name, property_value)
                       else 
                         (name, value))
                    current_props
            | NONE => (property_name, property_value) :: current_props
        in
          (#properties obj) := new_props;
          true
        end
      else
        raise QueryError ("Failed to set property: " ^ stderr)
    end

  (* Delete AD object *)
  fun delete_object dn =
    let
      (* Verify connection *)
      val _ = if not (is_connected()) then
                raise ConnectionError "Not connected to Active Directory"
              else
                ()
                
      (* Build command *)
      val delete_cmd = 
        "try {" ^
        "  $secpasswd = ConvertTo-SecureString (Read-Host -AsSecureString) -AsPlainText -Force;" ^
        "  $creds = New-Object System.Management.Automation.PSCredential('" ^ 
                  (#domain (!current_connection)) ^ "\\" ^ (#username (!current_connection)) ^ 
                  "', $secpasswd);" ^
        "  Remove-ADObject -Credential $creds -Identity '" ^ dn ^ "' -Confirm:$false;" ^
        "  Write-Output 'Object deleted successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to delete object: ' + $_.Exception.Message)" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell delete_cmd
    in
      if rc = 0 andalso String.isSubstring "successfully" stdout then
        true
      else
        raise QueryError ("Failed to delete object: " ^ stderr)
    end
end