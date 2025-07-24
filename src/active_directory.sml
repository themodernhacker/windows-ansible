(* active_directory.sml
 * Active Directory integration for Windows Ansible Core
 *)

structure ActiveDirectory : ACTIVE_DIRECTORY = struct
  (* Type definitions to match signature *)
  datatype object_type = USER | GROUP | COMPUTER | OU | ANY
  
  type ad_connection = {
    server: string,
    port: int,
    use_ssl: bool,
    domain: string,
    username: string,
    connected: bool
  }
  
  type ad_object = {
    distinguished_name: string,
    object_class: string,
    object_type: object_type,
    properties: (string * string) list
  }
  
  (* Exceptions *)
  exception ADError of string
  
  (* Current connection state *)
  val default_connection = {
    server = "",
    port = 389,
    use_ssl = false,
    domain = "",
    username = "",
    connected = false
  }
  
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
      
      fun skipLeading [] = []
        | skipLeading (c::cs) = 
            if Char.isSpace c then skipLeading cs else c::cs
            
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

  (* Connect to Active Directory - matching signature *)
  fun connect (SOME config_path) =
    let
      (* Read config file and extract connection parameters *)
      val config_lines = 
        let
          val file = TextIO.openIn config_path
                     handle _ => raise ADError ("Cannot open config file: " ^ config_path)
          val lines = TextIO.inputAll file
          val _ = TextIO.closeIn file
        in
          String.tokens (fn c => c = #"\n") lines
        end
        
      (* Parse config lines to get connection parameters *)
      fun get_config_value key =
        case List.find (fn line => String.isSubstring (key ^ "=") line) config_lines of
          SOME line =>
            let
              val parts = String.tokens (fn c => c = #"=") line
            in
              if length parts >= 2 then string_trim(List.nth(parts, 1))
              else raise ADError ("Invalid config format for key: " ^ key)
            end
        | NONE => raise ADError ("Missing config value for: " ^ key)
        
      val server = get_config_value "server"
      val port_str = get_config_value "port"
      val port = 
        case Int.fromString port_str of
          SOME p => p
        | NONE => 389 (* default LDAP port *)
      val use_ssl_str = get_config_value "use_ssl"
      val use_ssl = use_ssl_str = "true" orelse use_ssl_str = "yes"
      val domain = get_config_value "domain"
      val username = get_config_value "username"
      val password = get_config_value "password"
      
      (* Build connection string *)
      val protocol = if use_ssl then "ldaps" else "ldap"
      val conn_string = protocol ^ "://" ^ server ^ ":" ^ Int.toString port
      
      (* Test connection *)
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
            connected = true
          }
        in
          current_connection := new_connection;
          ()
        end
      else
        raise ADError (
          if stderr = "" then 
            "Failed to connect to Active Directory"
          else 
            "Connection error: " ^ stderr
        )
    end
  | connect NONE =
    (* Use default connection parameters - typically from environment *)
    let
      val server = "localhost" (* Default values - should be overridden in practice *)
      val port = 389
      val use_ssl = false
      val domain = "WORKGROUP"
      val username = "Administrator"
      
      (* Update current connection *)
      val new_connection = {
        server = server,
        port = port,
        use_ssl = use_ssl,
        domain = domain,
        username = username,
        connected = true
      }
    in
      current_connection := new_connection;
      ()
    end
  
  (* Disconnect *)
  fun disconnect () = (current_connection := default_connection)
  
  (* Object type string conversion *)
  fun object_type_to_string USER = "user"
    | object_type_to_string GROUP = "group"
    | object_type_to_string COMPUTER = "computer"
    | object_type_to_string OU = "organizationalUnit"
    | object_type_to_string ANY = "any"
    
  fun string_to_object_type "user" = USER
    | string_to_object_type "group" = GROUP
    | string_to_object_type "computer" = COMPUTER
    | string_to_object_type "organizationalUnit" = OU
    | string_to_object_type _ = ANY
  
  (* Get a single AD object by DN *)
  fun get_object (conn, dn, obj_type) =
    let
      val type_filter = 
        case obj_type of
          ANY => ""
        | _ => "(objectClass=" ^ object_type_to_string obj_type ^ ")"
          
      val filter = 
        if type_filter = "" then
          "(distinguishedName=" ^ dn ^ ")"
        else
          "(&(distinguishedName=" ^ dn ^ ")" ^ type_filter ^ ")"
          
      (* Build search command *)
      val search_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  $root = New-Object System.DirectoryServices.DirectoryEntry(" ^ 
                "'LDAP://" ^ (#server (!current_connection)) ^ ":" ^ 
                Int.toString(#port (!current_connection)) ^ "');" ^
        "  $searcher = New-Object System.DirectoryServices.DirectorySearcher($root);" ^
        "  $searcher.Filter = '" ^ filter ^ "';" ^
        "  $searcher.SearchScope = 'Subtree';" ^
        "  $result = $searcher.FindOne();" ^
        "  if ($result -eq $null) {" ^
        "    Write-Error 'Object not found'" ^
        "    exit 1" ^
        "  }" ^
        "  $dn = $result.Properties['distinguishedname'][0];" ^
        "  Write-Output \"DN=$dn\";" ^
        "  $objClass = $result.Properties['objectclass'][0];" ^
        "  Write-Output \"ObjectClass=$objClass\";" ^
        "  foreach ($prop in $result.Properties.Keys) {" ^
        "    foreach ($val in $result.Properties[$prop]) {" ^
        "      Write-Output \"$prop=$val\";" ^
        "    }" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Search error: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell search_cmd
      
      (* Parse result *)
      val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
      
      (* Extract DN and class *)
      val dn_line = 
        case List.find (fn l => String.isPrefix "DN=" l) result_lines of
          SOME l => string_trim (String.extract(l, 3, NONE))
        | NONE => raise ADError "Invalid object format: no DN found"
          
      val class_line = 
        case List.find (fn l => String.isPrefix "ObjectClass=" l) result_lines of
          SOME l => string_trim (String.extract(l, 12, NONE))
        | NONE => "unknown"
          
      (* Determine object type from class *)
      val obj_type = 
        if String.isSubstring "user" class_line then USER
        else if String.isSubstring "group" class_line then GROUP
        else if String.isSubstring "computer" class_line then COMPUTER
        else if String.isSubstring "organizationalUnit" class_line then OU
        else ANY
          
      (* Parse properties *)
      fun parse_prop line =
        case String.tokens (fn c => c = #"=") line of
          name::value::_ => (string_trim name, string_trim value)
        | _ => ("unknown", line)
          
      val props = 
        List.filter (fn l => String.isSubstring "=" l andalso 
                            not (String.isPrefix "DN=" l) andalso
                            not (String.isPrefix "ObjectClass=" l))
                    result_lines
      val properties = map parse_prop props
    in
      if rc <> 0 then
        raise ADError ("Object not found or error: " ^ stderr)
      else
        {
          distinguished_name = dn_line,
          object_class = class_line,
          object_type = obj_type,
          properties = properties
        }
    end
  
  (* Create user *)
  fun create_user (conn, {container, name, sam_account_name, description, password}) =
    let
      val pw = case password of SOME p => p | NONE => ""
      val desc = case description of SOME d => d | NONE => ""
      
      (* Build command *)
      val create_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  $userpass = ConvertTo-SecureString '" ^ pw ^ "' -AsPlainText -Force;" ^
        "  New-ADUser -SamAccountName '" ^ sam_account_name ^ "'" ^
        "    -Name '" ^ name ^ "' -DisplayName '" ^ name ^ "'" ^
        "    -Path '" ^ container ^ "'" ^ 
        (if pw <> "" then " -AccountPassword $userpass -Enabled $true" else "") ^
        (if desc <> "" then " -Description '" ^ desc ^ "'" else "") ^ ";" ^
        "  $user = Get-ADUser -Filter \"sAMAccountName -eq '" ^ sam_account_name ^ "'\" -Properties *;" ^
        "  Write-Output \"DN=$($user.DistinguishedName)\";" ^
        "  Write-Output \"ObjectClass=$($user.ObjectClass)\";" ^
        "  foreach ($prop in $user.PropertyNames) {" ^
        "    foreach ($val in $user.$prop) {" ^
        "      Write-Output \"$prop=$val\";" ^
        "    }" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('User creation failed: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell create_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to create user: " ^ stderr)
      else
        (* Parse result lines to create ad_object *)
        let
          val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
          
          val dn_line = 
            case List.find (fn l => String.isPrefix "DN=" l) result_lines of
              SOME l => string_trim (String.extract(l, 3, NONE))
            | NONE => raise ADError "Invalid object format: no DN found"
              
          val class_line = 
            case List.find (fn l => String.isPrefix "ObjectClass=" l) result_lines of
              SOME l => string_trim (String.extract(l, 12, NONE))
            | NONE => "user"
              
          (* Parse properties *)
          fun parse_prop line =
            case String.tokens (fn c => c = #"=") line of
              name::value::_ => (string_trim name, string_trim value)
            | _ => ("unknown", line)
              
          val props = 
            List.filter (fn l => String.isSubstring "=" l andalso 
                                not (String.isPrefix "DN=" l) andalso
                                not (String.isPrefix "ObjectClass=" l))
                        result_lines
          val properties = map parse_prop props
        in
          {
            distinguished_name = dn_line,
            object_class = class_line,
            object_type = USER,
            properties = properties
          }
        end
    end
  
  (* Create group *)
  fun create_group (conn, {container, name, description, scope, group_type}) =
    let
      val desc = case description of SOME d => d | NONE => ""
      
      (* Build command *)
      val create_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  New-ADGroup -Name '" ^ name ^ "'" ^
        "    -Path '" ^ container ^ "'" ^
        "    -GroupScope '" ^ scope ^ "'" ^
        "    -GroupCategory '" ^ group_type ^ "'" ^
        (if desc <> "" then " -Description '" ^ desc ^ "'" else "") ^ ";" ^
        "  $group = Get-ADGroup -Filter \"Name -eq '" ^ name ^ "'\" -Properties *;" ^
        "  Write-Output \"DN=$($group.DistinguishedName)\";" ^
        "  Write-Output \"ObjectClass=$($group.ObjectClass)\";" ^
        "  foreach ($prop in $group.PropertyNames) {" ^
        "    foreach ($val in $group.$prop) {" ^
        "      Write-Output \"$prop=$val\";" ^
        "    }" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Group creation failed: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell create_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to create group: " ^ stderr)
      else
        (* Parse result lines to create ad_object *)
        let
          val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
          
          val dn_line = 
            case List.find (fn l => String.isPrefix "DN=" l) result_lines of
              SOME l => string_trim (String.extract(l, 3, NONE))
            | NONE => raise ADError "Invalid object format: no DN found"
              
          val class_line = 
            case List.find (fn l => String.isPrefix "ObjectClass=" l) result_lines of
              SOME l => string_trim (String.extract(l, 12, NONE))
            | NONE => "group"
              
          (* Parse properties *)
          fun parse_prop line =
            case String.tokens (fn c => c = #"=") line of
              name::value::_ => (string_trim name, string_trim value)
            | _ => ("unknown", line)
              
          val props = 
            List.filter (fn l => String.isSubstring "=" l andalso 
                                not (String.isPrefix "DN=" l) andalso
                                not (String.isPrefix "ObjectClass=" l))
                        result_lines
          val properties = map parse_prop props
        in
          {
            distinguished_name = dn_line,
            object_class = class_line,
            object_type = GROUP,
            properties = properties
          }
        end
    end
  
  (* Delete AD object *)
  fun delete_object (conn, obj) =
    let
      val dn = #distinguished_name obj
      
      (* Build command *)
      val delete_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  Remove-ADObject -Identity '" ^ dn ^ "' -Confirm:$false;" ^
        "  Write-Output 'Object deleted successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to delete object: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell delete_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to delete object: " ^ stderr)
    end
  
  (* Move AD object *)
  fun move_object (conn, obj, new_container) =
    let
      val dn = #distinguished_name obj
      
      (* Build command *)
      val move_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  Move-ADObject -Identity '" ^ dn ^ "' -TargetPath '" ^ new_container ^ "';" ^
        "  $movedObj = Get-ADObject -Identity '" ^ dn ^ "' -Properties *;" ^
        "  Write-Output \"DN=$($movedObj.DistinguishedName)\";" ^
        "  Write-Output \"ObjectClass=$($movedObj.ObjectClass)\";" ^
        "  foreach ($prop in $movedObj.PropertyNames) {" ^
        "    foreach ($val in $movedObj.$prop) {" ^
        "      Write-Output \"$prop=$val\";" ^
        "    }" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Failed to move object: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell move_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to move object: " ^ stderr)
      else
        (* Parse result lines to create ad_object *)
        let
          val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
          
          val dn_line = 
            case List.find (fn l => String.isPrefix "DN=" l) result_lines of
              SOME l => string_trim (String.extract(l, 3, NONE))
            | NONE => raise ADError "Invalid object format: no DN found"
              
          val class_line = 
            case List.find (fn l => String.isPrefix "ObjectClass=" l) result_lines of
              SOME l => string_trim (String.extract(l, 12, NONE))
            | NONE => #object_class obj
              
          (* Parse properties *)
          fun parse_prop line =
            case String.tokens (fn c => c = #"=") line of
              name::value::_ => (string_trim name, string_trim value)
            | _ => ("unknown", line)
              
          val props = 
            List.filter (fn l => String.isSubstring "=" l andalso 
                                not (String.isPrefix "DN=" l) andalso
                                not (String.isPrefix "ObjectClass=" l))
                        result_lines
          val properties = map parse_prop props
        in
          {
            distinguished_name = dn_line,
            object_class = class_line,
            object_type = #object_type obj,
            properties = properties
          }
        end
    end
  
  (* Add user/computer to group *)
  fun add_to_group (conn, obj, group_obj) =
    let
      val member_dn = #distinguished_name obj
      val group_dn = #distinguished_name group_obj
      
      (* Build command *)
      val add_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  Add-ADGroupMember -Identity '" ^ group_dn ^ "'" ^
        "    -Members '" ^ member_dn ^ "';" ^
        "  Write-Output 'Added to group successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to add to group: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell add_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to add to group: " ^ stderr)
    end
  
  (* Remove from group *)
  fun remove_from_group (conn, obj, group_obj) =
    let
      val member_dn = #distinguished_name obj
      val group_dn = #distinguished_name group_obj
      
      (* Build command *)
      val remove_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  Remove-ADGroupMember -Identity '" ^ group_dn ^ "'" ^
        "    -Members '" ^ member_dn ^ "' -Confirm:$false;" ^
        "  Write-Output 'Removed from group successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to remove from group: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell remove_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to remove from group: " ^ stderr)
    end
  
  (* Check if member of group *)
  fun is_member_of (conn, obj, group_obj) =
    let
      val member_dn = #distinguished_name obj
      val group_dn = #distinguished_name group_obj
      
      (* Build command *)
      val check_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  $group = Get-ADGroup -Identity '" ^ group_dn ^ "' -Properties Members;" ^
        "  $isMember = $group.Members -contains '" ^ member_dn ^ "';" ^
        "  if ($isMember) {" ^
        "    Write-Output 'TRUE'" ^
        "  } else {" ^
        "    Write-Output 'FALSE'" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Membership check failed: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell check_cmd
    in
      if rc <> 0 then
        raise ADError ("Membership check failed: " ^ stderr)
      else
        String.isSubstring "TRUE" stdout
    end
  
  (* Get group members *)
  fun get_group_members (conn, group_obj) =
    let
      val group_dn = #distinguished_name group_obj
      
      (* Build command *)
      val members_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  $group = Get-ADGroup -Identity '" ^ group_dn ^ "' -Properties Members;" ^
        "  foreach ($member in $group.Members) {" ^
        "    $obj = Get-ADObject -Identity $member -Properties *;" ^
        "    Write-Output \"START_MEMBER\";" ^
        "    Write-Output \"DN=$($obj.DistinguishedName)\";" ^
        "    Write-Output \"ObjectClass=$($obj.ObjectClass)\";" ^
        "    foreach ($prop in $obj.PropertyNames) {" ^
        "      foreach ($val in $obj.$prop) {" ^
        "        Write-Output \"$prop=$val\";" ^
        "      }" ^
        "    }" ^
        "    Write-Output \"END_MEMBER\";" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Failed to get group members: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell members_cmd
      
      (* Parse results *)
      val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
      
      (* Group lines by member *)
      fun parse_lines [] current_obj objects = 
            if current_obj = [] then objects
            else List.rev current_obj :: objects
        | parse_lines ("END_MEMBER"::rest) current_obj objects =
            parse_lines rest [] (List.rev current_obj :: objects)
        | parse_lines ("START_MEMBER"::rest) current_obj objects =
            if current_obj = [] then
              parse_lines rest [] objects
            else
              parse_lines rest [] (List.rev current_obj :: objects)
        | parse_lines (line::rest) current_obj objects =
            parse_lines rest (line :: current_obj) objects
            
      val grouped_lines = List.rev (parse_lines result_lines [] [])
      
      (* Parse each object *)
      fun parse_object lines =
        let
          (* Extract DN and class *)
          val dn_line = 
            case List.find (fn l => String.isPrefix "DN=" l) lines of
              SOME l => string_trim (String.extract(l, 3, NONE))
            | NONE => raise ADError "Invalid object format: no DN found"
              
          val class_line = 
            case List.find (fn l => String.isPrefix "ObjectClass=" l) lines of
              SOME l => string_trim (String.extract(l, 12, NONE))
            | NONE => "unknown"
              
          (* Determine object type from class *)
          val obj_type = 
            if String.isSubstring "user" class_line then USER
            else if String.isSubstring "group" class_line then GROUP
            else if String.isSubstring "computer" class_line then COMPUTER
            else if String.isSubstring "organizationalUnit" class_line then OU
            else ANY
              
          (* Parse properties *)
          fun parse_prop line =
            case String.tokens (fn c => c = #"=") line of
              name::value::_ => (string_trim name, string_trim value)
            | _ => ("unknown", line)
              
          val props = 
            List.filter (fn l => String.isSubstring "=" l andalso 
                                not (String.isPrefix "DN=" l) andalso
                                not (String.isPrefix "ObjectClass=" l))
                        lines
          val properties = map parse_prop props
        in
          {
            distinguished_name = dn_line,
            object_class = class_line,
            object_type = obj_type,
            properties = properties
          }
        end
    in
      if rc <> 0 then
        raise ADError ("Failed to get group members: " ^ stderr)
      else
        map parse_object grouped_lines
    end
  
  (* Property management *)
  fun get_property obj property_name =
    let
      val properties = #properties obj
    in
      case List.find (fn (name, _) => name = property_name) properties of
        SOME (_, value) => SOME value
      | NONE => NONE
    end
  
  fun set_property (conn, obj, property_name, property_value) =
    let
      val dn = #distinguished_name obj
      
      (* Build command *)
      val set_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  Set-ADObject -Identity '" ^ dn ^ "'" ^
        "    -Replace @{'" ^ property_name ^ "'='" ^ property_value ^ "'};" ^
        "  Write-Output 'Property updated successfully'" ^
        "} catch {" ^
        "  Write-Error ('Failed to set property: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell set_cmd
    in
      if rc <> 0 then
        raise ADError ("Failed to set property: " ^ stderr)
    end
  
  (* Get all properties *)
  fun get_properties obj = #properties obj
  
  (* Find objects *)
  fun find_objects (conn, base_dn, filter) =
    let
      (* Build search command *)
      val search_cmd = 
        "try {" ^
        "  $domain = '" ^ (#domain (!current_connection)) ^ "';" ^
        "  $username = '" ^ (#username (!current_connection)) ^ "';" ^
        "  $root = New-Object System.DirectoryServices.DirectoryEntry(" ^ 
                "'LDAP://" ^ (#server (!current_connection)) ^ ":" ^ 
                Int.toString(#port (!current_connection)) ^ "/" ^ base_dn ^ "');" ^
        "  $searcher = New-Object System.DirectoryServices.DirectorySearcher($root);" ^
        "  $searcher.Filter = '" ^ filter ^ "';" ^
        "  $searcher.SearchScope = 'Subtree';" ^
        "  $results = $searcher.FindAll();" ^
        "  foreach ($result in $results) {" ^
        "    Write-Output \"START_OBJECT\";" ^
        "    $dn = $result.Properties['distinguishedname'][0];" ^
        "    Write-Output \"DN=$dn\";" ^
        "    $objClass = 'unknown';" ^
        "    if ($result.Properties['objectclass']) {" ^
        "      $objClass = $result.Properties['objectclass'][0];" ^
        "    }" ^
        "    Write-Output \"ObjectClass=$objClass\";" ^
        "    foreach ($prop in $result.Properties.Keys) {" ^
        "      foreach ($val in $result.Properties[$prop]) {" ^
        "        Write-Output \"$prop=$val\";" ^
        "      }" ^
        "    }" ^
        "    Write-Output \"END_OBJECT\";" ^
        "  }" ^
        "} catch {" ^
        "  Write-Error ('Search error: ' + $_.Exception.Message)" ^
        "  exit 1" ^
        "}"
        
      val (rc, stdout, stderr) = execute_powershell search_cmd
      
      (* Parse results *)
      val result_lines = String.tokens (fn c => c = #"\n" orelse c = #"\r") stdout
      
      (* Group lines by object *)
      fun parse_lines [] current_obj objects = 
            if current_obj = [] then objects
            else List.rev current_obj :: objects
        | parse_lines ("END_OBJECT"::rest) current_obj objects =
            parse_lines rest [] (List.rev current_obj :: objects)
        | parse_lines ("START_OBJECT"::rest) current_obj objects =
            if current_obj = [] then
              parse_lines rest [] objects
            else
              parse_lines rest [] (List.rev current_obj :: objects)
        | parse_lines (line::rest) current_obj objects =
            parse_lines rest (line :: current_obj) objects
            
      val grouped_lines = List.rev (parse_lines result_lines [] [])
      
      (* Parse each object *)
      fun parse_object lines =
        let
          (* Extract DN and class *)
          val dn_line = 
            case List.find (fn l => String.isPrefix "DN=" l) lines of
              SOME l => string_trim (String.extract(l, 3, NONE))
            | NONE => raise ADError "Invalid object format: no DN found"
              
          val class_line = 
            case List.find (fn l => String.isPrefix "ObjectClass=" l) lines of
              SOME l => string_trim (String.extract(l, 12, NONE))
            | NONE => "unknown"
              
          (* Determine object type from class *)
          val obj_type = 
            if String.isSubstring "user" class_line then USER
            else if String.isSubstring "group" class_line then GROUP
            else if String.isSubstring "computer" class_line then COMPUTER
            else if String.isSubstring "organizationalUnit" class_line then OU
            else ANY
              
          (* Parse properties *)
          fun parse_prop line =
            case String.tokens (fn c => c = #"=") line of
              name::value::_ => (string_trim name, string_trim value)
            | _ => ("unknown", line)
              
          val props = 
            List.filter (fn l => String.isSubstring "=" l andalso 
                                not (String.isPrefix "DN=" l) andalso
                                not (String.isPrefix "ObjectClass=" l))
                        lines
          val properties = map parse_prop props
        in
          {
            distinguished_name = dn_line,
            object_class = class_line,
            object_type = obj_type,
            properties = properties
          }
        end
    in
      if rc <> 0 then
        raise ADError ("Search failed: " ^ stderr)
      else
        map parse_object grouped_lines
    end
  
  (* Find users *)
  fun find_users (conn, base_dn, name_filter) =
    let
      val filter = 
        if name_filter = "" then
          "(objectClass=user)"
        else
          "(&(objectClass=user)(cn=*" ^ name_filter ^ "*))"
    in
      find_objects (conn, base_dn, filter)
    end
  
  (* Find groups *)
  fun find_groups (conn, base_dn, name_filter) =
    let
      val filter = 
        if name_filter = "" then
          "(objectClass=group)"
        else
          "(&(objectClass=group)(cn=*" ^ name_filter ^ "*))"
    in
      find_objects (conn, base_dn, filter)
    end
end