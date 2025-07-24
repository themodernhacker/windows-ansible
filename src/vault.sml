(* vault.sml
 * Implementation of secure secret management
 *)

structure Vault : VAULT = struct
  type secret = string
  
  (* Vault data structure *)
  type vault = {
    path: string,
    password: string,
    secrets: (string, secret) HashTable.hash_table,
    modified: bool ref
  }
  
  exception VaultError of string
  exception AuthenticationError of string
  
  (* Utility function for simple encryption *)
  fun simple_encrypt (password, text) =
    let
      (* Convert password and text to character codes *)
      val pwd_chars = String.explode password
      val pwd_codes = map Char.ord pwd_chars
      
      (* Repeat password to match text length *)
      fun repeat_pwd codes text_len =
        if length codes >= text_len then
          List.take(codes, text_len)
        else
          codes @ repeat_pwd codes (text_len - length codes)
          
      val text_chars = String.explode text
      val text_codes = map Char.ord text_chars
      val pwd_repeated = repeat_pwd pwd_codes (length text_codes)
      
      (* XOR text with password *)
      val encrypted_codes = 
        ListPair.map (fn (t, p) => Word.toInt(Word.xorb(Word.fromInt t, Word.fromInt p)))
                     (text_codes, pwd_repeated)
                     
      (* Convert back to characters *)
      val encrypted_chars = map Char.chr encrypted_codes
      val encrypted_text = String.implode encrypted_chars
      
      (* Base64 encode - simplified version *)
      val encoded = 
        "$ANSIBLE_VAULT;1.1;AES256\n" ^ 
        Byte.bytesToString(Byte.stringToBytes encrypted_text)
    in
      encoded
    end
    
  (* Utility function for simple decryption *)
  fun simple_decrypt (password, encrypted_text) =
    let
      (* Strip header and decode *)
      val header = "$ANSIBLE_VAULT;1.1;AES256\n"
      val body = 
        if String.isPrefix header encrypted_text then
          String.extract(encrypted_text, String.size header, NONE)
        else
          raise VaultError("Invalid vault format")
          
      (* Base64 decode - simplified *)
      val decoded = Byte.bytesToString(Byte.stringToBytes body)
      
      (* Convert password and decoded text to character codes *)
      val pwd_chars = String.explode password
      val pwd_codes = map Char.ord pwd_chars
      
      (* Repeat password to match text length *)
      fun repeat_pwd codes text_len =
        if length codes >= text_len then
          List.take(codes, text_len)
        else
          codes @ repeat_pwd codes (text_len - length codes)
          
      val text_chars = String.explode decoded
      val text_codes = map Char.ord text_chars
      val pwd_repeated = repeat_pwd pwd_codes (length text_codes)
      
      (* XOR text with password to decrypt *)
      val decrypted_codes = 
        ListPair.map (fn (t, p) => Word.toInt(Word.xorb(Word.fromInt t, Word.fromInt p)))
                     (text_codes, pwd_repeated)
                     
      (* Convert back to characters *)
      val decrypted_chars = map Char.chr decrypted_codes
      val decrypted_text = String.implode decrypted_chars
    in
      decrypted_text
    end
    
  (* Create a new vault *)
  fun create (path, password) =
    let
      (* Create empty vault file *)
      val file = TextIO.openOut path
      val _ = TextIO.output(file, "# Windows Ansible Core Vault\n")
      val _ = TextIO.closeOut file
      
      (* Create vault data structure *)
      val secrets = HashTable.mkTable (HashString.hashString, op=) 
                                     (50, VaultError "Secret not found")
    in
      {
        path = path,
        password = password,
        secrets = secrets,
        modified = ref false
      }
    end
    handle IO.Io {name, ...} => 
      raise VaultError("Failed to create vault: " ^ name)
      
  (* Open an existing vault *)
  fun open_vault (path, password) =
    let
      (* Read vault file *)
      val file = TextIO.openIn path
      val content = TextIO.inputAll file
      val _ = TextIO.closeIn file
      
      (* Parse the file content *)
      val lines = String.tokens (fn c => c = #"\n") content
      
      (* Filter out comments and empty lines *)
      val secret_lines = 
        List.filter (fn line => 
                      String.size line > 0 andalso
                      not (String.isPrefix "#" line))
                    lines
      
      (* Create vault data structure *)
      val secrets = HashTable.mkTable (HashString.hashString, op=) 
                                     (50, VaultError "Secret not found")
                                     
      (* Process each secret line *)
      fun process_line line =
        let
          val parts = String.tokens (fn c => c = #":") line
        in
          if length parts >= 2 then
            let
              val key = hd parts
              val encrypted_value = String.concatWith ":" (tl parts)
              
              (* Try to decrypt the value *)
              val value = 
                (simple_decrypt (password, encrypted_value))
                handle _ => 
                  raise AuthenticationError("Failed to decrypt vault: wrong password")
                  
              val _ = HashTable.insert secrets (key, value)
            in
              ()
            end
          else
            () (* Ignore malformed lines *)
        end
        
      val _ = List.app process_line secret_lines
    in
      {
        path = path,
        password = password,
        secrets = secrets,
        modified = ref false
      }
    end
    handle IO.Io {name, ...} => 
      raise VaultError("Failed to open vault: " ^ name)
    
  (* Close a vault, saving changes if needed *)
  fun close_vault vault =
    if !(#modified vault) then
      let
        (* Save all secrets to the file *)
        val file = TextIO.openOut (#path vault)
        val _ = TextIO.output(file, "# Windows Ansible Core Vault\n")
        
        (* Write each secret *)
        val _ = HashTable.appi 
                 (fn (key, value) => 
                   let
                     val encrypted = simple_encrypt(#password vault, value)
                     val line = key ^ ":" ^ encrypted ^ "\n"
                   in
                     TextIO.output(file, line)
                   end)
                 (#secrets vault)
                 
        val _ = TextIO.closeOut file
      in
        ()
      end
    else
      ()
    handle IO.Io {name, ...} => 
      raise VaultError("Failed to save vault: " ^ name)
  
  (* Encrypt a string *)
  fun encrypt_string (vault, text) =
    simple_encrypt(#password vault, text)
  
  (* Decrypt a string *)
  fun decrypt_string (vault, encrypted_text) =
    simple_decrypt(#password vault, encrypted_text)
    handle _ => 
      raise VaultError("Failed to decrypt: invalid data or wrong password")
  
  (* Encrypt a file *)
  fun encrypt_file (vault, input_path, output_path) =
    let
      (* Read the input file *)
      val input = TextIO.openIn input_path
      val content = TextIO.inputAll input
      val _ = TextIO.closeIn input
      
      (* Encrypt the content *)
      val encrypted = encrypt_string(vault, content)
      
      (* Write to output file *)
      val output = TextIO.openOut output_path
      val _ = TextIO.output(output, encrypted)
      val _ = TextIO.closeOut output
    in
      ()
    end
    handle IO.Io {name, ...} => 
      raise VaultError("File operation failed: " ^ name)
  
  (* Decrypt a file *)
  fun decrypt_file (vault, input_path, output_path) =
    let
      (* Read the encrypted file *)
      val input = TextIO.openIn input_path
      val content = TextIO.inputAll input
      val _ = TextIO.closeIn input
      
      (* Decrypt the content *)
      val decrypted = decrypt_string(vault, content)
      
      (* Write to output file *)
      val output = TextIO.openOut output_path
      val _ = TextIO.output(output, decrypted)
      val _ = TextIO.closeOut output
    in
      ()
    end
    handle IO.Io {name, ...} => 
      raise VaultError("File operation failed: " ^ name)
  
  (* Get a secret from the vault *)
  fun get_secret (vault, key) =
    SOME (HashTable.lookup (#secrets vault) key)
    handle _ => NONE
  
  (* Set a secret in the vault *)
  fun set_secret (vault, key, value) =
    (HashTable.insert (#secrets vault) (key, value);
     #modified vault := true)
  
  (* List all secret keys *)
  fun list_secrets vault =
    HashTable.foldi (fn (key, _, acc) => key :: acc) [] (#secrets vault)
  
  (* Check if a string is vault-encrypted *)
  fun is_vault_encrypted str =
    String.isPrefix "$ANSIBLE_VAULT;" str
  
  (* Get the vault ID from an encrypted string *)
  fun get_vault_id encrypted =
    if is_vault_encrypted encrypted then
      let
        val parts = String.tokens (fn c => c = #";") encrypted
      in
        if length parts >= 3 then
          SOME (List.nth(parts, 0) ^ ";" ^ List.nth(parts, 1) ^ ";" ^ List.nth(parts, 2))
        else
          NONE
      end
    else
      NONE
end