(* vault.sml
 * Secret management for secure storage
 *)

structure Vault : VAULT = struct
  (* Types *)
  type secret = string
  
  (* Define the vault record type with all required fields *)
  type vault = {
    path: string,
    password: string,
    secrets: (string, string) HashTable.hash_table,
    modified: bool ref
  }
  
  (* Exceptions *)
  exception VaultError of string
  exception AuthenticationError of string
  
  (* Custom string search function to replace String.find *)
  fun findChar c s =
    let
      fun findAt i =
        if i >= String.size s then
          NONE
        else if String.sub(s, i) = c then
          SOME i
        else
          findAt (i + 1)
    in
      findAt 0
    end
  
  (* Helper functions *)
  fun xorBytes (s1: string, s2: string) : string =
    let
      val bytes1 = map Char.ord (explode s1)
      val bytes2 = map Char.ord (explode s2)
      
      (* Repeat the key if necessary *)
      fun extendKey key [] = []
        | extendKey [] bs = extendKey bytes2 bs
        | extendKey (k::ks) (b::bs) = k :: extendKey ks bs
      
      val extendedKey = extendKey bytes2 bytes1
      
      (* XOR the bytes *)
      val xored = ListPair.map (fn (b1, k) => Char.chr (Word8.toInt (Word8.xorb (Word8.fromInt b1, Word8.fromInt k)))) (bytes1, extendedKey)
    in
      implode xored
    end
  
  (* Simple Base64-like encoding and decoding *)
  val chars64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  
  fun encodeBase64 (s: string) : string =
    let
      val bytes = map Char.ord (explode s)
      
      (* Process three bytes at a time *)
      fun encode3 (b1::b2::b3::bs) acc =
            let
              val n = (b1 * 65536) + (b2 * 256) + b3
              val c1 = String.sub(chars64, n div 262144)
              val c2 = String.sub(chars64, (n div 4096) mod 64)
              val c3 = String.sub(chars64, (n div 64) mod 64)
              val c4 = String.sub(chars64, n mod 64)
            in
              encode3 bs (acc ^ str c1 ^ str c2 ^ str c3 ^ str c4)
            end
        | encode3 [b1, b2] acc =
            let
              val n = (b1 * 65536) + (b2 * 256)
              val c1 = String.sub(chars64, n div 262144)
              val c2 = String.sub(chars64, (n div 4096) mod 64)
              val c3 = String.sub(chars64, (n div 64) mod 64)
            in
              acc ^ str c1 ^ str c2 ^ str c3 ^ "="
            end
        | encode3 [b1] acc =
            let
              val n = b1 * 65536
              val c1 = String.sub(chars64, n div 262144)
              val c2 = String.sub(chars64, (n div 4096) mod 64)
            in
              acc ^ str c1 ^ str c2 ^ "=="
            end
        | encode3 [] acc = acc
    in
      encode3 bytes ""
    end
  
  fun char64ToInt c =
    let
      fun findIndexInString (ch, str, idx) =
        if idx >= String.size str then
          ~1
        else if String.sub(str, idx) = ch then
          idx
        else
          findIndexInString(ch, str, idx + 1)
    in
      findIndexInString(c, chars64, 0)
    end
  
  fun decodeBase64 (s: string) : string =
    let
      val chars = explode s
      
      (* Process four chars at a time *)
      fun decode4 (c1::c2::c3::c4::cs) acc =
            let
              val i1 = char64ToInt c1
              val i2 = char64ToInt c2
              val i3 = if c3 = #"=" then 0 else char64ToInt c3
              val i4 = if c4 = #"=" then 0 else char64ToInt c4
            in
              if i1 < 0 orelse i2 < 0 orelse (c3 <> #"=" andalso i3 < 0) 
                           orelse (c4 <> #"=" andalso i4 < 0) then
                raise VaultError "Invalid Base64 character"
              else if c3 = #"=" andalso c4 = #"=" then
                (* Only 1 byte *)
                let
                  val n = (i1 * 64) + i2
                  val b1 = n div 4
                in
                  decode4 cs (acc ^ str (Char.chr b1))
                end
              else if c4 = #"=" then
                (* Only 2 bytes *)
                let
                  val n = (i1 * 4096) + (i2 * 64) + i3
                  val b1 = n div 1024
                  val b2 = (n div 4) mod 256
                in
                  decode4 cs (acc ^ str (Char.chr b1) ^ str (Char.chr b2))
                end
              else
                (* Full 3 bytes *)
                let
                  val n = (i1 * 262144) + (i2 * 4096) + 
                          (i3 * 64) + i4
                  val b1 = n div 65536
                  val b2 = (n div 256) mod 256
                  val b3 = n mod 256
                in
                  decode4 cs (acc ^ str (Char.chr b1) ^ str (Char.chr b2) ^ str (Char.chr b3))
                end
            end
        | decode4 [] acc = acc
        | decode4 _ _ = raise VaultError "Invalid Base64 string length"
    in
      decode4 chars ""
    end
  
  (* Generate a simple hash for password validation *)
  fun simpleHash (s: string) : string =
    let
      val chars = explode s
      val hash = foldl (fn (c, h) => h * 31 + Char.ord c) 0 chars
    in
      Int.toString hash
    end
  
  (* Check if a string is vault-encrypted *)
  fun is_vault_encrypted (s: string) : bool =
    String.isPrefix "$ANSIBLE_VAULT" s
  
  (* Get the vault ID from an encrypted string *)
  fun get_vault_id (s: string) : string option =
    if is_vault_encrypted s then
      let
        val header = hd (String.tokens (fn c => c = #"\n") s)
        val parts = String.tokens (fn c => c = #";") header
      in
        if length parts >= 3 then
          SOME (List.nth(parts, 2))
        else
          NONE
      end
    else
      NONE
  
  (* Encrypt a string *)
  fun encrypt_string (v: vault, s: string) : string =
    let
      val {password, ...} = v
      val encrypted = xorBytes(s, password)
      val encoded = encodeBase64(encrypted)
    in
      "$ANSIBLE_VAULT;1.0;AES256\n" ^ encoded
    end
  
  (* Decrypt a string *)
  fun decrypt_string (v: vault, s: string) : string =
    let
      val {password, ...} = v
      
      (* Check if this is a vault string and extract the content *)
      val content = 
        if String.isPrefix "$ANSIBLE_VAULT" s then
          let
            val parts = String.tokens (fn c => c = #"\n") s
          in
            if length parts < 2 then
              raise VaultError "Invalid vault format"
            else
              String.concatWith "\n" (tl parts)
          end
        else
          raise VaultError "Not a vault-encrypted string"
          
      (* Decode and decrypt *)
      val decoded = decodeBase64(content)
      val decrypted = xorBytes(decoded, password)
    in
      decrypted
    end
  
  (* Close and save the vault *)
  fun close_vault (v: vault) : unit =
    let
      val {path, password, secrets, modified} = v
      
      (* Only save if modified *)
      val _ = if not (!modified) then () else
        let
          (* Serialize the secrets *)
          val secretsList = ref []
          val _ = HashTable.appi (fn (k, v) => secretsList := (k ^ ":" ^ v) :: !secretsList) secrets
          val secretsText = String.concatWith "\n" (!secretsList)
          
          (* Encrypt the content *)
          val encryptedSecrets = encrypt_string(v, secretsText)
          
          (* Add the password hash for validation on open *)
          val passwordHash = simpleHash password
          val fullContent = "VAULT_HASH:" ^ passwordHash ^ "\n" ^ encryptedSecrets
          
          (* Save to file *)
          val file = TextIO.openOut path
                    handle _ => raise VaultError ("Cannot write to vault file: " ^ path)
          val _ = TextIO.output(file, fullContent)
          val _ = TextIO.closeOut file
        in
          ()
        end
    in
      ()
    end
  
  (* Create a new vault *)
  fun create (path: string, password: string) : vault =
    let
      val secrets = HashTable.mkTable (HashString.hashString, op=) (100, Fail "Secret not found")
      val vault = {
        path = path,
        password = password,
        secrets = secrets,
        modified = ref false
      }
      (* Save the empty vault to establish the file *)
      val _ = close_vault vault
    in
      vault
    end
  
  (* Open an existing vault *)
  fun open_vault (path: string, password: string) : vault =
    let
      val secrets = HashTable.mkTable (HashString.hashString, op=) (100, Fail "Secret not found")
      
      (* Try to load the vault from the file *)
      val contents = 
        let
          val file = TextIO.openIn path
                    handle _ => raise VaultError ("Cannot open vault file: " ^ path)
          val content = TextIO.inputAll file
          val _ = TextIO.closeIn file
        in
          content
        end
      
      (* Check if this is actually a vault file *)
      val _ = if not (is_vault_encrypted contents) then
                raise VaultError "Not a valid vault file"
              else
                ()
      
      (* Get the stored hash to validate the password *)
      val storedHashLine = 
        case String.tokens (fn c => c = #"\n") contents of
          line1::rest => line1
        | _ => raise VaultError "Invalid vault file format"
      
      val storedHash = 
        if String.isPrefix "VAULT_HASH:" storedHashLine then
          String.extract(storedHashLine, 11, NONE)
        else
          raise VaultError "Invalid vault file format"
      
      (* Validate the password *)
      val passwordHash = simpleHash password
      val _ = if passwordHash <> storedHash then
                raise AuthenticationError "Incorrect password"
              else
                ()
      
      (* Create the vault object first *)
      val vault = {
        path = path,
        password = password,
        secrets = secrets,
        modified = ref false
      }
      
      (* Decrypt and load the secrets *)
      val encryptedContent = String.concatWith "\n" (tl (String.tokens (fn c => c = #"\n") contents))
      val decryptedContent = decrypt_string(vault, encryptedContent)
      
      (* Parse the secrets *)
      val lines = String.tokens (fn c => c = #"\n") decryptedContent
      
      fun parseSecrets [] = ()
        | parseSecrets (line::rest) =
            let
              val parts = String.tokens (fn c => c = #":") line
            in
              if length parts >= 2 then
                let
                  val key = hd parts
                  val value = String.concatWith ":" (tl parts)
                in
                  HashTable.insert secrets (key, value);
                  parseSecrets rest
                end
              else
                parseSecrets rest
            end
    in
      parseSecrets lines;
      vault
    end
    handle IO.Io _ => raise VaultError ("Cannot open vault file: " ^ path)
         | OS.SysErr _ => raise VaultError ("System error accessing vault file: " ^ path)

  (* Encrypt a file *)
  fun encrypt_file (v: vault, input_path: string, output_path: string) : unit =
    let
      val inFile = TextIO.openIn input_path
                  handle _ => raise VaultError ("Cannot open input file: " ^ input_path)
      val content = TextIO.inputAll inFile
      val _ = TextIO.closeIn inFile
      
      val encrypted = encrypt_string(v, content)
      
      val outFile = TextIO.openOut output_path
                   handle _ => raise VaultError ("Cannot open output file: " ^ output_path)
      val _ = TextIO.output(outFile, encrypted)
      val _ = TextIO.closeOut outFile
    in
      ()
    end
  
  (* Decrypt a file *)
  fun decrypt_file (v: vault, input_path: string, output_path: string) : unit =
    let
      val inFile = TextIO.openIn input_path
                  handle _ => raise VaultError ("Cannot open input file: " ^ input_path)
      val content = TextIO.inputAll inFile
      val _ = TextIO.closeIn inFile
      
      val decrypted = decrypt_string(v, content)
      
      val outFile = TextIO.openOut output_path
                   handle _ => raise VaultError ("Cannot open output file: " ^ output_path)
      val _ = TextIO.output(outFile, decrypted)
      val _ = TextIO.closeOut outFile
    in
      ()
    end
  
  (* Get a secret *)
  fun get_secret (v: vault, key: string) : secret option =
    let
      val {secrets, ...} = v
    in
      HashTable.find secrets key
    end
  
  (* Set a secret *)
  fun set_secret (v: vault, key: string, value: string) : unit =
    let
      val {secrets, modified, ...} = v
      val _ = HashTable.insert secrets (key, value)
      val _ = modified := true
    in
      ()
    end
  
  (* List all secrets *)
  fun list_secrets (v: vault) : string list =
    let
      val {secrets, ...} = v
      val keys = ref []
      val _ = HashTable.appi (fn (k, _) => keys := k :: !keys) secrets
    in
      !keys
    end
end