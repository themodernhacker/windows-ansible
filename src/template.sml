(* template.sml
 * Implementation of Ansible-like templating system
 *)

structure Template : TEMPLATE = struct
  (* Variable resolution *)
  type vars = (string * string) list
  
  (* Exceptions *)
  exception TemplateError of string
  exception UndefinedVariable of string
  
  (* Filter registry *)
  val filter_registry : (string, string -> string) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (20, Fail "Filter not found")
    
  (* Variable pattern - simple version, would be more sophisticated in real implementation *)
  val var_pattern = "{{" (* opening marker *)
  val var_pattern_end = "}}" (* closing marker *)
  
  (* Check if a string has a variable reference *)
  fun has_variable (template, var_name) =
    String.isSubstring (var_pattern ^ " " ^ var_name ^ " " ^ var_pattern_end) template orelse
    String.isSubstring (var_pattern ^ var_name ^ var_pattern_end) template
  
  (* Extract all variables from a template *)
  fun extract_variables template =
    let
      (* Find all variable patterns in the template *)
      fun find_vars (template, start_pos, acc) =
        if start_pos >= String.size template then
          acc
        else
          let
            val var_start = 
              String.findSubstring var_pattern 
                                  (String.substring(template, 
                                                  start_pos,
                                                  String.size template - start_pos))
          in
            case var_start of
              SOME pos =>
                let
                  val real_start = start_pos + pos + String.size var_pattern
                  val var_end = 
                    String.findSubstring var_pattern_end
                                        (String.substring(template, 
                                                        real_start,
                                                        String.size template - real_start))
                in
                  case var_end of
                    SOME end_pos =>
                      let
                        val var_name = 
                          String.substring(template, 
                                          real_start, 
                                          end_pos)
                        val var_name = String.trim var_name
                      in
                        find_vars(template, 
                                 real_start + end_pos + String.size var_pattern_end,
                                 var_name :: acc)
                      end
                  | NONE => acc (* Malformed template - missing closing marker *)
                end
            | NONE => acc (* No more variables *)
          end
    in
      find_vars(template, 0, [])
    end
  
  (* Process a template string *)
  fun process_string (template, variables) =
    let
      (* Process a single variable *)
      fun process_var var_expr =
        let
          (* Check if there's a filter applied *)
          val filter_parts = String.tokens (fn c => c = #"|") var_expr
          val var_name = String.trim (List.nth(filter_parts, 0))
          
          (* Look up the variable value *)
          val var_value = 
            case List.find (fn (k, _) => k = var_name) variables of
              SOME (_, v) => v
            | NONE => raise UndefinedVariable("Variable not defined: " ^ var_name)
            
          (* Apply filter if present *)
          val result = 
            if length filter_parts > 1 then
              let
                val filter_name = String.trim (List.nth(filter_parts, 1))
                val filter_fn = 
                  HashTable.lookup filter_registry filter_name
                  handle _ => raise TemplateError("Unknown filter: " ^ filter_name)
              in
                filter_fn var_value
              end
            else
              var_value
        in
          result
        end
      
      (* Find and replace all variables in the template *)
      fun replace_vars (template, start_pos, acc) =
        if start_pos >= String.size template then
          acc
        else
          let
            val var_start = 
              String.findSubstring var_pattern 
                                  (String.substring(template, 
                                                  start_pos,
                                                  String.size template - start_pos))
          in
            case var_start of
              SOME pos =>
                let
                  val prefix = 
                    acc ^ String.substring(template, start_pos, pos)
                  val real_start = start_pos + pos + String.size var_pattern
                  val var_end = 
                    String.findSubstring var_pattern_end
                                        (String.substring(template, 
                                                        real_start,
                                                        String.size template - real_start))
                in
                  case var_end of
                    SOME end_pos =>
                      let
                        val var_expr = 
                          String.substring(template, 
                                          real_start, 
                                          end_pos)
                        val var_value = process_var (String.trim var_expr)
                      in
                        replace_vars(template, 
                                    real_start + end_pos + String.size var_pattern_end,
                                    prefix ^ var_value)
                      end
                  | NONE => 
                      (* Malformed template - missing closing marker *)
                      replace_vars(template, 
                                  start_pos + pos + String.size var_pattern,
                                  prefix ^ var_pattern)
                end
            | NONE => 
                (* No more variables, append the rest of the template *)
                acc ^ String.substring(template, 
                                      start_pos, 
                                      String.size template - start_pos)
          end
    in
      replace_vars(template, 0, "")
    end
    handle UndefinedVariable msg => raise TemplateError(msg)
  
  (* Process a template file *)
  fun process_file (input_file, variables, output_file) =
    let
      val input = TextIO.openIn input_file
      val template = TextIO.inputAll input
      val _ = TextIO.closeIn input
      
      val processed = process_string(template, variables)
      
      val output = TextIO.openOut output_file
      val _ = TextIO.output(output, processed)
      val _ = TextIO.closeOut output
    in
      ()
    end
    handle IO.Io {name, ...} => 
      raise TemplateError("File error: " ^ name)
  
  (* Register a filter *)
  fun register_filter (name, fn_) =
    HashTable.insert filter_registry (name, fn_)
  
  (* Get a filter *)
  fun get_filter name =
    SOME (HashTable.lookup filter_registry name)
    handle _ => NONE
  
  (* List all filters *)
  fun list_filters () =
    HashTable.foldi (fn (name, _, acc) => name :: acc) [] filter_registry
  
  (* Register default filters *)
  fun default_filters () = [
    ("upper", String.map Char.toUpper),
    ("lower", String.map Char.toLower),
    ("capitalize", fn s => 
      if String.size s > 0 then
        String.str(Char.toUpper(String.sub(s, 0))) ^ 
        String.substring(s, 1, String.size s - 1)
      else
        s),
    ("trim", String.trim),
    ("basename", OS.Path.file),
    ("dirname", OS.Path.dir),
    ("default", fn s => if s = "" then "default" else s)
  ]
  
  (* Initialize default filters *)
  val _ = 
    List.app (fn (name, fn_) => register_filter(name, fn_)) (default_filters())
end