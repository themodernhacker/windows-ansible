(* template.sml
 * Template processing for Windows Ansible Core
 *)

structure Template : TEMPLATE = struct
  (* Type definitions *)
  type vars = (string * string) list
  
  (* Exceptions *)
  exception TemplateError of string
  exception UndefinedVariable of string
  
  (* Custom implementations of missing String functions *)
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
    
  (* Custom implementation of String.findSubstring *)
  fun string_findSubstring needle haystack =
    let
      val needle_len = String.size needle
      val haystack_len = String.size haystack
      
      fun find start =
        if start > haystack_len - needle_len then
          NONE
        else if String.substring(haystack, start, needle_len) = needle then
          SOME start
        else
          find (start + 1)
    in
      if needle_len = 0 orelse haystack_len = 0 then NONE
      else find 0
    end
  
  (* Storage for registered filters *)
  val filters = ref ([] : (string * (string -> string)) list)
  
  (* Register a filter function *)
  fun register_filter (name, func) =
    filters := (name, func) :: !filters
    
  (* Get a filter by name *)
  fun get_filter name =
    case List.find (fn (n, _) => n = name) (!filters) of
      SOME (_, f) => SOME f
    | NONE => NONE
    
  (* List all available filters *)
  fun list_filters () =
    map (fn (name, _) => name) (!filters)
    
  (* Define default filters and return them *)
  fun default_filters () = 
    let
      (* Define default filter functions *)
      fun uppercase s = String.map Char.toUpper s
      fun lowercase s = String.map Char.toLower s
      fun capitalize s =
        case explode s of
          [] => s
        | c::cs => implode (Char.toUpper c :: cs)
      
      (* Define the list of filters *)
      val default_filter_list = [
        ("upper", uppercase),
        ("lower", lowercase),
        ("capitalize", capitalize)
      ]
      
      (* Register the default filters *)
      val _ = app register_filter default_filter_list
    in
      default_filter_list
    end
  
  (* Extract all variables from a template *)
  fun extract_variables template =
    let
      fun extract acc pos =
        if pos >= String.size template then
          acc
        else
          case string_findSubstring "{{" template of
            NONE => acc
          | SOME start_idx =>
              if start_idx < pos then 
                extract acc (pos + 1)
              else
                case string_findSubstring "}}" template of
                  NONE => acc
                | SOME end_idx => 
                    if end_idx <= start_idx + 2 then
                      extract acc (end_idx + 2)
                    else
                      let
                        val var_expr = String.substring(template, start_idx + 2, end_idx - start_idx - 2)
                        val var_name = 
                          case String.tokens (fn c => c = #"|") var_expr of
                            [] => ""
                          | name::_ => string_trim name
                        val new_pos = end_idx + 2
                      in
                        if var_name = "" orelse List.exists (fn v => v = var_name) acc then
                          extract acc new_pos
                        else
                          extract (var_name :: acc) new_pos
                      end
    in
      extract [] 0
    end

  (* Check if a template contains a specific variable *)
  fun has_variable (template, var_name) = 
    let
      val var_pattern = "{{" ^ var_name ^ "}}"
      val stripped_var = string_trim var_name
      val template_vars = extract_variables template
    in
      List.exists (fn v => v = stripped_var) template_vars
    end
  
  (* Helper function to get a variable value *)
  fun lookup_var vars name =
    case List.find (fn (k, _) => k = name) vars of
      SOME (_, value) => SOME value
    | NONE => NONE

  (* Simple string replacement with variable lookup and filters *)
  fun replace_variables (template, vars) =
    let
      (* Parse a variable with optional filter *)
      fun parse_var var_str =
        case String.tokens (fn c => c = #"|") var_str of
          [] => ("", NONE)
        | [var_name] => (string_trim var_name, NONE)
        | var_name::filter_name::_ => (string_trim var_name, SOME (string_trim filter_name))
      
      (* Apply filter if specified *)
      fun apply_filter value filter_name_opt =
        case filter_name_opt of
          NONE => value
        | SOME filter_name =>
            case get_filter filter_name of
              SOME filter_fn => filter_fn value
            | NONE => value
      
      (* Recursive replacement function *)
      fun replace_all acc remaining =
        if remaining = "" then
          acc
        else
          case string_findSubstring "{{" remaining of
            NONE => acc ^ remaining
          | SOME start_idx =>
              let
                val prefix = String.substring(remaining, 0, start_idx)
                val after_start = String.substring(remaining, 
                                                 start_idx + 2, 
                                                 String.size remaining - start_idx - 2)
              in
                case string_findSubstring "}}" after_start of
                  NONE => acc ^ remaining (* Unclosed variable, return as-is *)
                | SOME end_idx =>
                    let
                      val var_expr = String.substring(after_start, 0, end_idx)
                      val (var_name, filter) = parse_var var_expr
                      
                      val var_value = case lookup_var vars var_name of
                                        SOME v => v
                                      | NONE => raise UndefinedVariable var_name
                                        
                      val final_value = apply_filter var_value filter
                      
                      val remaining_suffix = String.substring(after_start, 
                                                           end_idx + 2, 
                                                           String.size after_start - end_idx - 2)
                    in
                      replace_all (acc ^ prefix ^ final_value) remaining_suffix
                    end
              end
    in
      replace_all "" template
    end
  
  (* Conditional replacement with if blocks *)
  fun replace_conditionals (template, vars) =
    let
      (* Recursive replacement function *)
      fun replace_all acc remaining =
        if remaining = "" then
          acc
        else
          case string_findSubstring "{%if" remaining of
            NONE => acc ^ remaining
          | SOME start_idx =>
              let
                val prefix = String.substring(remaining, 0, start_idx)
                val after_start = String.substring(remaining, 
                                                 start_idx + 4, 
                                                 String.size remaining - start_idx - 4)
              in
                case string_findSubstring "%}" after_start of
                  NONE => acc ^ remaining (* Unclosed if, return as-is *)
                | SOME cond_end_idx =>
                    let
                      val condition = string_trim(String.substring(after_start, 0, cond_end_idx))
                      val after_cond = String.substring(after_start, 
                                                      cond_end_idx + 2, 
                                                      String.size after_start - cond_end_idx - 2)
                    in
                      case string_findSubstring "{%endif%}" after_cond of
                        NONE => acc ^ remaining (* No endif, return as-is *)
                      | SOME endif_idx =>
                          let
                            val if_content = String.substring(after_cond, 0, endif_idx)
                            val else_content = 
                              case string_findSubstring "{%else%}" if_content of
                                SOME else_idx =>
                                  let
                                    val true_content = String.substring(if_content, 0, else_idx)
                                    val false_content = String.substring(if_content, 
                                                                      else_idx + 7, 
                                                                      String.size if_content - else_idx - 7)
                                  in
                                    (true_content, false_content)
                                  end
                              | NONE => (if_content, "")
                                
                            val var_name = string_trim condition
                            val condition_result = 
                              case lookup_var vars var_name of
                                SOME "true" => true
                              | SOME "yes" => true
                              | SOME "1" => true
                              | _ => false
                                
                            val result_content = if condition_result then #1 else_content else #2 else_content
                                
                            val remaining_suffix = String.substring(after_cond, 
                                                                 endif_idx + 9, 
                                                                 String.size after_cond - endif_idx - 9)
                          in
                            replace_all (acc ^ prefix ^ result_content) remaining_suffix
                          end
                    end
              end
    in
      replace_all "" template
    end
  
  (* Process a template string with variables *)
  fun process_string (template, vars) =
    let
      (* First replace conditionals, then variables *)
      val after_conditionals = replace_conditionals (template, vars)
    in
      replace_variables (after_conditionals, vars)
      handle UndefinedVariable var => raise UndefinedVariable var
    end
    
  (* Process a template file *)
  fun process_file (input_file, vars, output_file) =
    let
      (* Read the input file *)
      val template_content = 
        let
          val file = TextIO.openIn input_file
                     handle _ => raise TemplateError ("Cannot open template file: " ^ input_file)
          val content = TextIO.inputAll file
          val _ = TextIO.closeIn file
        in
          content
        end
        
      (* Process the template *)
      val processed = process_string (template_content, vars)
      
      (* Write to the output file *)
      val out = TextIO.openOut output_file
               handle _ => raise TemplateError ("Cannot open output file: " ^ output_file)
      val _ = TextIO.output (out, processed)
      val _ = TextIO.closeOut out
    in
      ()
    end

  (* Initialize default filters *)
  val _ = default_filters()
end