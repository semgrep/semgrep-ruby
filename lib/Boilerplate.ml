(**
   Boilerplate to be used as a template when mapping the ruby CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_splat_star (env : env) (tok : CST.splat_star) =
  (* splat_star *) token env tok

let map_hash_splat_star_star (env : env) (tok : CST.hash_splat_star_star) =
  (* hash_splat_star_star *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_anon_choice_PLUSEQ_6a24756 (env : env) (x : CST.anon_choice_PLUSEQ_6a24756) =
  (match x with
  | `PLUSEQ tok -> R.Case ("PLUSEQ",
      (* "+=" *) token env tok
    )
  | `DASHEQ tok -> R.Case ("DASHEQ",
      (* "-=" *) token env tok
    )
  | `STAREQ tok -> R.Case ("STAREQ",
      (* "*=" *) token env tok
    )
  | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
      (* "**=" *) token env tok
    )
  | `SLASHEQ tok -> R.Case ("SLASHEQ",
      (* "/=" *) token env tok
    )
  | `BARBAREQ tok -> R.Case ("BARBAREQ",
      (* "||=" *) token env tok
    )
  | `BAREQ tok -> R.Case ("BAREQ",
      (* "|=" *) token env tok
    )
  | `AMPAMPEQ tok -> R.Case ("AMPAMPEQ",
      (* "&&=" *) token env tok
    )
  | `AMPEQ tok -> R.Case ("AMPEQ",
      (* "&=" *) token env tok
    )
  | `PERCEQ tok -> R.Case ("PERCEQ",
      (* "%=" *) token env tok
    )
  | `GTGTEQ tok -> R.Case ("GTGTEQ",
      (* ">>=" *) token env tok
    )
  | `LTLTEQ tok -> R.Case ("LTLTEQ",
      (* "<<=" *) token env tok
    )
  | `HATEQ tok -> R.Case ("HATEQ",
      (* "^=" *) token env tok
    )
  )

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_unary_minus (env : env) (tok : CST.unary_minus) =
  (* unary_minus *) token env tok

let map_complex (env : env) (tok : CST.complex) =
  (* pattern (\d+)?(\+|-)?(\d+)i *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_character (env : env) (tok : CST.character) =
  (* pattern \?(\\\S({[0-9A-Fa-f]*}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S) *) token env tok

let map_binary_star (env : env) (tok : CST.binary_star) =
  (* binary_star *) token env tok

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_anon_choice_BANG_b88b9c5 (env : env) (x : CST.anon_choice_BANG_b88b9c5) =
  (match x with
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  )

let map_hash_key_symbol (env : env) (tok : CST.hash_key_symbol) =
  (* hash_key_symbol *) token env tok

let map_anon_choice_DOT_5431c66 (env : env) (x : CST.anon_choice_DOT_5431c66) =
  (match x with
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `AMPDOT tok -> R.Case ("AMPDOT",
      (* "&." *) token env tok
    )
  )

let map_constant (env : env) (tok : CST.constant) =
  (* constant *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *) token env tok

let map_symbol_array_start (env : env) (tok : CST.symbol_array_start) =
  (* symbol_array_start *) token env tok

let map_heredoc_beginning (env : env) (tok : CST.heredoc_beginning) =
  (* heredoc_beginning *) token env tok

let map_binary_star_star (env : env) (tok : CST.binary_star_star) =
  (* binary_star_star *) token env tok

let map_class_variable (env : env) (tok : CST.class_variable) =
  (* class_variable *) token env tok

let map_uninterpreted (env : env) (tok : CST.uninterpreted) =
  (* pattern (.|\s)* *) token env tok

let map_string_array_start (env : env) (tok : CST.string_array_start) =
  (* string_array_start *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_element_reference_bracket (env : env) (tok : CST.element_reference_bracket) =
  (* element_reference_bracket *) token env tok

let map_operator (env : env) (x : CST.operator) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `LTEQGT tok -> R.Case ("LTEQGT",
      (* "<=>" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `EQEQEQ tok -> R.Case ("EQEQEQ",
      (* "===" *) token env tok
    )
  | `EQTILDE tok -> R.Case ("EQTILDE",
      (* "=~" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `BANGTILDE tok -> R.Case ("BANGTILDE",
      (* "!~" *) token env tok
    )
  | `STARSTAR tok -> R.Case ("STARSTAR",
      (* "**" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `PLUSAT tok -> R.Case ("PLUSAT",
      (* "+@" *) token env tok
    )
  | `DASHAT tok -> R.Case ("DASHAT",
      (* "-@" *) token env tok
    )
  | `LBRACKRBRACK tok -> R.Case ("LBRACKRBRACK",
      (* "[]" *) token env tok
    )
  | `LBRACKRBRACKEQ tok -> R.Case ("LBRACKRBRACKEQ",
      (* "[]=" *) token env tok
    )
  | `BQUOT tok -> R.Case ("BQUOT",
      (* "`" *) token env tok
    )
  )

let map_subshell_start (env : env) (tok : CST.subshell_start) =
  (* subshell_start *) token env tok

let map_true_ (env : env) (tok : CST.true_) =
  (* true *) token env tok

let map_nil (env : env) (tok : CST.nil) =
  (* nil *) token env tok

let map_symbol_start (env : env) (tok : CST.symbol_start) =
  (* symbol_start *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_singleton_class_left_angle_left_langle (env : env) (tok : CST.singleton_class_left_angle_left_langle) =
  (* singleton_class_left_angle_left_langle *) token env tok

let map_global_variable (env : env) (tok : CST.global_variable) =
  (* pattern "\\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *) token env tok

let map_heredoc_body_start (env : env) (tok : CST.heredoc_body_start) =
  (* heredoc_body_start *) token env tok

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_simple_symbol (env : env) (tok : CST.simple_symbol) =
  (* simple_symbol *) token env tok

let map_regex_start (env : env) (tok : CST.regex_start) =
  (* regex_start *) token env tok

let map_imm_tok_coloncolon (env : env) (tok : CST.imm_tok_coloncolon) =
  (* "::" *) token env tok

let map_line_break (env : env) (tok : CST.line_break) =
  (* line_break *) token env tok

let map_pat_3d340f6 (env : env) (tok : CST.pat_3d340f6) =
  (* pattern \s+ *) token env tok

let map_block_ampersand (env : env) (tok : CST.block_ampersand) =
  (* block_ampersand *) token env tok

let map_binary_minus (env : env) (tok : CST.binary_minus) =
  (* binary_minus *) token env tok

let map_instance_variable (env : env) (tok : CST.instance_variable) =
  (* instance_variable *) token env tok

let map_heredoc_content (env : env) (tok : CST.heredoc_content) =
  (* heredoc_content *) token env tok

let map_anon_choice_DOTDOT_ed078ec (env : env) (x : CST.anon_choice_DOTDOT_ed078ec) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

let map_false_ (env : env) (tok : CST.false_) =
  (* false *) token env tok

let map_anon_choice_un_minus_157a1bc (env : env) (x : CST.anon_choice_un_minus_157a1bc) =
  (match x with
  | `Un_minus tok -> R.Case ("Un_minus",
      (* unary_minus *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  )

let map_anon_choice_int_e7b97da (env : env) (x : CST.anon_choice_int_e7b97da) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *) token env tok
    )
  )

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `Line_brk tok -> R.Case ("Line_brk",
      (* line_break *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Self tok -> R.Case ("Self",
      (* "self" *) token env tok
    )
  | `Super tok -> R.Case ("Super",
      (* "super" *) token env tok
    )
  | `Inst_var tok -> R.Case ("Inst_var",
      (* instance_variable *) token env tok
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_variable *) token env tok
    )
  | `Global_var tok -> R.Case ("Global_var",
      (* pattern "\\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Cst tok -> R.Case ("Cst",
      (* constant *) token env tok
    )
  )

let rec map_anon_choice_call_fd54051 (env : env) (x : CST.anon_choice_call_fd54051) =
  (match x with
  | `Call x -> R.Case ("Call",
      map_call env x
    )
  | `Choice_var x -> R.Case ("Choice_var",
      map_anon_choice_var_18b08b3 env x
    )
  )

and map_anon_choice_cst_c1a97cb (env : env) (x : CST.anon_choice_cst_c1a97cb) =
  (match x with
  | `Cst tok -> R.Case ("Cst",
      (* constant *) token env tok
    )
  | `Scope_resol x -> R.Case ("Scope_resol",
      map_scope_resolution env x
    )
  )

and map_anon_choice_else_4cfa13b (env : env) (x : CST.anon_choice_else_4cfa13b) =
  (match x with
  | `Else x -> R.Case ("Else",
      map_else_ env x
    )
  | `Elsif (v1, v2, v3, v4) -> R.Case ("Elsif",
      let v1 = (* "elsif" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_else_4cfa13b env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_id_5ca805c (env : env) (x : CST.anon_choice_id_5ca805c) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Op x -> R.Case ("Op",
      map_operator env x
    )
  | `Cst tok -> R.Case ("Cst",
      (* constant *) token env tok
    )
  | `Arg_list x -> R.Case ("Arg_list",
      map_argument_list env x
    )
  )

and map_anon_choice_lhs_3a98eae (env : env) (x : CST.anon_choice_lhs_3a98eae) =
  (match x with
  | `Lhs x -> R.Case ("Lhs",
      map_lhs env x
    )
  | `Rest_assign (v1, v2) -> R.Case ("Rest_assign",
      let v1 = (* "*" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_lhs env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Dest_left_assign (v1, v2, v3) -> R.Case ("Dest_left_assign",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_left_assignment_list env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_lhs_6f12f8f (env : env) (x : CST.anon_choice_lhs_6f12f8f) =
  (match x with
  | `Lhs x -> R.Case ("Lhs",
      map_lhs env x
    )
  | `Left_assign_list x -> R.Case ("Left_assign_list",
      map_left_assignment_list env x
    )
  )

and map_anon_choice_pair_a4f33e2 (env : env) (x : CST.anon_choice_pair_a4f33e2) =
  (match x with
  | `Pair x -> R.Case ("Pair",
      map_pair env x
    )
  | `Hash_splat_arg x -> R.Case ("Hash_splat_arg",
      map_hash_splat_argument env x
    )
  )

and map_anon_choice_term_b9e1843 (env : env) (x : CST.anon_choice_term_b9e1843) =
  (match x with
  | `Term x -> R.Case ("Term",
      map_terminator env x
    )
  | `Then x -> R.Case ("Then",
      map_then_ env x
    )
  )

and map_anon_choice_var_18b08b3 (env : env) (x : CST.anon_choice_var_18b08b3) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Scope_resol x -> R.Case ("Scope_resol",
      map_scope_resolution env x
    )
  )

and map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 (env : env) ((v1, v2) : CST.anon_formal_param_rep_COMMA_formal_param_fcb57c2) =
  let v1 = map_formal_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_formal_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e (env : env) ((v1, v2) : CST.anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e) =
  let v1 = map_literal_contents env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_pat_3d340f6 env v1 in
      let v2 = map_literal_contents env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_arg (env : env) (x : CST.arg) =
  (match x with
  | `Prim x -> R.Case ("Prim",
      map_primary env x
    )
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Op_assign (v1, v2, v3) -> R.Case ("Op_assign",
      let v1 = map_lhs env v1 in
      let v2 = map_anon_choice_PLUSEQ_6a24756 env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cond (v1, v2, v3, v4, v5) -> R.Case ("Cond",
      let v1 = map_arg env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_arg env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_arg env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Range x -> R.Case ("Range",
      map_range env x
    )
  | `Bin x -> R.Case ("Bin",
      map_binary env x
    )
  | `Un x -> R.Case ("Un",
      map_unary env x
    )
  )

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Splat_arg x -> R.Case ("Splat_arg",
      map_splat_argument env x
    )
  | `Hash_splat_arg x -> R.Case ("Hash_splat_arg",
      map_hash_splat_argument env x
    )
  | `Blk_arg (v1, v2) -> R.Case ("Blk_arg",
      let v1 = (* block_ampersand *) token env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Pair x -> R.Case ("Pair",
      map_pair env x
    )
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument_list_with_trailing_comma env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_argument_list_with_trailing_comma (env : env) ((v1, v2, v3) : CST.argument_list_with_trailing_comma) =
  let v1 = map_argument env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Choice_lhs_EQ_choice_arg (v1, v2, v3) -> R.Case ("Choice_lhs_EQ_choice_arg",
      let v1 = map_anon_choice_lhs_6f12f8f env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Arg x -> R.Case ("Arg",
            map_arg env x
          )
        | `Splat_arg x -> R.Case ("Splat_arg",
            map_splat_argument env x
          )
        | `Right_assign_list x -> R.Case ("Right_assign_list",
            map_right_assignment_list env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bare_parameters (env : env) ((v1, v2) : CST.bare_parameters) =
  let v1 = map_simple_formal_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_formal_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_binary (env : env) (x : CST.binary) =
  (match x with
  | `Arg_and_arg (v1, v2, v3) -> R.Case ("Arg_and_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_or_arg (v1, v2, v3) -> R.Case ("Arg_or_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_BARBAR_arg (v1, v2, v3) -> R.Case ("Arg_BARBAR_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_AMPAMP_arg (v1, v2, v3) -> R.Case ("Arg_AMPAMP_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_LTLT_arg (v1, v2, v3) -> R.Case ("Arg_choice_LTLT_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> R.Case ("LTLT",
            (* "<<" *) token env tok
          )
        | `GTGT tok -> R.Case ("GTGT",
            (* ">>" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_LT_arg (v1, v2, v3) -> R.Case ("Arg_choice_LT_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_AMP_arg (v1, v2, v3) -> R.Case ("Arg_AMP_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_HAT_arg (v1, v2, v3) -> R.Case ("Arg_choice_HAT_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> R.Case ("HAT",
            (* "^" *) token env tok
          )
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_PLUS_arg (v1, v2, v3) -> R.Case ("Arg_choice_PLUS_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `Bin_minus tok -> R.Case ("Bin_minus",
            (* binary_minus *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_SLASH_arg (v1, v2, v3) -> R.Case ("Arg_choice_SLASH_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        | `Bin_star tok -> R.Case ("Bin_star",
            (* binary_star *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_choice_EQEQ_arg (v1, v2, v3) -> R.Case ("Arg_choice_EQEQ_arg",
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `EQEQEQ tok -> R.Case ("EQEQEQ",
            (* "===" *) token env tok
          )
        | `LTEQGT tok -> R.Case ("LTEQGT",
            (* "<=>" *) token env tok
          )
        | `EQTILDE tok -> R.Case ("EQTILDE",
            (* "=~" *) token env tok
          )
        | `BANGTILDE tok -> R.Case ("BANGTILDE",
            (* "!~" *) token env tok
          )
        )
      in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_bin_star_star_arg (v1, v2, v3) -> R.Case ("Arg_bin_star_star_arg",
      let v1 = map_arg env v1 in
      let v2 = (* binary_star_star *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_block_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.block_parameters) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* ";" *) token env v1 in
        let v2 = (* identifier *) token env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v5 = (* "|" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_body_statement (env : env) ((v1, v2, v3) : CST.body_statement) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Rescue x -> R.Case ("Rescue",
          map_rescue env x
        )
      | `Else x -> R.Case ("Else",
          map_else_ env x
        )
      | `Ensure x -> R.Case ("Ensure",
          map_ensure env x
        )
      )
    ) v2)
  in
  let v3 = (* "end" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call (env : env) ((v1, v2, v3) : CST.call) =
  let v1 = map_primary env v1 in
  let v2 = map_anon_choice_DOT_5431c66 env v2 in
  let v3 = map_anon_choice_id_5ca805c env v3 in
  R.Tuple [v1; v2; v3]

and map_call_ (env : env) (x : CST.call_) =
  (match x with
  | `Choice_call_arg_list (v1, v2) -> R.Case ("Choice_call_arg_list",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_call_arg_list_blk (v1, v2, v3) -> R.Case ("Choice_call_arg_list_blk",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_argument_list env v2 in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_call_arg_list_do_blk (v1, v2, v3) -> R.Case ("Choice_call_arg_list_do_blk",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_argument_list env v2 in
      let v3 = map_do_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_call_blk (v1, v2) -> R.Case ("Choice_call_blk",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_call_do_blk (v1, v2) -> R.Case ("Choice_call_do_blk",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_do_block env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_chained_command_call (env : env) ((v1, v2, v3) : CST.chained_command_call) =
  let v1 = map_command_call_with_block env v1 in
  let v2 = map_anon_choice_DOT_5431c66 env v2 in
  let v3 = map_anon_choice_id_5ca805c env v3 in
  R.Tuple [v1; v2; v3]

and map_command_argument_list (env : env) ((v1, v2) : CST.command_argument_list) =
  let v1 = map_argument env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_command_assignment (env : env) (x : CST.command_assignment) =
  (match x with
  | `Choice_lhs_EQ_exp (v1, v2, v3) -> R.Case ("Choice_lhs_EQ_exp",
      let v1 = map_anon_choice_lhs_6f12f8f env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_command_call_with_block (env : env) (x : CST.command_call_with_block) =
  (match x with
  | `Choice_call_cmd_arg_list_blk (v1, v2, v3) -> R.Case ("Choice_call_cmd_arg_list_blk",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_command_argument_list env v2 in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_call_cmd_arg_list_do_blk (v1, v2, v3) -> R.Case ("Choice_call_cmd_arg_list_do_blk",
      let v1 = map_anon_choice_call_fd54051 env v1 in
      let v2 = map_command_argument_list env v2 in
      let v3 = map_do_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_command_unary (env : env) (x : CST.command_unary) =
  (match x with
  | `Defi_exp (v1, v2) -> R.Case ("Defi_exp",
      let v1 = (* "defined?" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Not_exp (v1, v2) -> R.Case ("Not_exp",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_un_minus_exp (v1, v2) -> R.Case ("Choice_un_minus_exp",
      let v1 = map_anon_choice_un_minus_157a1bc env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_BANG_exp (v1, v2) -> R.Case ("Choice_BANG_exp",
      let v1 = map_anon_choice_BANG_b88b9c5 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_delimited_symbol (env : env) ((v1, v2, v3) : CST.delimited_symbol) =
  let v1 = (* symbol_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_literal_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_do_ (env : env) ((v1, v2, v3) : CST.do_) =
  let v1 =
    (match v1 with
    | `Do tok -> R.Case ("Do",
        (* "do" *) token env tok
      )
    | `Term x -> R.Case ("Term",
        map_terminator env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "end" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_do_block (env : env) ((v1, v2, v3, v4) : CST.do_block) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_block_parameters env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_terminator env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = map_body_statement env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_else_ (env : env) ((v1, v2, v3) : CST.else_) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_ensure (env : env) ((v1, v2) : CST.ensure) =
  let v1 = (* "ensure" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_exception_variable (env : env) ((v1, v2) : CST.exception_variable) =
  let v1 = (* "=>" *) token env v1 in
  let v2 = map_lhs env v2 in
  R.Tuple [v1; v2]

and map_exceptions (env : env) ((v1, v2) : CST.exceptions) =
  let v1 = map_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Cmd_bin (v1, v2, v3) -> R.Case ("Cmd_bin",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `Or tok -> R.Case ("Or",
            (* "or" *) token env tok
          )
        | `And tok -> R.Case ("And",
            (* "and" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cmd_un x -> R.Case ("Cmd_un",
      map_command_unary env x
    )
  | `Cmd_assign x -> R.Case ("Cmd_assign",
      map_command_assignment env x
    )
  | `Cmd_op_assign (v1, v2, v3) -> R.Case ("Cmd_op_assign",
      let v1 = map_lhs env v1 in
      let v2 = map_anon_choice_PLUSEQ_6a24756 env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cmd_call (v1, v2) -> R.Case ("Cmd_call",
      let v1 =
        (match v1 with
        | `Call x -> R.Case ("Call",
            map_call env x
          )
        | `Chai_cmd_call x -> R.Case ("Chai_cmd_call",
            map_chained_command_call env x
          )
        | `Choice_var x -> R.Case ("Choice_var",
            map_anon_choice_var_18b08b3 env x
          )
        )
      in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Cmd_call_with_blk x -> R.Case ("Cmd_call_with_blk",
      map_command_call_with_block env x
    )
  | `Chai_cmd_call x -> R.Case ("Chai_cmd_call",
      map_chained_command_call env x
    )
  | `Ret_cmd (v1, v2) -> R.Case ("Ret_cmd",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Yield_cmd (v1, v2) -> R.Case ("Yield_cmd",
      let v1 = (* "yield" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Brk_cmd (v1, v2) -> R.Case ("Brk_cmd",
      let v1 = (* "break" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Next_cmd (v1, v2) -> R.Case ("Next_cmd",
      let v1 = (* "next" *) token env v1 in
      let v2 = map_command_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Arg x -> R.Case ("Arg",
      map_arg env x
    )
  )

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Simple_formal_param x -> R.Case ("Simple_formal_param",
      map_simple_formal_parameter env x
    )
  | `Params x -> R.Case ("Params",
      map_parameters env x
    )
  )

and map_hash_splat_argument (env : env) ((v1, v2) : CST.hash_splat_argument) =
  let v1 = (* hash_splat_star_star *) token env v1 in
  let v2 = map_arg env v2 in
  R.Tuple [v1; v2]

and map_in_ (env : env) ((v1, v2) : CST.in_) =
  let v1 = (* "in" *) token env v1 in
  let v2 = map_arg env v2 in
  R.Tuple [v1; v2]

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = (* "#{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statement env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_left_assignment_list (env : env) (x : CST.left_assignment_list) =
  map_mlhs env x

and map_lhs (env : env) (x : CST.lhs) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `True tok -> R.Case ("True",
      (* true *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* false *) token env tok
    )
  | `Nil tok -> R.Case ("Nil",
      (* nil *) token env tok
    )
  | `Scope_resol x -> R.Case ("Scope_resol",
      map_scope_resolution env x
    )
  | `Elem_ref (v1, v2, v3, v4) -> R.Case ("Elem_ref",
      let v1 = map_primary env v1 in
      let v2 = (* element_reference_bracket *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_argument_list_with_trailing_comma env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Call x -> R.Case ("Call",
      map_call env x
    )
  | `Call_ x -> R.Case ("Call_",
      map_call_ env x
    )
  )

and map_literal_contents (env : env) (xs : CST.literal_contents) =
  R.List (List.map (fun x ->
    (match x with
    | `Str_content tok -> R.Case ("Str_content",
        (* string_content *) token env tok
      )
    | `Interp x -> R.Case ("Interp",
        map_interpolation env x
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    )
  ) xs)

and map_method_name (env : env) (x : CST.method_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Cst tok -> R.Case ("Cst",
      (* constant *) token env tok
    )
  | `Setter (v1, v2) -> R.Case ("Setter",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Simple_symb tok -> R.Case ("Simple_symb",
      (* simple_symbol *) token env tok
    )
  | `Deli_symb x -> R.Case ("Deli_symb",
      map_delimited_symbol env x
    )
  | `Op x -> R.Case ("Op",
      map_operator env x
    )
  | `Inst_var tok -> R.Case ("Inst_var",
      (* instance_variable *) token env tok
    )
  | `Class_var tok -> R.Case ("Class_var",
      (* class_variable *) token env tok
    )
  | `Global_var tok -> R.Case ("Global_var",
      (* pattern "\\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *) token env tok
    )
  )

and map_method_rest (env : env) ((v1, v2, v3) : CST.method_rest) =
  let v1 = map_method_name env v1 in
  let v2 =
    (match v2 with
    | `Params_opt_term (v1, v2) -> R.Case ("Params_opt_term",
        let v1 = map_parameters env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_terminator env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Opt_bare_params_term (v1, v2) -> R.Case ("Opt_bare_params_term",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_bare_parameters env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_terminator env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = map_body_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_mlhs (env : env) ((v1, v2, v3) : CST.mlhs) =
  let v1 = map_anon_choice_lhs_3a98eae env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_lhs_3a98eae env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_pair (env : env) (x : CST.pair) =
  (match x with
  | `Arg_EQGT_arg (v1, v2, v3) -> R.Case ("Arg_EQGT_arg",
      let v1 = map_arg env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_hash_key_symb_imm_tok_colon_arg (v1, v2, v3) -> R.Case ("Choice_hash_key_symb_imm_tok_colon_arg",
      let v1 =
        (match v1 with
        | `Hash_key_symb tok -> R.Case ("Hash_key_symb",
            (* hash_key_symbol *) token env tok
          )
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Cst tok -> R.Case ("Cst",
            (* constant *) token env tok
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        )
      in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_statements (env : env) ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Arg x -> R.Case ("Arg",
      map_arg env x
    )
  | `Splat_arg x -> R.Case ("Splat_arg",
      map_splat_argument env x
    )
  )

and map_primary (env : env) (x : CST.primary) =
  (match x with
  | `Paren_stmts x -> R.Case ("Paren_stmts",
      map_parenthesized_statements env x
    )
  | `Lhs x -> R.Case ("Lhs",
      map_lhs env x
    )
  | `Array (v1, v2, v3) -> R.Case ("Array",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list_with_trailing_comma env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Str_array (v1, v2, v3, v4, v5) -> R.Case ("Str_array",
      let v1 = (* string_array_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_3d340f6 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_pat_3d340f6 env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* string_end *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Symb_array (v1, v2, v3, v4, v5) -> R.Case ("Symb_array",
      let v1 = (* symbol_array_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_pat_3d340f6 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_pat_3d340f6 env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* string_end *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Hash (v1, v2, v3) -> R.Case ("Hash",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_anon_choice_pair_a4f33e2 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_pair_a4f33e2 env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Subs (v1, v2, v3) -> R.Case ("Subs",
      let v1 = (* subshell_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_literal_contents env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_symb tok -> R.Case ("Simple_symb",
      (* simple_symbol *) token env tok
    )
  | `Deli_symb x -> R.Case ("Deli_symb",
      map_delimited_symbol env x
    )
  | `Int tok -> R.Case ("Int",
      (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *) token env tok
    )
  | `Comp tok -> R.Case ("Comp",
      (* pattern (\d+)?(\+|-)?(\d+)i *) token env tok
    )
  | `Rati (v1, v2) -> R.Case ("Rati",
      let v1 = map_anon_choice_int_e7b97da env v1 in
      let v2 = (* "r" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Char tok -> R.Case ("Char",
      (* pattern \?(\\\S({[0-9A-Fa-f]*}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S) *) token env tok
    )
  | `Chai_str (v1, v2) -> R.Case ("Chai_str",
      let v1 = map_string_ env v1 in
      let v2 = R.List (List.map (map_string_ env) v2) in
      R.Tuple [v1; v2]
    )
  | `Regex (v1, v2, v3) -> R.Case ("Regex",
      let v1 = (* regex_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_literal_contents env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lambda (v1, v2, v3) -> R.Case ("Lambda",
      let v1 = (* "->" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Params x -> R.Case ("Params",
                map_parameters env x
              )
            | `Bare_params x -> R.Case ("Bare_params",
                map_bare_parameters env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Blk x -> R.Case ("Blk",
            map_block env x
          )
        | `Do_blk x -> R.Case ("Do_blk",
            map_do_block env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Meth (v1, v2) -> R.Case ("Meth",
      let v1 = (* "def" *) token env v1 in
      let v2 = map_method_rest env v2 in
      R.Tuple [v1; v2]
    )
  | `Sing_meth (v1, v2, v3, v4) -> R.Case ("Sing_meth",
      let v1 = (* "def" *) token env v1 in
      let v2 =
        (match v2 with
        | `Var x -> R.Case ("Var",
            map_variable env x
          )
        | `LPAR_arg_RPAR (v1, v2, v3) -> R.Case ("LPAR_arg_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_arg env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v3 =
        (match v3 with
        | `DOT tok -> R.Case ("DOT",
            (* "." *) token env tok
          )
        | `COLONCOLON tok -> R.Case ("COLONCOLON",
            (* "::" *) token env tok
          )
        )
      in
      let v4 = map_method_rest env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Class (v1, v2, v3, v4, v5) -> R.Case ("Class",
      let v1 = (* "class" *) token env v1 in
      let v2 = map_anon_choice_cst_c1a97cb env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_superclass env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_terminator env v4 in
      let v5 = map_body_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Sing_class (v1, v2, v3, v4, v5) -> R.Case ("Sing_class",
      let v1 = (* "class" *) token env v1 in
      let v2 =
        (* singleton_class_left_angle_left_langle *) token env v2
      in
      let v3 = map_arg env v3 in
      let v4 = map_terminator env v4 in
      let v5 = map_body_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Module (v1, v2, v3) -> R.Case ("Module",
      let v1 = (* "module" *) token env v1 in
      let v2 = map_anon_choice_cst_c1a97cb env v2 in
      let v3 =
        (match v3 with
        | `Term_body_stmt (v1, v2) -> R.Case ("Term_body_stmt",
            let v1 = map_terminator env v1 in
            let v2 = map_body_statement env v2 in
            R.Tuple [v1; v2]
          )
        | `End tok -> R.Case ("End",
            (* "end" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Begin (v1, v2, v3) -> R.Case ("Begin",
      let v1 = (* "begin" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_body_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While (v1, v2, v3) -> R.Case ("While",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_do_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Until (v1, v2, v3) -> R.Case ("Until",
      let v1 = (* "until" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_do_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If (v1, v2, v3, v4, v5) -> R.Case ("If",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_else_4cfa13b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Unless (v1, v2, v3, v4, v5) -> R.Case ("Unless",
      let v1 = (* "unless" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_else_4cfa13b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For (v1, v2, v3, v4) -> R.Case ("For",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_anon_choice_lhs_6f12f8f env v2 in
      let v3 = map_in_ env v3 in
      let v4 = map_do_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Case (v1, v2, v3, v4, v5, v6) -> R.Case ("Case",
      let v1 = (* "case" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_statement env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = R.List (List.map (map_when_ env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_else_ env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "end" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Ret (v1, v2) -> R.Case ("Ret",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Yield (v1, v2) -> R.Case ("Yield",
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Brk (v1, v2) -> R.Case ("Brk",
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Next (v1, v2) -> R.Case ("Next",
      let v1 = (* "next" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Redo (v1, v2) -> R.Case ("Redo",
      let v1 = (* "redo" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Retry (v1, v2) -> R.Case ("Retry",
      let v1 = (* "retry" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Paren_un (v1, v2) -> R.Case ("Paren_un",
      let v1 =
        (match v1 with
        | `Defi tok -> R.Case ("Defi",
            (* "defined?" *) token env tok
          )
        | `Not tok -> R.Case ("Not",
            (* "not" *) token env tok
          )
        )
      in
      let v2 = map_parenthesized_statements env v2 in
      R.Tuple [v1; v2]
    )
  | `Un_lit (v1, v2) -> R.Case ("Un_lit",
      let v1 = map_anon_choice_un_minus_157a1bc env v1 in
      let v2 = map_anon_choice_int_e7b97da env v2 in
      R.Tuple [v1; v2]
    )
  | `Here_begin tok -> R.Case ("Here_begin",
      (* heredoc_beginning *) token env tok
    )
  )

and map_range (env : env) (x : CST.range) =
  (match x with
  | `Arg_choice_DOTDOT_arg (v1, v2, v3) -> R.Case ("Arg_choice_DOTDOT_arg",
      let v1 = map_arg env v1 in
      let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_DOTDOT_arg (v1, v2) -> R.Case ("Choice_DOTDOT_arg",
      let v1 = map_anon_choice_DOTDOT_ed078ec env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Arg_choice_DOTDOT (v1, v2) -> R.Case ("Arg_choice_DOTDOT",
      let v1 = map_arg env v1 in
      let v2 = map_anon_choice_DOTDOT_ed078ec env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_rescue (env : env) ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = (* "rescue" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_exceptions env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_exception_variable env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_anon_choice_term_b9e1843 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_right_assignment_list (env : env) ((v1, v2) : CST.right_assignment_list) =
  let v1 = map_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) =
  let v1 =
    (match v1 with
    | `COLONCOLON tok -> R.Case ("COLONCOLON",
        (* "::" *) token env tok
      )
    | `Prim_imm_tok_colo (v1, v2) -> R.Case ("Prim_imm_tok_colo",
        let v1 = map_primary env v1 in
        let v2 = map_imm_tok_coloncolon env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Cst tok -> R.Case ("Cst",
        (* constant *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_simple_formal_parameter (env : env) (x : CST.simple_formal_parameter) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Splat_param (v1, v2) -> R.Case ("Splat_param",
      let v1 = (* "*" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Hash_splat_param (v1, v2) -> R.Case ("Hash_splat_param",
      let v1 = (* "**" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* identifier *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Blk_param (v1, v2) -> R.Case ("Blk_param",
      let v1 = (* "&" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Kw_param (v1, v2, v3) -> R.Case ("Kw_param",
      let v1 = (* identifier *) token env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_arg env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_param (v1, v2, v3) -> R.Case ("Opt_param",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_arg env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_splat_argument (env : env) ((v1, v2) : CST.splat_argument) =
  let v1 = (* splat_star *) token env v1 in
  let v2 = map_arg env v2 in
  R.Tuple [v1; v2]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Undef (v1, v2, v3) -> R.Case ("Undef",
      let v1 = (* "undef" *) token env v1 in
      let v2 = map_method_name env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_method_name env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Alias (v1, v2, v3) -> R.Case ("Alias",
      let v1 = (* "alias" *) token env v1 in
      let v2 = map_method_name env v2 in
      let v3 = map_method_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_modi (v1, v2, v3) -> R.Case ("If_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "if" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Unless_modi (v1, v2, v3) -> R.Case ("Unless_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "unless" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_modi (v1, v2, v3) -> R.Case ("While_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "while" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Until_modi (v1, v2, v3) -> R.Case ("Until_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "until" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rescue_modi (v1, v2, v3) -> R.Case ("Rescue_modi",
      let v1 = map_statement env v1 in
      let v2 = (* "rescue" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Begin_blk (v1, v2, v3, v4) -> R.Case ("Begin_blk",
      let v1 = (* "BEGIN" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_statements env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `End_blk (v1, v2, v3, v4) -> R.Case ("End_blk",
      let v1 = (* "END" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_statements env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_statements (env : env) (x : CST.statements) =
  (match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) -> R.Case ("Rep1_choice_stmt_term_opt_stmt",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Stmt_term (v1, v2) -> R.Case ("Stmt_term",
              let v1 = map_statement env v1 in
              let v2 = map_terminator env v2 in
              R.Tuple [v1; v2]
            )
          | `Empty_stmt tok -> R.Case ("Empty_stmt",
              (* ";" *) token env tok
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Stmt x -> R.Case ("Stmt",
      map_statement env x
    )
  )

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* string_start *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_literal_contents env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* string_end *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_then_ (env : env) (x : CST.then_) =
  (match x with
  | `Term_stmts (v1, v2) -> R.Case ("Term_stmts",
      let v1 = map_terminator env v1 in
      let v2 = map_statements env v2 in
      R.Tuple [v1; v2]
    )
  | `Opt_term_then_opt_stmts (v1, v2, v3) -> R.Case ("Opt_term_then_opt_stmts",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "then" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_statements env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_unary (env : env) (x : CST.unary) =
  (match x with
  | `Defi_arg (v1, v2) -> R.Case ("Defi_arg",
      let v1 = (* "defined?" *) token env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Not_arg (v1, v2) -> R.Case ("Not_arg",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_un_minus_arg (v1, v2) -> R.Case ("Choice_un_minus_arg",
      let v1 = map_anon_choice_un_minus_157a1bc env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_BANG_arg (v1, v2) -> R.Case ("Choice_BANG_arg",
      let v1 = map_anon_choice_BANG_b88b9c5 env v1 in
      let v2 = map_arg env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_when_ (env : env) ((v1, v2, v3, v4) : CST.when_) =
  let v1 = (* "when" *) token env v1 in
  let v2 = map_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_anon_choice_term_b9e1843 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_statements env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "__END__" *) token env v1 in
        let v2 = (* line_break *) token env v2 in
        let v3 = (* pattern (.|\s)* *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_string
  |> print_string
