(* Generated by ocaml-tree-sitter. *)
(*
   ruby grammar

   entrypoint: program
*)

open! Sexplib.Conv
open Tree_sitter_run

type element_reference_bracket = Token.t

type imm_tok_ri = Token.t (* "ri" *)

type string_content = Token.t

type imm_tok_r = Token.t (* "r" *)

type hash_key_symbol = Token.t

type pat_3d340f6 = Token.t (* pattern \s+ *)

type string_start = Token.t

type tok_pat_562b724_pat_f7bc484 = Token.t

type line_break = Token.t

type string_array_start = Token.t

type class_variable = Token.t

type block_ampersand = Token.t

type uninterpreted = Token.t (* pattern (.|\s)* *)

type imm_tok_eq = Token.t (* "=" *)

type splat_star = Token.t

type anon_choice_BANG_b88b9c5 = [
    `BANG of Token.t (* "!" *)
  | `TILDE of Token.t (* "~" *)
]

type string_end = Token.t

type semgrep_ellipsis_followed_by_newline = Token.t

type anon_choice_PLUSEQ_6a24756 = [
    `PLUSEQ of Token.t (* "+=" *)
  | `DASHEQ of Token.t (* "-=" *)
  | `STAREQ of Token.t (* "*=" *)
  | `STARSTAREQ of Token.t (* "**=" *)
  | `SLASHEQ of Token.t (* "/=" *)
  | `BARBAREQ of Token.t (* "||=" *)
  | `BAREQ of Token.t (* "|=" *)
  | `AMPAMPEQ of Token.t (* "&&=" *)
  | `AMPEQ of Token.t (* "&=" *)
  | `PERCEQ of Token.t (* "%=" *)
  | `GTGTEQ of Token.t (* ">>=" *)
  | `LTLTEQ of Token.t (* "<<=" *)
  | `HATEQ of Token.t (* "^=" *)
]

type float_ =
  Token.t (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *)

type escape_sequence = Token.t

type unary_minus = Token.t

type singleton_class_left_angle_left_langle = Token.t

type symbol_array_start = Token.t

type character =
  Token.t (* pattern \?(\\\S({[0-9A-Fa-f]*}|[0-9A-Fa-f]*|-\S([MC]-\S)?)?|\S) *)

type pat_74d21aa = Token.t (* pattern __END__[\r\n] *)

type instance_variable = Token.t

type hash_splat_nil = (Token.t (* "**" *) * Token.t (* "nil" *))

type constant_suffix_ = Token.t

type tok_pat_562b724_pat_f7bc484_pat_38b534e = Token.t

type identifier = Token.t

type regex_start = Token.t

type binary_star = Token.t

type heredoc_content = Token.t

type operator = [
    `DOTDOT of Token.t (* ".." *)
  | `BAR of Token.t (* "|" *)
  | `HAT of Token.t (* "^" *)
  | `AMP of Token.t (* "&" *)
  | `LTEQGT of Token.t (* "<=>" *)
  | `EQEQ of Token.t (* "==" *)
  | `EQEQEQ of Token.t (* "===" *)
  | `EQTILDE of Token.t (* "=~" *)
  | `GT of Token.t (* ">" *)
  | `GTEQ of Token.t (* ">=" *)
  | `LT of Token.t (* "<" *)
  | `LTEQ of Token.t (* "<=" *)
  | `PLUS of Token.t (* "+" *)
  | `DASH of Token.t (* "-" *)
  | `STAR of Token.t (* "*" *)
  | `SLASH of Token.t (* "/" *)
  | `PERC of Token.t (* "%" *)
  | `BANG of Token.t (* "!" *)
  | `BANGTILDE of Token.t (* "!~" *)
  | `STARSTAR of Token.t (* "**" *)
  | `LTLT of Token.t (* "<<" *)
  | `GTGT of Token.t (* ">>" *)
  | `TILDE of Token.t (* "~" *)
  | `PLUSAT of Token.t (* "+@" *)
  | `DASHAT of Token.t (* "-@" *)
  | `TILDEAT of Token.t (* "~@" *)
  | `LBRACKRBRACK of Token.t (* "[]" *)
  | `LBRACKRBRACKEQ of Token.t (* "[]=" *)
  | `BQUOT of Token.t (* "`" *)
]

type no_line_break = Token.t

type hash_splat_star_star = Token.t

type simple_symbol = Token.t

type imm_tok_lpar = Token.t (* "(" *)

type semgrep_metavariable = Token.t

type integer =
  Token.t (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])* *)

type imm_tok_lbrack = Token.t (* "[" *)

type short_interpolation = Token.t

type imm_tok_coloncolon = Token.t (* "::" *)

type binary_minus = Token.t

type tok_prec_p1000_dotdotdot_comma = Token.t

type heredoc_body_start = Token.t

type unary_minus_num = Token.t

type identifier_suffix_ = Token.t

type imm_tok_i = Token.t (* "i" *)

type heredoc_end = Token.t

type subshell_start = Token.t

type binary_star_star = Token.t

type tok_pat_3fee85b_pat_f7bc484_pat_38b534e = Token.t

type heredoc_beginning = Token.t

type global_variable =
  Token.t (* pattern "\\$(-[a-zA-Z0-9_]|[!@&`'+~=/\\\\,;.<>*$?:\"]|[0-9]+|[a-zA-Z_][a-zA-Z0-9_]*\
  )" *)

type imm_tok_colon = Token.t (* ":" *)

type symbol_start = Token.t

type anon_choice_DOTDOT_ed078ec = [
    `DOTDOT of Token.t (* ".." *)
  | `DOTDOTDOT of Token.t (* "..." *)
]

type terminator = [
    `Line_brk of line_break (*tok*)
  | `SEMI of Token.t (* ";" *)
]

type constant_suffix = [
    `Tok_pat_562b724_pat_f7bc484_pat_38b534e of
      tok_pat_562b724_pat_f7bc484_pat_38b534e
  | `Cst_suffix_ of constant_suffix_ (*tok*)
]

type splat_parameter = (Token.t (* "*" *) * identifier (*tok*) option)

type hash_splat_parameter = (Token.t (* "**" *) * identifier (*tok*) option)

type constant = [
    `Tok_pat_562b724_pat_f7bc484 of tok_pat_562b724_pat_f7bc484
  | `Semg_meta of semgrep_metavariable (*tok*)
]

type int_or_float = [ `Int of integer (*tok*) | `Float of float_ (*tok*) ]

type call_operator = [
    `DOT of Token.t (* "." *)
  | `AMPDOT of Token.t (* "&." *)
  | `Imm_tok_colo of imm_tok_coloncolon (*tok*)
]

type identifier_suffix = [
    `Tok_pat_3fee85b_pat_f7bc484_pat_38b534e of
      tok_pat_3fee85b_pat_f7bc484_pat_38b534e
  | `Id_suffix_ of identifier_suffix_ (*tok*)
]

type nonlocal_variable = [
    `Inst_var of instance_variable (*tok*)
  | `Class_var of class_variable (*tok*)
  | `Global_var of global_variable (*tok*)
]

type keyword_variable = [
    `Nil of Token.t (* "nil" *)
  | `Self of Token.t (* "self" *)
  | `True of Token.t (* "true" *)
  | `False of Token.t (* "false" *)
  | `Line of Token.t (* "__LINE__" *)
  | `File of Token.t (* "__FILE__" *)
  | `Enco of Token.t (* "__ENCODING__" *)
]

type hash_pattern_any_rest = [
    `Hash_splat_param of hash_splat_parameter
  | `Hash_splat_nil of hash_splat_nil
]

type pattern_constant = [
    `Cst of constant
  | `Pat_cst_resol of (
        pattern_constant option
      * Token.t (* "::" *)
      * constant
    )
]

type complex = [
    `Int_or_float_imm_tok_i of (int_or_float * imm_tok_i (*tok*))
  | `Int_or_float_imm_tok_ri of (int_or_float * imm_tok_ri (*tok*))
]

type function_identifier = [
    `Id_suffix of identifier_suffix
  | `Cst_suffix of constant_suffix
]

type variable = [
    `Self of Token.t (* "self" *)
  | `Super of Token.t (* "super" *)
  | `Nonl_var of nonlocal_variable
  | `Id of identifier (*tok*)
  | `Cst of constant
]

type simple_numeric = [
    `Int of integer (*tok*)
  | `Float of float_ (*tok*)
  | `Comp of complex
  | `Rati of (int_or_float * imm_tok_r (*tok*))
]

type function_identifier_call = function_identifier

type anon_choice_var_2a392d7 = [
    `Var of variable
  | `Func_id of function_identifier_call
]

type numeric = [
    `Simple_nume of simple_numeric
  | `Un_lit of (
        [
            `Un_minus_num of unary_minus_num (*tok*)
          | `PLUS of Token.t (* "+" *)
        ]
      * simple_numeric
    )
]

type anon_choice_call__23b9492 = [
    `Call_ of call_
  | `Choice_var of anon_choice_var_2a392d7
]

and anon_choice_choice_call__cfb94af = [
    `Choice_call_ of anon_choice_call__23b9492
  | `Prim_choice_DOT of (primary * call_operator)
]

and anon_choice_cst_c1a97cb = [
    `Cst of constant
  | `Scope_resol of scope_resolution
]

and anon_choice_else_4cfa13b = [
    `Else of else_
  | `Elsif of (
        Token.t (* "elsif" *)
      * statement
      * anon_choice_term_b9e1843
      * anon_choice_else_4cfa13b option
    )
]

and anon_choice_exp_dae8cc5 = [
    `Exp of expression
  | `Rescue_modi_exp of (expression * Token.t (* "rescue" *) * arg)
]

and anon_choice_lhs_3a98eae = [
    `Lhs of lhs
  | `Rest_assign of (Token.t (* "*" *) * lhs option)
  | `Dest_left_assign of (
        Token.t (* "(" *) * left_assignment_list * Token.t (* ")" *)
    )
]

and anon_choice_lhs_6f12f8f = [
    `Lhs of lhs
  | `Left_assign_list of left_assignment_list
]

and anon_choice_pair_a4f33e2 = [
    `Pair of pair
  | `Hash_splat_arg of hash_splat_argument
]

and anon_choice_rescue_d627f1b = [
    `Rescue of (
        Token.t (* "rescue" *)
      * exceptions option
      * exception_variable option
      * anon_choice_term_b9e1843
    )
  | `Else of else_
  | `Ensure of (Token.t (* "ensure" *) * block_body option)
]

and anon_choice_term_b9e1843 = [ `Term of terminator | `Then of then_ ]

and anon_formal_param_rep_COMMA_formal_param_fcb57c2 = (
    formal_parameter
  * (Token.t (* "," *) * formal_parameter) list (* zero or more *)
)

and anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e = (
    literal_contents
  * (pat_3d340f6 * literal_contents) list (* zero or more *)
)

and arg = [
    `Un_minus_pow of (unary_minus_num (*tok*) * pow)
  | `Prim of primary
  | `Assign of assignment
  | `Op_assign of (lhs * anon_choice_PLUSEQ_6a24756 * arg_rhs)
  | `Cond of (arg * Token.t (* "?" *) * arg * Token.t (* ":" *) * arg)
  | `Range of range
  | `Bin of binary
  | `Un of unary
]

and arg_rhs = [
    `Arg of arg
  | `Rescue_modi_arg of (arg * Token.t (* "rescue" *) * arg)
]

and argument = [
    `Exp of expression
  | `Splat_arg of splat_argument
  | `Hash_splat_arg of hash_splat_argument
  | `Forw_arg of Token.t (* "..." *)
  | `Blk_arg of (block_ampersand (*tok*) * arg option)
  | `Pair of pair
]

and argument_list = (
    imm_tok_lpar (*tok*)
  * argument_list_with_trailing_comma option
  * Token.t (* ")" *)
)

and argument_list_with_trailing_comma = (
    argument
  * (Token.t (* "," *) * argument) list (* zero or more *)
  * Token.t (* "," *) option
)

and array_ = (
    Token.t (* "[" *)
  * argument_list_with_trailing_comma option
  * Token.t (* "]" *)
)

and array_pattern = [
    `LBRACK_opt_array_pat_body_RBRACK of (
        Token.t (* "[" *)
      * array_pattern_body option
      * Token.t (* "]" *)
    )
  | `Pat_cst_imm_tok_lbrack_opt_array_pat_body_RBRACK of (
        pattern_constant
      * imm_tok_lbrack (*tok*)
      * array_pattern_body option
      * Token.t (* "]" *)
    )
  | `Pat_cst_imm_tok_lpar_opt_array_pat_body_RPAR of (
        pattern_constant
      * imm_tok_lpar (*tok*)
      * array_pattern_body option
      * Token.t (* ")" *)
    )
]

and array_pattern_body = [
    `Pat_expr of pattern_expr
  | `Array_pat_n of array_pattern_n
]

and array_pattern_n = [
    `Pat_expr_COMMA of (pattern_expr * Token.t (* "," *))
  | `Pat_expr_COMMA_choice_pat_expr of (
        pattern_expr * Token.t (* "," *) * array_pattern_body
    )
  | `Splat_param_rep_COMMA_pat_expr of (
        splat_parameter
      * (Token.t (* "," *) * pattern_expr) list (* zero or more *)
    )
]

and assignment = [
  `Choice_lhs_EQ_choice_choice_arg of (
      anon_choice_lhs_6f12f8f
    * Token.t (* "=" *)
    * [
          `Choice_arg of arg_rhs
        | `Splat_arg of splat_argument
        | `Right_assign_list of right_assignment_list
      ]
  )
]

and bare_parameters = (
    simple_formal_parameter
  * (Token.t (* "," *) * formal_parameter) list (* zero or more *)
)

and begin_ = (
    Token.t (* "begin" *)
  * terminator option
  * body_statement option
  * Token.t (* "end" *)
)

and binary = [
    `Arg_and_arg of (arg * Token.t (* "and" *) * arg)
  | `Arg_or_arg of (arg * Token.t (* "or" *) * arg)
  | `Arg_BARBAR_arg of (arg * Token.t (* "||" *) * arg)
  | `Arg_AMPAMP_arg of (arg * Token.t (* "&&" *) * arg)
  | `Arg_choice_LTLT_arg of (
        arg
      * [ `LTLT of Token.t (* "<<" *) | `GTGT of Token.t (* ">>" *) ]
      * arg
    )
  | `Arg_choice_LT_arg of (
        arg
      * [
            `LT of Token.t (* "<" *)
          | `LTEQ of Token.t (* "<=" *)
          | `GT of Token.t (* ">" *)
          | `GTEQ of Token.t (* ">=" *)
        ]
      * arg
    )
  | `Arg_AMP_arg of (arg * Token.t (* "&" *) * arg)
  | `Arg_choice_HAT_arg of (
        arg
      * [ `HAT of Token.t (* "^" *) | `BAR of Token.t (* "|" *) ]
      * arg
    )
  | `Arg_choice_PLUS_arg of (
        arg
      * [ `PLUS of Token.t (* "+" *) | `Bin_minus of binary_minus (*tok*) ]
      * arg
    )
  | `Arg_choice_SLASH_arg of (
        arg
      * [
            `SLASH of Token.t (* "/" *)
          | `PERC of Token.t (* "%" *)
          | `Bin_star of binary_star (*tok*)
        ]
      * arg
    )
  | `Arg_choice_EQEQ_arg of (
        arg
      * [
            `EQEQ of Token.t (* "==" *)
          | `BANGEQ of Token.t (* "!=" *)
          | `EQEQEQ of Token.t (* "===" *)
          | `LTEQGT of Token.t (* "<=>" *)
          | `EQTILDE of Token.t (* "=~" *)
          | `BANGTILDE of Token.t (* "!~" *)
        ]
      * arg
    )
  | `Arg_bin_star_star_arg of (arg * binary_star_star (*tok*) * arg)
]

and block = (
    Token.t (* "{" *)
  * block_parameters option
  * block_body option
  * Token.t (* "}" *)
)

and block_body = statements

and block_parameters = (
    Token.t (* "|" *)
  * anon_formal_param_rep_COMMA_formal_param_fcb57c2 option
  * Token.t (* "," *) option
  * (
        Token.t (* ";" *)
      * identifier (*tok*)
      * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
    )
      option
  * Token.t (* "|" *)
)

and body_expr = (Token.t (* "=" *) * arg_rhs)

and body_statement = body_statement_

and body_statement_ = [
    `Stmts_rep_choice_rescue of (
        block_body
      * anon_choice_rescue_d627f1b list (* zero or more *)
    )
  | `Opt_stmts_rep1_choice_rescue of (
        block_body option
      * anon_choice_rescue_d627f1b list (* one or more *)
    )
]

and break = (Token.t (* "break" *) * argument_list option)

and call = [
    `Choice_choice_call__arg_list of (
        anon_choice_choice_call__cfb94af * argument_list
    )
  | `Choice_choice_call__arg_list_blk of (
        anon_choice_choice_call__cfb94af * argument_list * block
    )
  | `Choice_choice_call__arg_list_do_blk of (
        anon_choice_choice_call__cfb94af * argument_list * do_block
    )
  | `Choice_call__blk of (anon_choice_call__23b9492 * block)
  | `Choice_call__do_blk of (anon_choice_call__23b9492 * do_block)
]

and call_ = (
    primary
  * call_operator
  * [
        `Id of identifier (*tok*)
      | `Op of operator
      | `Cst of constant
      | `Func_id of function_identifier_call
    ]
)

and case = (
    Token.t (* "case" *)
  * statement option
  * terminator option
  * when_ list (* zero or more *)
  * else_ option
  * Token.t (* "end" *)
)

and case_match = (
    Token.t (* "case" *)
  * statement
  * terminator option
  * in_clause list (* one or more *)
  * else_ option
  * Token.t (* "end" *)
)

and chained_command_call = (
    command_call_with_block
  * call_operator
  * [
        `Id of identifier (*tok*)
      | `Func_id of function_identifier_call
      | `Op of operator
      | `Cst of constant
    ]
)

and chained_string = (string_ * string_ list (* one or more *))

and class_ = (
    Token.t (* "class" *)
  * anon_choice_cst_c1a97cb
  * [
        `Supe_term of (superclass * terminator)
      | `Opt_term of terminator option
    ]
  * body_statement option
  * Token.t (* "end" *)
)

and command_argument_list = [
    `Tok_prec_p1000_dotd_comma_arg_rep_COMMA_arg of (
        tok_prec_p1000_dotdotdot_comma
      * argument
      * (Token.t (* "," *) * argument) list (* zero or more *)
    )
  | `Arg_rep_COMMA_arg of (
        argument
      * (Token.t (* "," *) * argument) list (* zero or more *)
    )
]

and command_call_with_block = [
    `Choice_call__cmd_arg_list_blk of (
        anon_choice_call__23b9492 * command_argument_list * block
    )
  | `Choice_call__cmd_arg_list_do_blk of (
        anon_choice_call__23b9492 * command_argument_list * do_block
    )
]

and command_unary = [
    `Defi_exp of (Token.t (* "defined?" *) * expression)
  | `Not_exp of (Token.t (* "not" *) * expression)
  | `Choice_un_minus_exp of (
        [ `Un_minus of unary_minus (*tok*) | `PLUS of Token.t (* "+" *) ]
      * expression
    )
  | `Choice_BANG_exp of (anon_choice_BANG_b88b9c5 * expression)
]

and delimited_symbol = (
    symbol_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)

and do_ = (
    [ `Do of Token.t (* "do" *) | `Term of terminator ]
  * block_body option
  * Token.t (* "end" *)
)

and do_block = (
    Token.t (* "do" *)
  * terminator option
  * (block_parameters * terminator option) option
  * body_statement option
  * Token.t (* "end" *)
)

and else_ = (Token.t (* "else" *) * terminator option * block_body option)

and exception_variable = (Token.t (* "=>" *) * lhs)

and exceptions = (
    pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
)

and expression = [
    `Cmd_bin of (
        expression
      * [ `Or of Token.t (* "or" *) | `And of Token.t (* "and" *) ]
      * expression
    )
  | `Cmd_un of command_unary
  | `Cmd_assign of (
        anon_choice_lhs_6f12f8f * Token.t (* "=" *) * anon_choice_exp_dae8cc5
    )
  | `Cmd_op_assign of (
        lhs * anon_choice_PLUSEQ_6a24756 * anon_choice_exp_dae8cc5
    )
  | `Cmd_call of (
        [
            `Call_ of call_
          | `Chai_cmd_call of chained_command_call
          | `Choice_var of anon_choice_var_2a392d7
        ]
      * command_argument_list
    )
  | `Cmd_call_with_blk of command_call_with_block
  | `Chai_cmd_call of chained_command_call
  | `Ret_cmd of (Token.t (* "return" *) * command_argument_list)
  | `Yield_cmd of (Token.t (* "yield" *) * command_argument_list)
  | `Brk_cmd of (Token.t (* "break" *) * command_argument_list)
  | `Next_cmd of (Token.t (* "next" *) * command_argument_list)
  | `Match_pat of (arg * Token.t (* "=>" *) * pattern_top_expr_body)
  | `Test_pat of (arg * Token.t (* "in" *) * pattern_top_expr_body)
  | `Arg of arg
]

and find_pattern = [
    `LBRACK_find_pat_body_RBRACK of (
        Token.t (* "[" *) * find_pattern_body * Token.t (* "]" *)
    )
  | `Pat_cst_imm_tok_lbrack_find_pat_body_RBRACK of (
        pattern_constant * imm_tok_lbrack (*tok*) * find_pattern_body
      * Token.t (* "]" *)
    )
  | `Pat_cst_imm_tok_lpar_find_pat_body_RPAR of (
        pattern_constant * imm_tok_lpar (*tok*) * find_pattern_body
      * Token.t (* ")" *)
    )
]

and find_pattern_body = (
    splat_parameter
  * (Token.t (* "," *) * pattern_expr) list (* one or more *)
  * Token.t (* "," *)
  * splat_parameter
)

and for_ = (Token.t (* "for" *) * anon_choice_lhs_6f12f8f * in_ * do_)

and formal_parameter = [
    `Simple_formal_param of simple_formal_parameter
  | `Params of parameters
]

and guard = [
    `If_guard of (Token.t (* "if" *) * expression)
  | `Unless_guard of (Token.t (* "unless" *) * expression)
]

and hash = (
    Token.t (* "{" *)
  * (
        anon_choice_pair_a4f33e2
      * (Token.t (* "," *) * anon_choice_pair_a4f33e2)
          list (* zero or more *)
      * Token.t (* "," *) option
    )
      option
  * Token.t (* "}" *)
)

and hash_pattern = [
    `LCURL_opt_hash_pat_body_RCURL of (
        Token.t (* "{" *)
      * hash_pattern_body option
      * Token.t (* "}" *)
    )
  | `Pat_cst_imm_tok_lbrack_hash_pat_body_RBRACK of (
        pattern_constant * imm_tok_lbrack (*tok*) * hash_pattern_body
      * Token.t (* "]" *)
    )
  | `Pat_cst_imm_tok_lpar_hash_pat_body_RPAR of (
        pattern_constant * imm_tok_lpar (*tok*) * hash_pattern_body
      * Token.t (* ")" *)
    )
]

and hash_pattern_body = [
    `Kw_pat_rep_COMMA_kw_pat_opt_COMMA of (
        keyword_pattern
      * (Token.t (* "," *) * keyword_pattern) list (* zero or more *)
      * Token.t (* "," *) option
    )
  | `Kw_pat_rep_COMMA_kw_pat_COMMA_hash_pat_any_rest of (
        keyword_pattern
      * (Token.t (* "," *) * keyword_pattern) list (* zero or more *)
      * Token.t (* "," *)
      * hash_pattern_any_rest
    )
  | `Hash_pat_any_rest of hash_pattern_any_rest
]

and hash_splat_argument = (hash_splat_star_star (*tok*) * arg option)

and if_ = (
    Token.t (* "if" *)
  * statement
  * anon_choice_term_b9e1843
  * anon_choice_else_4cfa13b option
  * Token.t (* "end" *)
)

and in_ = (Token.t (* "in" *) * arg)

and in_clause = (
    Token.t (* "in" *)
  * pattern_top_expr_body
  * guard option
  * anon_choice_term_b9e1843
)

and interpolation = [
    `HASHLCURL_opt_stmts_RCURL of (
        Token.t (* "#{" *)
      * block_body option
      * Token.t (* "}" *)
    )
  | `Short_interp_nonl_var of (
        short_interpolation (*tok*) * nonlocal_variable
    )
]

and keyword_pattern = [
    `Choice_id_imm_tok_colon_opt_pat_expr of (
        [
            `Id of identifier (*tok*)
          | `Cst of constant
          | `Id_suffix of identifier_suffix
          | `Cst_suffix of constant_suffix
          | `Str of string_
        ]
      * imm_tok_colon (*tok*)
      * pattern_expr option
    )
  | `Semg_ellips of Token.t (* "..." *)
]

and lambda = (
    Token.t (* "->" *)
  * [ `Params of parameters | `Bare_params of bare_parameters ] option
  * [ `Blk of block | `Do_blk of do_block ]
)

and left_assignment_list = mlhs

and lhs = [
    `Var of variable
  | `True of Token.t (* "true" *)
  | `False of Token.t (* "false" *)
  | `Nil of Token.t (* "nil" *)
  | `Scope_resol of scope_resolution
  | `Elem_ref of (
        primary
      * element_reference_bracket (*tok*)
      * argument_list_with_trailing_comma option
      * Token.t (* "]" *)
    )
  | `Call_ of call_
]

and literal = [
    `Simple_symb of simple_symbol (*tok*)
  | `Deli_symb of delimited_symbol
  | `Nume of numeric
]

and literal_contents =
  [
      `Str_content of string_content (*tok*)
    | `Interp of interpolation
    | `Esc_seq of escape_sequence (*tok*)
  ]
    list (* one or more *)

and method_ = (Token.t (* "def" *) * method_rest)

and method_name = [
    `Id of identifier (*tok*)
  | `Func_id of function_identifier_call
  | `Cst of constant
  | `Setter of (identifier (*tok*) * imm_tok_eq (*tok*))
  | `Simple_symb of simple_symbol (*tok*)
  | `Deli_symb of delimited_symbol
  | `Op of operator
  | `Nonl_var of nonlocal_variable
]

and method_rest = (
    method_name
  * [
        `Body_expr of body_expr
      | `Params_choice_opt_term_opt_body_stmt_end of (
            parameters
          * [
                `Opt_term_opt_body_stmt_end of (
                    terminator option
                  * body_statement option
                  * Token.t (* "end" *)
                )
              | `Body_expr of body_expr
            ]
        )
      | `Opt_bare_params_term_opt_body_stmt_end of (
            bare_parameters option
          * terminator
          * body_statement option
          * Token.t (* "end" *)
        )
    ]
)

and mlhs = (
    anon_choice_lhs_3a98eae
  * (Token.t (* "," *) * anon_choice_lhs_3a98eae) list (* zero or more *)
  * Token.t (* "," *) option
)

and module_ = (
    Token.t (* "module" *)
  * anon_choice_cst_c1a97cb
  * terminator option
  * body_statement option
  * Token.t (* "end" *)
)

and next = (Token.t (* "next" *) * argument_list option)

and pair = [
    `Choice_arg_EQGT_arg of [
        `Arg_EQGT_arg of (arg * Token.t (* "=>" *) * arg)
      | `Choice_str_imm_tok_colon_arg of (
            [ `Str of string_ ]
          * imm_tok_colon (*tok*)
          * arg
        )
      | `Choice_hash_key_symb_imm_tok_colon_choice_opt_arg of (
            [
                `Hash_key_symb of hash_key_symbol (*tok*)
              | `Id of identifier (*tok*)
              | `Cst of constant
              | `Id_suffix of identifier_suffix
              | `Cst_suffix of constant_suffix
            ]
          * imm_tok_colon (*tok*)
          * [
                `Opt_arg of arg option
              | `No_line_brk of no_line_break (*tok*)
            ]
        )
    ]
  | `Semg_ellips of Token.t (* "..." *)
]

and parameters = (
    Token.t (* "(" *)
  * anon_formal_param_rep_COMMA_formal_param_fcb57c2 option
  * Token.t (* ")" *)
)

and parenthesized_statements = (
    Token.t (* "(" *)
  * block_body option
  * Token.t (* ")" *)
)

and parenthesized_unary = (
    [ `Defi of Token.t (* "defined?" *) | `Not of Token.t (* "not" *) ]
  * parenthesized_statements
)

and pattern = [ `Arg of arg | `Splat_arg of splat_argument ]

and pattern_expr = [
    `As_pat of (pattern_expr * Token.t (* "=>" *) * identifier (*tok*))
  | `Pat_expr_alt of pattern_expr_alt
]

and pattern_expr_alt = [
    `Alt_pat of (
        pattern_expr_basic
      * (Token.t (* "|" *) * pattern_expr_basic) list (* one or more *)
    )
  | `Pat_expr_basic of pattern_expr_basic
]

and pattern_expr_basic = [
    `Pat_value of pattern_value
  | `Id of identifier (*tok*)
  | `Array_pat of array_pattern
  | `Find_pat of find_pattern
  | `Hash_pat of hash_pattern
  | `Paren_pat of (Token.t (* "(" *) * pattern_expr * Token.t (* ")" *))
]

and pattern_lambda = lambda

and pattern_literal = [
    `Lit of literal
  | `Str of string_
  | `Subs of subshell
  | `Here_begin of heredoc_beginning (*tok*)
  | `Regex of regex
  | `Str_array of string_array
  | `Symb_array of symbol_array
  | `Kw_var of keyword_variable
]

and pattern_primitive = [
    `Pat_lit of pattern_literal
  | `Pat_lambda of pattern_lambda
]

and pattern_range = [
    `Pat_prim_choice_DOTDOT_pat_prim of (
        pattern_primitive * anon_choice_DOTDOT_ed078ec * pattern_primitive
    )
  | `Choice_DOTDOT_pat_prim of (
        anon_choice_DOTDOT_ed078ec * pattern_primitive
    )
  | `Pat_prim_choice_DOTDOT of (
        pattern_primitive * anon_choice_DOTDOT_ed078ec
    )
]

and pattern_top_expr_body = [
    `Pat_expr of pattern_expr
  | `Array_pat_n of array_pattern_n
  | `Find_pat_body of find_pattern_body
  | `Hash_pat_body of hash_pattern_body
]

and pattern_value = [
    `Pat_prim of pattern_primitive
  | `Pat_range of pattern_range
  | `Var_ref_pat of (
        Token.t (* "^" *)
      * [ `Id of identifier (*tok*) | `Nonl_var of nonlocal_variable ]
    )
  | `Exp_ref_pat of (
        Token.t (* "^" *) * Token.t (* "(" *) * expression
      * Token.t (* ")" *)
    )
  | `Pat_cst of pattern_constant
]

and pow = (simple_numeric * binary_star_star (*tok*) * arg)

and primary = [
    `Choice_paren_stmts of [
        `Paren_stmts of parenthesized_statements
      | `Lhs of lhs
      | `Func_id_call of function_identifier_call
      | `Call of call
      | `Array of array_
      | `Str_array of string_array
      | `Symb_array of symbol_array
      | `Hash of hash
      | `Subs of subshell
      | `Lit of literal
      | `Str of string_
      | `Char of character (*tok*)
      | `Chai_str of chained_string
      | `Regex of regex
      | `Lambda of pattern_lambda
      | `Meth of method_
      | `Sing_meth of singleton_method
      | `Class of class_
      | `Sing_class of singleton_class
      | `Module of module_
      | `Begin of begin_
      | `While of while_
      | `Until of until
      | `If of if_
      | `Unless of unless
      | `For of for_
      | `Case of case
      | `Case_match of case_match
      | `Ret of return
      | `Yield of yield
      | `Brk of break
      | `Next of next
      | `Redo of redo
      | `Retry of retry
      | `Paren_un of parenthesized_unary
      | `Here_begin of heredoc_beginning (*tok*)
    ]
  | `Semg_ellips of Token.t (* "..." *)
  | `Semg_ellips_foll_by_nl of semgrep_ellipsis_followed_by_newline (*tok*)
  | `Deep_ellips of (
        Token.t (* "<..." *) * expression * Token.t (* "...>" *)
    )
]

and range = [
    `Arg_choice_DOTDOT_arg of (arg * anon_choice_DOTDOT_ed078ec * arg)
  | `Choice_DOTDOT_arg of (anon_choice_DOTDOT_ed078ec * arg)
  | `Arg_choice_DOTDOT of (arg * anon_choice_DOTDOT_ed078ec)
]

and redo = (Token.t (* "redo" *) * argument_list option)

and regex = (
    regex_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)

and retry = (Token.t (* "retry" *) * argument_list option)

and return = (Token.t (* "return" *) * argument_list option)

and right_assignment_list = (
    pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
)

and scope_resolution = (
    [
        `COLONCOLON of Token.t (* "::" *)
      | `Prim_imm_tok_colo of (primary * imm_tok_coloncolon (*tok*))
    ]
  * constant
)

and simple_formal_parameter = [
    `Id of identifier (*tok*)
  | `Splat_param of splat_parameter
  | `Hash_splat_param of hash_splat_parameter
  | `Hash_splat_nil of hash_splat_nil
  | `Forw_param of Token.t (* "..." *)
  | `Blk_param of (Token.t (* "&" *) * identifier (*tok*) option)
  | `Kw_param of (identifier (*tok*) * imm_tok_colon (*tok*) * arg option)
  | `Opt_param of (identifier (*tok*) * Token.t (* "=" *) * arg)
]

and singleton_class = (
    Token.t (* "class" *)
  * singleton_class_left_angle_left_langle (*tok*)
  * arg
  * terminator
  * body_statement option
  * Token.t (* "end" *)
)

and singleton_method = (
    Token.t (* "def" *)
  * [
        `Var of variable
      | `LPAR_arg_RPAR of (Token.t (* "(" *) * arg * Token.t (* ")" *))
    ]
  * [ `DOT of Token.t (* "." *) | `COLONCOLON of Token.t (* "::" *) ]
  * method_rest
)

and splat_argument = (splat_star (*tok*) * arg option)

and statement = [
    `Undef of (
        Token.t (* "undef" *)
      * method_name
      * (Token.t (* "," *) * method_name) list (* zero or more *)
    )
  | `Alias of (Token.t (* "alias" *) * method_name * method_name)
  | `If_modi of (statement * Token.t (* "if" *) * expression)
  | `Unless_modi of (statement * Token.t (* "unless" *) * expression)
  | `While_modi of (statement * Token.t (* "while" *) * expression)
  | `Until_modi of (statement * Token.t (* "until" *) * expression)
  | `Rescue_modi of (statement * Token.t (* "rescue" *) * expression)
  | `Begin_blk of (
        Token.t (* "BEGIN" *)
      * Token.t (* "{" *)
      * block_body option
      * Token.t (* "}" *)
    )
  | `End_blk of (
        Token.t (* "END" *)
      * Token.t (* "{" *)
      * block_body option
      * Token.t (* "}" *)
    )
  | `Exp of expression
]

and statements = [
    `Rep1_choice_stmt_term_opt_stmt of (
        [
            `Stmt_term of (statement * terminator)
          | `Empty_stmt of Token.t (* ";" *)
        ]
          list (* one or more *)
      * statement option
    )
  | `Stmt of statement
]

and string_ = (
    string_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)

and string_array = (
    string_array_start (*tok*)
  * pat_3d340f6 option
  * anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e option
  * pat_3d340f6 option
  * string_end (*tok*)
)

and subshell = (
    subshell_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)

and superclass = (Token.t (* "<" *) * expression)

and symbol_array = (
    symbol_array_start (*tok*)
  * pat_3d340f6 option
  * anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e option
  * pat_3d340f6 option
  * string_end (*tok*)
)

and then_ = [
    `Term_stmts of (terminator * block_body)
  | `Opt_term_then_opt_stmts of (
        terminator option
      * Token.t (* "then" *)
      * block_body option
    )
]

and unary = [
    `Defi_arg of (Token.t (* "defined?" *) * arg)
  | `Not_arg of (Token.t (* "not" *) * arg)
  | `Choice_un_minus_arg of (
        [
            `Un_minus of unary_minus (*tok*)
          | `Bin_minus of binary_minus (*tok*)
          | `PLUS of Token.t (* "+" *)
        ]
      * arg
    )
  | `Choice_BANG_arg of (anon_choice_BANG_b88b9c5 * arg)
]

and unless = (
    Token.t (* "unless" *)
  * statement
  * anon_choice_term_b9e1843
  * anon_choice_else_4cfa13b option
  * Token.t (* "end" *)
)

and until = (Token.t (* "until" *) * statement * do_)

and when_ = (
    Token.t (* "when" *)
  * pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
  * anon_choice_term_b9e1843
)

and while_ = (Token.t (* "while" *) * statement * do_)

and yield = (Token.t (* "yield" *) * argument_list option)

type program = (
    block_body option
  * [
        `Pat_74d21aa_unin of (pat_74d21aa * uninterpreted (*tok*))
      | `X___END___ of (Token.t (* "__END__" *) * Token.t (* "" *))
    ]
      option
)

type forward_argument (* inlined *) = Token.t (* "..." *)

type semgrep_ellipsis (* inlined *) = Token.t (* "..." *)

type file (* inlined *) = Token.t (* "__FILE__" *)

type nil (* inlined *) = Token.t (* "nil" *)

type self (* inlined *) = Token.t (* "self" *)

type forward_parameter (* inlined *) = Token.t (* "..." *)

type comment (* inlined *) = Token.t

type empty_statement (* inlined *) = Token.t (* ";" *)

type line (* inlined *) = Token.t (* "__LINE__" *)

type super (* inlined *) = Token.t (* "super" *)

type false_ (* inlined *) = Token.t (* "false" *)

type true_ (* inlined *) = Token.t (* "true" *)

type encoding (* inlined *) = Token.t (* "__ENCODING__" *)

type block_parameter (* inlined *) = (
    Token.t (* "&" *)
  * identifier (*tok*) option
)

type setter (* inlined *) = (identifier (*tok*) * imm_tok_eq (*tok*))

type pattern_constant_resolution (* inlined *) = (
    pattern_constant option
  * Token.t (* "::" *)
  * constant
)

type rational (* inlined *) = (int_or_float * imm_tok_r (*tok*))

type variable_reference_pattern (* inlined *) = (
    Token.t (* "^" *)
  * [ `Id of identifier (*tok*) | `Nonl_var of nonlocal_variable ]
)

type unary_literal (* inlined *) = (
    [ `Un_minus_num of unary_minus_num (*tok*) | `PLUS of Token.t (* "+" *) ]
  * simple_numeric
)

type alias (* inlined *) = (
    Token.t (* "alias" *) * method_name * method_name
)

type alternative_pattern (* inlined *) = (
    pattern_expr_basic
  * (Token.t (* "|" *) * pattern_expr_basic) list (* one or more *)
)

type as_pattern (* inlined *) = (
    pattern_expr * Token.t (* "=>" *) * identifier (*tok*)
)

type begin_block (* inlined *) = (
    Token.t (* "BEGIN" *)
  * Token.t (* "{" *)
  * block_body option
  * Token.t (* "}" *)
)

type block_argument (* inlined *) = (block_ampersand (*tok*) * arg option)

type break_command (* inlined *) = (
    Token.t (* "break" *) * command_argument_list
)

type command_assignment (* inlined *) = (
    anon_choice_lhs_6f12f8f * Token.t (* "=" *) * anon_choice_exp_dae8cc5
)

type command_binary (* inlined *) = (
    expression
  * [ `Or of Token.t (* "or" *) | `And of Token.t (* "and" *) ]
  * expression
)

type command_call (* inlined *) = (
    [
        `Call_ of call_
      | `Chai_cmd_call of chained_command_call
      | `Choice_var of anon_choice_var_2a392d7
    ]
  * command_argument_list
)

type command_operator_assignment (* inlined *) = (
    lhs * anon_choice_PLUSEQ_6a24756 * anon_choice_exp_dae8cc5
)

type conditional (* inlined *) = (
    arg * Token.t (* "?" *) * arg * Token.t (* ":" *) * arg
)

type deep_ellipsis (* inlined *) = (
    Token.t (* "<..." *) * expression * Token.t (* "...>" *)
)

type destructured_left_assignment (* inlined *) = (
    Token.t (* "(" *) * left_assignment_list * Token.t (* ")" *)
)

type element_reference (* inlined *) = (
    primary
  * element_reference_bracket (*tok*)
  * argument_list_with_trailing_comma option
  * Token.t (* "]" *)
)

type elsif (* inlined *) = (
    Token.t (* "elsif" *)
  * statement
  * anon_choice_term_b9e1843
  * anon_choice_else_4cfa13b option
)

type end_block (* inlined *) = (
    Token.t (* "END" *)
  * Token.t (* "{" *)
  * block_body option
  * Token.t (* "}" *)
)

type ensure (* inlined *) = (Token.t (* "ensure" *) * block_body option)

type expression_reference_pattern (* inlined *) = (
    Token.t (* "^" *) * Token.t (* "(" *) * expression * Token.t (* ")" *)
)

type if_guard (* inlined *) = (Token.t (* "if" *) * expression)

type if_modifier (* inlined *) = (
    statement * Token.t (* "if" *) * expression
)

type keyword_parameter (* inlined *) = (
    identifier (*tok*)
  * imm_tok_colon (*tok*)
  * arg option
)

type match_pattern (* inlined *) = (
    arg * Token.t (* "=>" *) * pattern_top_expr_body
)

type next_command (* inlined *) = (
    Token.t (* "next" *) * command_argument_list
)

type operator_assignment (* inlined *) = (
    lhs * anon_choice_PLUSEQ_6a24756 * arg_rhs
)

type optional_parameter (* inlined *) = (
    identifier (*tok*) * Token.t (* "=" *) * arg
)

type parenthesized_pattern (* inlined *) = (
    Token.t (* "(" *) * pattern_expr * Token.t (* ")" *)
)

type rescue (* inlined *) = (
    Token.t (* "rescue" *)
  * exceptions option
  * exception_variable option
  * anon_choice_term_b9e1843
)

type rescue_modifier (* inlined *) = (
    statement * Token.t (* "rescue" *) * expression
)

type rescue_modifier_arg (* inlined *) = (arg * Token.t (* "rescue" *) * arg)

type rescue_modifier_expression (* inlined *) = (
    expression * Token.t (* "rescue" *) * arg
)

type rest_assignment (* inlined *) = (Token.t (* "*" *) * lhs option)

type return_command (* inlined *) = (
    Token.t (* "return" *) * command_argument_list
)

type test_pattern (* inlined *) = (
    arg * Token.t (* "in" *) * pattern_top_expr_body
)

type unary_minus_pow (* inlined *) = (unary_minus_num (*tok*) * pow)

type undef (* inlined *) = (
    Token.t (* "undef" *)
  * method_name
  * (Token.t (* "," *) * method_name) list (* zero or more *)
)

type unless_guard (* inlined *) = (Token.t (* "unless" *) * expression)

type unless_modifier (* inlined *) = (
    statement * Token.t (* "unless" *) * expression
)

type until_modifier (* inlined *) = (
    statement * Token.t (* "until" *) * expression
)

type while_modifier (* inlined *) = (
    statement * Token.t (* "while" *) * expression
)

type yield_command (* inlined *) = (
    Token.t (* "yield" *) * command_argument_list
)

type heredoc_body (* inlined *) = (
    heredoc_body_start (*tok*)
  * [
        `Here_content of heredoc_content (*tok*)
      | `Interp of interpolation
      | `Esc_seq of escape_sequence (*tok*)
    ]
      list (* zero or more *)
  * heredoc_end (*tok*)
)
