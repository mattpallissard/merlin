open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_HASH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EFFECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_index_mod -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_effect_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_effect_core_type_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_effect_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_effect_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_effect_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;2;3;3;4;1;2;1;1;2;1;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;1;2;1;2;3;1;4;1;1;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;2;1;2;1;1;1;2;1;2;1;1;2;1;2;2;1;1;2;2;1;2;1;1;2;1;2;1;2;3;4;2;3;2;3;1;3;2;3;2;1;2;3;4;1;2;3;3;1;1;3;4;2;3;1;2;1;3;4;2;1;3;2;3;4;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;7;1;2;3;1;2;2;3;3;4;5;2;3;2;3;4;5;4;2;3;2;3;2;3;1;2;2;1;1;2;3;4;5;6;7;3;4;1;2;1;1;2;1;1;1;1;2;1;1;2;3;1;2;3;2;1;1;2;3;4;2;3;4;1;1;2;1;1;2;2;1;2;3;1;2;3;1;2;1;2;3;4;5;6;4;4;3;4;5;3;3;1;7;8;9;1;2;1;2;3;4;5;6;7;8;2;3;4;5;1;2;9;6;7;1;8;1;2;3;1;2;3;1;2;1;1;2;3;4;5;4;5;9;10;2;2;1;1;1;1;1;2;3;4;1;4;5;6;7;8;5;6;7;8;9;1;1;1;1;2;3;4;1;2;1;2;3;1;1;1;2;2;1;2;2;1;1;2;3;4;1;1;5;6;6;1;2;3;4;1;2;3;1;1;1;2;3;1;2;3;1;2;1;2;3;1;4;1;1;1;1;1;2;3;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;2;3;1;1;1;1;1;2;3;2;3;2;1;2;1;2;4;4;5;2;3;2;3;2;3;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;1;1;2;1;2;3;3;4;2;1;2;3;1;1;1;1;1;2;1;2;3;3;4;5;1;2;1;2;1;2;3;4;1;2;1;1;2;3;4;5;1;2;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;1;2;3;4;2;3;4;1;3;2;3;2;3;2;1;2;3;3;1;1;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;1;1;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;3;4;2;2;3;3;4;5;3;4;5;3;4;5;6;7;1;2;3;5;6;7;5;6;7;3;1;2;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;1;2;2;1;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;3;4;4;5;3;4;5;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;5;1;4;5;1;2;3;3;6;1;1;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;1;2;5;1;2;3;3;4;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;2;3;8;3;4;5;6;7;2;3;4;5;1;2;1;2;3;4;6;7;8;1;2;2;3;4;5;6;7;8;9;2;3;4;5;6;2;1;2;3;1;1;2;5;2;2;4;5;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;5;6;3;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;7;8;2;3;3;4;5;4;1;1;2;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;1;2;3;6;7;8;1;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;1;2;3;4;1;2;3;4;5;1;2;6;7;2;3;4;5;6;7;1;2;3;4;5;6;8;4;5;6;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;4;5;6;7;8;5;6;7;1;2;1;1;1;2;3;4;5;1;1;2;3;2;3;4;2;3;4;1;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;5;1;2;3;4;1;2;1;2;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;2;3;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;2;3;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;1;2;1;1;2;1;2;1;1;1;1;2;3;3;4;1;1;1;3;4;5;6;3;4;5;6;2;3;4;5;2;3;4;2;3;4;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;3;4;5;6;6;7;3;4;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;1;2;0;1;2;1;0;1;2;1;0;1;2;3;3;3;3;3;3;2;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SIG -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTIONQUESTION -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET_LWT -> true
  | T_LET -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_HASH -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_EFFECT -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 556] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 129] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 278 :: r6 in
  let r8 = [R 654] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 32] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 191] in
  let r13 = [R 33] in
  let r14 = [R 477] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 34] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 144] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = R 278 :: r23 in
  let r25 = [R 331] in
  let r26 = [R 125] in
  let r27 = Sub (r1) :: r26 in
  let r28 = R 278 :: r27 in
  let r29 = [R 311] in
  let r30 = Sub (r1) :: r29 in
  let r31 = S (T T_MINUSGREATER) :: r30 in
  let r32 = S (N N_pattern) :: r31 in
  let r33 = [R 521] in
  let r34 = Sub (r32) :: r33 in
  let r35 = [R 141] in
  let r36 = Sub (r34) :: r35 in
  let r37 = S (T T_WITH) :: r36 in
  let r38 = Sub (r1) :: r37 in
  let r39 = R 278 :: r38 in
  let r40 = [R 622] in
  let r41 = S (T T_QUESTIONQUESTION) :: r40 in
  let r42 = [R 612] in
  let r43 = [R 56] in
  let r44 = S (T T_LIDENT) :: r43 in
  let r45 = [R 605] in
  let r46 = Sub (r44) :: r45 in
  let r47 = R 278 :: r46 in
  let r48 = [R 57] in
  let r49 = S (T T_LIDENT) :: r48 in
  let r50 = [R 332] in
  let r51 = [R 279] in
  let r52 = [R 592] in
  let r53 = S (T T_RPAREN) :: r52 in
  let r54 = [R 101] in
  let r55 = [R 764] in
  let r56 = [R 192] in
  let r57 = S (T T_RBRACKET) :: r56 in
  let r58 = Sub (r15) :: r57 in
  let r59 = S (T T_LIDENT) :: r55 in
  let r60 = [R 15] in
  let r61 = S (T T_UNDERSCORE) :: r60 in
  let r62 = [R 744] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 204] in
  let r65 = Sub (r63) :: r64 in
  let r66 = [R 9] in
  let r67 = Sub (r65) :: r66 in
  let r68 = [R 111] in
  let r69 = Sub (r67) :: r68 in
  let r70 = [R 773] in
  let r71 = R 284 :: r70 in
  let r72 = Sub (r69) :: r71 in
  let r73 = S (T T_COLON) :: r72 in
  let r74 = Sub (r59) :: r73 in
  let r75 = R 278 :: r74 in
  let r76 = [R 420] in
  let r77 = S (T T_AMPERAMPER) :: r76 in
  let r78 = [R 765] in
  let r79 = S (T T_RPAREN) :: r78 in
  let r80 = Sub (r77) :: r79 in
  let r81 = [R 394] in
  let r82 = S (T T_RPAREN) :: r81 in
  let r83 = R 224 :: r82 in
  let r84 = [R 225] in
  let r85 = [R 396] in
  let r86 = S (T T_RBRACKET) :: r85 in
  let r87 = [R 398] in
  let r88 = S (T T_RBRACE) :: r87 in
  let r89 = [R 328] in
  let r90 = [R 222] in
  let r91 = S (T T_LIDENT) :: r90 in
  let r92 = [R 14] in
  let r93 = Sub (r91) :: r92 in
  let r94 = [R 443] in
  let r95 = S (T T_COLON) :: r94 in
  let r96 = [R 13] in
  let r97 = S (T T_RPAREN) :: r96 in
  let r98 = S (N N_module_type) :: r97 in
  let r99 = R 278 :: r98 in
  let r100 = R 190 :: r99 in
  let r101 = [R 561] in
  let r102 = R 286 :: r101 in
  let r103 = [R 349] in
  let r104 = S (T T_END) :: r103 in
  let r105 = Sub (r102) :: r104 in
  let r106 = [R 219] in
  let r107 = R 284 :: r106 in
  let r108 = R 511 :: r107 in
  let r109 = R 749 :: r108 in
  let r110 = S (T T_LIDENT) :: r109 in
  let r111 = R 754 :: r110 in
  let r112 = R 278 :: r111 in
  let r113 = R 190 :: r112 in
  let r114 = [R 751] in
  let r115 = S (T T_LIDENT) :: r114 in
  let r116 = [R 96] in
  let r117 = S (T T_FALSE) :: r116 in
  let r118 = [R 216] in
  let r119 = R 278 :: r118 in
  let r120 = R 211 :: r119 in
  let r121 = Sub (r117) :: r120 in
  let r122 = [R 508] in
  let r123 = Sub (r121) :: r122 in
  let r124 = [R 569] in
  let r125 = R 284 :: r124 in
  let r126 = Sub (r123) :: r125 in
  let r127 = R 488 :: r126 in
  let r128 = S (T T_PLUSEQ) :: r127 in
  let r129 = Sub (r115) :: r128 in
  let r130 = R 754 :: r129 in
  let r131 = R 278 :: r130 in
  let r132 = [R 220] in
  let r133 = R 284 :: r132 in
  let r134 = R 511 :: r133 in
  let r135 = R 749 :: r134 in
  let r136 = S (T T_LIDENT) :: r135 in
  let r137 = R 754 :: r136 in
  let r138 = [R 570] in
  let r139 = R 284 :: r138 in
  let r140 = Sub (r123) :: r139 in
  let r141 = R 488 :: r140 in
  let r142 = S (T T_PLUSEQ) :: r141 in
  let r143 = Sub (r115) :: r142 in
  let r144 = [R 758] in
  let r145 = S (T T_UNDERSCORE) :: r144 in
  let r146 = [R 753] in
  let r147 = Sub (r145) :: r146 in
  let r148 = R 759 :: r147 in
  let r149 = [R 532] in
  let r150 = Sub (r148) :: r149 in
  let r151 = [R 756] in
  let r152 = S (T T_RPAREN) :: r151 in
  let r153 = [R 757] in
  let r154 = [R 533] in
  let r155 = [R 379] in
  let r156 = S (T T_DOTDOT) :: r155 in
  let r157 = [R 750] in
  let r158 = [R 380] in
  let r159 = [R 94] in
  let r160 = [R 206] in
  let r161 = Sub (r65) :: r160 in
  let r162 = S (T T_MINUSGREATER) :: r161 in
  let r163 = Sub (r63) :: r162 in
  let r164 = [R 20] in
  let r165 = [R 484] in
  let r166 = Sub (r67) :: r165 in
  let r167 = [R 318] in
  let r168 = R 278 :: r167 in
  let r169 = Sub (r166) :: r168 in
  let r170 = [R 519] in
  let r171 = [R 543] in
  let r172 = Sub (r69) :: r171 in
  let r173 = [R 528] in
  let r174 = Sub (r172) :: r173 in
  let r175 = [R 29] in
  let r176 = S (T T_RBRACKET) :: r175 in
  let r177 = Sub (r174) :: r176 in
  let r178 = [R 28] in
  let r179 = [R 27] in
  let r180 = S (T T_RBRACKET) :: r179 in
  let r181 = [R 368] in
  let r182 = Sub (r91) :: r181 in
  let r183 = S (T T_BACKQUOTE) :: r182 in
  let r184 = [R 732] in
  let r185 = R 278 :: r184 in
  let r186 = Sub (r183) :: r185 in
  let r187 = [R 24] in
  let r188 = S (T T_RBRACKET) :: r187 in
  let r189 = [R 21] in
  let r190 = [R 25] in
  let r191 = S (T T_RBRACKET) :: r190 in
  let r192 = [R 207] in
  let r193 = [R 540] in
  let r194 = [R 752] in
  let r195 = S (T T_LIDENT) :: r194 in
  let r196 = S (T T_UIDENT) :: r89 in
  let r197 = [R 330] in
  let r198 = S (T T_RPAREN) :: r197 in
  let r199 = [R 329] in
  let r200 = [R 22] in
  let r201 = [R 205] in
  let r202 = Sub (r65) :: r201 in
  let r203 = S (T T_MINUSGREATER) :: r202 in
  let r204 = [R 541] in
  let r205 = [R 529] in
  let r206 = [R 524] in
  let r207 = Sub (r67) :: r206 in
  let r208 = [R 731] in
  let r209 = R 278 :: r208 in
  let r210 = Sub (r207) :: r209 in
  let r211 = [R 525] in
  let r212 = [R 10] in
  let r213 = Sub (r91) :: r212 in
  let r214 = [R 26] in
  let r215 = S (T T_RBRACKET) :: r214 in
  let r216 = Sub (r174) :: r215 in
  let r217 = [R 517] in
  let r218 = Sub (r183) :: r217 in
  let r219 = [R 30] in
  let r220 = S (T T_RBRACKET) :: r219 in
  let r221 = [R 485] in
  let r222 = Sub (r67) :: r221 in
  let r223 = [R 520] in
  let r224 = [R 316] in
  let r225 = [R 19] in
  let r226 = [R 95] in
  let r227 = [R 18] in
  let r228 = Sub (r115) :: r227 in
  let r229 = [R 23] in
  let r230 = [R 536] in
  let r231 = [R 12] in
  let r232 = [R 537] in
  let r233 = [R 93] in
  let r234 = [R 228] in
  let r235 = R 278 :: r234 in
  let r236 = Sub (r166) :: r235 in
  let r237 = S (T T_COLON) :: r236 in
  let r238 = S (T T_LIDENT) :: r237 in
  let r239 = R 361 :: r238 in
  let r240 = [R 230] in
  let r241 = Sub (r239) :: r240 in
  let r242 = [R 384] in
  let r243 = S (T T_RBRACE) :: r242 in
  let r244 = [R 229] in
  let r245 = R 278 :: r244 in
  let r246 = S (T T_SEMI) :: r245 in
  let r247 = R 278 :: r246 in
  let r248 = Sub (r166) :: r247 in
  let r249 = S (T T_COLON) :: r248 in
  let r250 = [R 215] in
  let r251 = R 278 :: r250 in
  let r252 = R 211 :: r251 in
  let r253 = [R 106] in
  let r254 = Sub (r61) :: r253 in
  let r255 = [R 212] in
  let r256 = [R 108] in
  let r257 = S (T T_RBRACE) :: r256 in
  let r258 = [R 107] in
  let r259 = Sub (r61) :: r258 in
  let r260 = [R 214] in
  let r261 = [R 213] in
  let r262 = Sub (r61) :: r261 in
  let r263 = Sub (r117) :: r252 in
  let r264 = [R 383] in
  let r265 = S (T T_RBRACE) :: r264 in
  let r266 = [R 381] in
  let r267 = [R 382] in
  let r268 = [R 386] in
  let r269 = S (T T_RBRACE) :: r268 in
  let r270 = [R 385] in
  let r271 = S (T T_RBRACE) :: r270 in
  let r272 = [R 218] in
  let r273 = R 284 :: r272 in
  let r274 = R 511 :: r273 in
  let r275 = [R 486] in
  let r276 = S (T T_RBRACKET) :: r275 in
  let r277 = Sub (r15) :: r276 in
  let r278 = [R 502] in
  let r279 = Sub (r121) :: r278 in
  let r280 = [R 719] in
  let r281 = R 284 :: r280 in
  let r282 = Sub (r279) :: r281 in
  let r283 = R 488 :: r282 in
  let r284 = S (T T_PLUSEQ) :: r283 in
  let r285 = Sub (r115) :: r284 in
  let r286 = R 754 :: r285 in
  let r287 = R 278 :: r286 in
  let r288 = [R 720] in
  let r289 = R 284 :: r288 in
  let r290 = Sub (r279) :: r289 in
  let r291 = R 488 :: r290 in
  let r292 = S (T T_PLUSEQ) :: r291 in
  let r293 = Sub (r115) :: r292 in
  let r294 = [R 512] in
  let r295 = Sub (r69) :: r294 in
  let r296 = S (T T_EQUAL) :: r295 in
  let r297 = [R 285] in
  let r298 = [R 103] in
  let r299 = S (T T_FALSE) :: r298 in
  let r300 = [R 193] in
  let r301 = R 278 :: r300 in
  let r302 = [R 102] in
  let r303 = [R 100] in
  let r304 = [R 99] in
  let r305 = S (T T_RPAREN) :: r304 in
  let r306 = S (T T_COLONCOLON) :: r305 in
  let r307 = [R 194] in
  let r308 = R 278 :: r307 in
  let r309 = [R 290] in
  let r310 = [R 387] in
  let r311 = R 284 :: r310 in
  let r312 = S (N N_module_expr) :: r311 in
  let r313 = R 278 :: r312 in
  let r314 = [R 388] in
  let r315 = R 284 :: r314 in
  let r316 = S (N N_module_expr) :: r315 in
  let r317 = R 278 :: r316 in
  let r318 = [R 338] in
  let r319 = S (T T_END) :: r318 in
  let r320 = S (N N_structure) :: r319 in
  let r321 = [R 148] in
  let r322 = S (T T_END) :: r321 in
  let r323 = R 295 :: r322 in
  let r324 = R 60 :: r323 in
  let r325 = R 278 :: r324 in
  let r326 = [R 58] in
  let r327 = S (T T_RPAREN) :: r326 in
  let r328 = [R 640] in
  let r329 = [R 584] in
  let r330 = [R 582] in
  let r331 = [R 636] in
  let r332 = S (T T_RPAREN) :: r331 in
  let r333 = [R 347] in
  let r334 = S (T T_UNDERSCORE) :: r333 in
  let r335 = [R 638] in
  let r336 = S (T T_RPAREN) :: r335 in
  let r337 = Sub (r334) :: r336 in
  let r338 = R 278 :: r337 in
  let r339 = [R 639] in
  let r340 = S (T T_RPAREN) :: r339 in
  let r341 = [R 351] in
  let r342 = S (N N_module_expr) :: r341 in
  let r343 = R 278 :: r342 in
  let r344 = S (T T_OF) :: r343 in
  let r345 = [R 445] in
  let r346 = S (T T_RPAREN) :: r345 in
  let r347 = [R 446] in
  let r348 = S (T T_RPAREN) :: r347 in
  let r349 = S (N N_expr) :: r348 in
  let r350 = [R 124] in
  let r351 = Sub (r34) :: r350 in
  let r352 = S (T T_WITH) :: r351 in
  let r353 = Sub (r1) :: r352 in
  let r354 = R 278 :: r353 in
  let r355 = [R 140] in
  let r356 = Sub (r34) :: r355 in
  let r357 = S (T T_WITH) :: r356 in
  let r358 = Sub (r1) :: r357 in
  let r359 = R 278 :: r358 in
  let r360 = [R 178] in
  let r361 = [R 466] in
  let r362 = S (N N_pattern) :: r361 in
  let r363 = Sub (r299) :: r362 in
  let r364 = [R 471] in
  let r365 = Sub (r363) :: r364 in
  let r366 = [R 254] in
  let r367 = Sub (r1) :: r366 in
  let r368 = S (T T_EQUAL) :: r367 in
  let r369 = Sub (r365) :: r368 in
  let r370 = [R 308] in
  let r371 = R 284 :: r370 in
  let r372 = Sub (r369) :: r371 in
  let r373 = R 495 :: r372 in
  let r374 = R 278 :: r373 in
  let r375 = [R 589] in
  let r376 = [R 550] in
  let r377 = S (N N_pattern) :: r376 in
  let r378 = [R 587] in
  let r379 = S (T T_RBRACKET) :: r378 in
  let r380 = [R 235] in
  let r381 = S (T T_LIDENT) :: r380 in
  let r382 = [R 304] in
  let r383 = R 436 :: r382 in
  let r384 = R 430 :: r383 in
  let r385 = Sub (r381) :: r384 in
  let r386 = [R 586] in
  let r387 = S (T T_RBRACE) :: r386 in
  let r388 = [R 236] in
  let r389 = S (T T_LIDENT) :: r388 in
  let r390 = [R 431] in
  let r391 = [R 437] in
  let r392 = S (T T_UNDERSCORE) :: r328 in
  let r393 = [R 635] in
  let r394 = Sub (r392) :: r393 in
  let r395 = [R 468] in
  let r396 = Sub (r394) :: r395 in
  let r397 = R 278 :: r396 in
  let r398 = [R 88] in
  let r399 = [R 645] in
  let r400 = S (T T_INT) :: r398 in
  let r401 = [R 581] in
  let r402 = Sub (r400) :: r401 in
  let r403 = [R 642] in
  let r404 = [R 647] in
  let r405 = S (T T_RBRACKET) :: r404 in
  let r406 = S (T T_LBRACKET) :: r405 in
  let r407 = [R 648] in
  let r408 = [R 459] in
  let r409 = S (N N_pattern) :: r408 in
  let r410 = R 278 :: r409 in
  let r411 = [R 460] in
  let r412 = Sub (r394) :: r411 in
  let r413 = [R 461] in
  let r414 = [R 453] in
  let r415 = [R 467] in
  let r416 = [R 649] in
  let r417 = [R 462] in
  let r418 = [R 458] in
  let r419 = [R 456] in
  let r420 = [R 306] in
  let r421 = [R 588] in
  let r422 = [R 706] in
  let r423 = Sub (r1) :: r422 in
  let r424 = S (T T_EQUAL) :: r423 in
  let r425 = [R 250] in
  let r426 = [R 247] in
  let r427 = [R 233] in
  let r428 = S (T T_LIDENT) :: r427 in
  let r429 = [R 246] in
  let r430 = S (T T_RPAREN) :: r429 in
  let r431 = [R 234] in
  let r432 = [R 243] in
  let r433 = [R 242] in
  let r434 = S (T T_RPAREN) :: r433 in
  let r435 = R 438 :: r434 in
  let r436 = [R 439] in
  let r437 = [R 265] in
  let r438 = Sub (r1) :: r437 in
  let r439 = S (T T_EQUAL) :: r438 in
  let r440 = Sub (r365) :: r439 in
  let r441 = [R 266] in
  let r442 = Sub (r440) :: r441 in
  let r443 = [R 176] in
  let r444 = Sub (r1) :: r443 in
  let r445 = S (T T_IN) :: r444 in
  let r446 = [R 263] in
  let r447 = [R 476] in
  let r448 = S (T T_UNDERSCORE) :: r447 in
  let r449 = [R 245] in
  let r450 = [R 244] in
  let r451 = S (T T_RPAREN) :: r450 in
  let r452 = R 438 :: r451 in
  let r453 = [R 262] in
  let r454 = [R 369] in
  let r455 = S (T T_LIDENT) :: r454 in
  let r456 = [R 198] in
  let r457 = Sub (r424) :: r456 in
  let r458 = [R 708] in
  let r459 = Sub (r457) :: r458 in
  let r460 = S (T T_RPAREN) :: r459 in
  let r461 = Sub (r455) :: r460 in
  let r462 = [R 248] in
  let r463 = [R 135] in
  let r464 = Sub (r1) :: r463 in
  let r465 = S (T T_IN) :: r464 in
  let r466 = S (N N_module_expr) :: r465 in
  let r467 = R 278 :: r466 in
  let r468 = R 190 :: r467 in
  let r469 = [R 256] in
  let r470 = R 284 :: r469 in
  let r471 = Sub (r369) :: r470 in
  let r472 = R 495 :: r471 in
  let r473 = R 278 :: r472 in
  let r474 = R 190 :: r473 in
  let r475 = [R 136] in
  let r476 = Sub (r1) :: r475 in
  let r477 = S (T T_IN) :: r476 in
  let r478 = S (N N_module_expr) :: r477 in
  let r479 = R 278 :: r478 in
  let r480 = [R 339] in
  let r481 = S (N N_module_expr) :: r480 in
  let r482 = S (T T_MINUSGREATER) :: r481 in
  let r483 = S (N N_functor_args) :: r482 in
  let r484 = [R 208] in
  let r485 = [R 209] in
  let r486 = S (T T_RPAREN) :: r485 in
  let r487 = S (N N_module_type) :: r486 in
  let r488 = [R 352] in
  let r489 = S (T T_RPAREN) :: r488 in
  let r490 = [R 350] in
  let r491 = S (N N_module_type) :: r490 in
  let r492 = S (T T_MINUSGREATER) :: r491 in
  let r493 = S (N N_functor_args) :: r492 in
  let r494 = S (T T_UIDENT) :: r25 in
  let r495 = [R 784] in
  let r496 = Sub (r196) :: r495 in
  let r497 = S (T T_EQUAL) :: r496 in
  let r498 = Sub (r494) :: r497 in
  let r499 = S (T T_MODULE) :: r498 in
  let r500 = [R 526] in
  let r501 = Sub (r499) :: r500 in
  let r502 = [R 356] in
  let r503 = [R 783] in
  let r504 = Sub (r67) :: r503 in
  let r505 = S (T T_COLONEQUAL) :: r504 in
  let r506 = Sub (r381) :: r505 in
  let r507 = [R 782] in
  let r508 = R 511 :: r507 in
  let r509 = [R 785] in
  let r510 = [R 527] in
  let r511 = [R 355] in
  let r512 = [R 360] in
  let r513 = Sub (r91) :: r512 in
  let r514 = [R 344] in
  let r515 = [R 444] in
  let r516 = S (T T_RPAREN) :: r515 in
  let r517 = [R 627] in
  let r518 = [R 544] in
  let r519 = S (N N_expr) :: r518 in
  let r520 = [R 630] in
  let r521 = S (T T_RBRACKET) :: r520 in
  let r522 = [R 615] in
  let r523 = [R 547] in
  let r524 = R 432 :: r523 in
  let r525 = [R 433] in
  let r526 = [R 553] in
  let r527 = R 432 :: r526 in
  let r528 = R 440 :: r527 in
  let r529 = Sub (r381) :: r528 in
  let r530 = [R 497] in
  let r531 = Sub (r529) :: r530 in
  let r532 = [R 624] in
  let r533 = S (T T_RBRACE) :: r532 in
  let r534 = [R 591] in
  let r535 = [R 590] in
  let r536 = S (T T_GREATERDOT) :: r535 in
  let r537 = [R 147] in
  let r538 = Sub (r41) :: r537 in
  let r539 = R 278 :: r538 in
  let r540 = [R 604] in
  let r541 = S (T T_END) :: r540 in
  let r542 = R 278 :: r541 in
  let r543 = [R 143] in
  let r544 = S (N N_expr) :: r543 in
  let r545 = S (T T_THEN) :: r544 in
  let r546 = Sub (r1) :: r545 in
  let r547 = R 278 :: r546 in
  let r548 = [R 137] in
  let r549 = Sub (r34) :: r548 in
  let r550 = R 278 :: r549 in
  let r551 = [R 522] in
  let r552 = [R 312] in
  let r553 = Sub (r1) :: r552 in
  let r554 = S (T T_MINUSGREATER) :: r553 in
  let r555 = [R 249] in
  let r556 = Sub (r394) :: r555 in
  let r557 = [R 200] in
  let r558 = Sub (r1) :: r557 in
  let r559 = S (T T_MINUSGREATER) :: r558 in
  let r560 = [R 138] in
  let r561 = Sub (r559) :: r560 in
  let r562 = Sub (r556) :: r561 in
  let r563 = R 278 :: r562 in
  let r564 = [R 139] in
  let r565 = Sub (r559) :: r564 in
  let r566 = S (T T_RPAREN) :: r565 in
  let r567 = [R 131] in
  let r568 = S (T T_DONE) :: r567 in
  let r569 = Sub (r1) :: r568 in
  let r570 = S (T T_DO) :: r569 in
  let r571 = Sub (r1) :: r570 in
  let r572 = S (T T_IN) :: r571 in
  let r573 = S (N N_pattern) :: r572 in
  let r574 = R 278 :: r573 in
  let r575 = [R 114] in
  let r576 = S (T T_DOWNTO) :: r575 in
  let r577 = [R 145] in
  let r578 = S (T T_DONE) :: r577 in
  let r579 = Sub (r1) :: r578 in
  let r580 = S (T T_DO) :: r579 in
  let r581 = Sub (r1) :: r580 in
  let r582 = Sub (r576) :: r581 in
  let r583 = Sub (r1) :: r582 in
  let r584 = S (T T_EQUAL) :: r583 in
  let r585 = S (N N_pattern) :: r584 in
  let r586 = R 278 :: r585 in
  let r587 = [R 613] in
  let r588 = [R 623] in
  let r589 = S (T T_RPAREN) :: r588 in
  let r590 = S (T T_LPAREN) :: r589 in
  let r591 = S (T T_DOT) :: r590 in
  let r592 = [R 633] in
  let r593 = S (T T_RPAREN) :: r592 in
  let r594 = S (N N_module_type) :: r593 in
  let r595 = S (T T_COLON) :: r594 in
  let r596 = S (N N_module_expr) :: r595 in
  let r597 = R 278 :: r596 in
  let r598 = [R 264] in
  let r599 = Sub (r1) :: r598 in
  let r600 = S (T T_EQUAL) :: r599 in
  let r601 = [R 146] in
  let r602 = Sub (r41) :: r601 in
  let r603 = R 278 :: r602 in
  let r604 = [R 620] in
  let r605 = [R 596] in
  let r606 = S (T T_RBRACKET) :: r605 in
  let r607 = Sub (r519) :: r606 in
  let r608 = S (T T_LBRACKET) :: r607 in
  let r609 = [R 597] in
  let r610 = S (T T_RPAREN) :: r609 in
  let r611 = Sub (r519) :: r610 in
  let r612 = [R 173] in
  let r613 = [R 239] in
  let r614 = [R 240] in
  let r615 = [R 241] in
  let r616 = [R 619] in
  let r617 = [R 602] in
  let r618 = S (T T_RBRACE) :: r617 in
  let r619 = S (N N_expr) :: r618 in
  let r620 = S (T T_LBRACE) :: r619 in
  let r621 = [R 594] in
  let r622 = S (T T_RPAREN) :: r621 in
  let r623 = Sub (r1) :: r622 in
  let r624 = [R 538] in
  let r625 = [R 123] in
  let r626 = Sub (r1) :: r625 in
  let r627 = [R 175] in
  let r628 = Sub (r1) :: r627 in
  let r629 = [R 163] in
  let r630 = [R 157] in
  let r631 = [R 174] in
  let r632 = [R 559] in
  let r633 = Sub (r1) :: r632 in
  let r634 = [R 160] in
  let r635 = [R 164] in
  let r636 = [R 156] in
  let r637 = [R 159] in
  let r638 = [R 158] in
  let r639 = [R 168] in
  let r640 = [R 162] in
  let r641 = [R 161] in
  let r642 = [R 166] in
  let r643 = [R 155] in
  let r644 = [R 154] in
  let r645 = [R 177] in
  let r646 = [R 153] in
  let r647 = [R 167] in
  let r648 = [R 165] in
  let r649 = [R 169] in
  let r650 = [R 170] in
  let r651 = [R 171] in
  let r652 = [R 539] in
  let r653 = [R 172] in
  let r654 = [R 11] in
  let r655 = R 284 :: r654 in
  let r656 = Sub (r369) :: r655 in
  let r657 = [R 255] in
  let r658 = Sub (r1) :: r657 in
  let r659 = S (T T_EQUAL) :: r658 in
  let r660 = [R 464] in
  let r661 = [R 469] in
  let r662 = [R 474] in
  let r663 = [R 472] in
  let r664 = [R 463] in
  let r665 = [R 595] in
  let r666 = S (T T_RBRACKET) :: r665 in
  let r667 = Sub (r1) :: r666 in
  let r668 = [R 599] in
  let r669 = S (T T_RBRACKET) :: r668 in
  let r670 = Sub (r519) :: r669 in
  let r671 = S (T T_LBRACKET) :: r670 in
  let r672 = [R 600] in
  let r673 = S (T T_RPAREN) :: r672 in
  let r674 = Sub (r519) :: r673 in
  let r675 = [R 601] in
  let r676 = S (T T_RBRACE) :: r675 in
  let r677 = Sub (r519) :: r676 in
  let r678 = [R 238] in
  let r679 = [R 184] in
  let r680 = [R 183] in
  let r681 = [R 598] in
  let r682 = S (T T_RBRACE) :: r681 in
  let r683 = Sub (r519) :: r682 in
  let r684 = [R 185] in
  let r685 = [R 180] in
  let r686 = [R 181] in
  let r687 = [R 182] in
  let r688 = [R 187] in
  let r689 = [R 186] in
  let r690 = [R 188] in
  let r691 = [R 179] in
  let r692 = [R 267] in
  let r693 = [R 617] in
  let r694 = [R 629] in
  let r695 = [R 628] in
  let r696 = [R 632] in
  let r697 = [R 631] in
  let r698 = S (T T_LIDENT) :: r524 in
  let r699 = [R 618] in
  let r700 = S (T T_GREATERRBRACE) :: r699 in
  let r701 = [R 625] in
  let r702 = S (T T_RBRACE) :: r701 in
  let r703 = [R 498] in
  let r704 = Sub (r529) :: r703 in
  let r705 = [R 748] in
  let r706 = [R 746] in
  let r707 = Sub (r69) :: r706 in
  let r708 = [R 747] in
  let r709 = [R 130] in
  let r710 = S (T T_DONE) :: r709 in
  let r711 = Sub (r1) :: r710 in
  let r712 = S (T T_DO) :: r711 in
  let r713 = Sub (r1) :: r712 in
  let r714 = Sub (r576) :: r713 in
  let r715 = [R 203] in
  let r716 = Sub (r559) :: r715 in
  let r717 = S (T T_RPAREN) :: r716 in
  let r718 = [R 201] in
  let r719 = Sub (r1) :: r718 in
  let r720 = S (T T_MINUSGREATER) :: r719 in
  let r721 = [R 202] in
  let r722 = [R 650] in
  let r723 = S (T T_RPAREN) :: r722 in
  let r724 = [R 523] in
  let r725 = [R 142] in
  let r726 = [R 603] in
  let r727 = [R 614] in
  let r728 = [R 626] in
  let r729 = [R 333] in
  let r730 = S (N N_module_expr) :: r729 in
  let r731 = S (T T_EQUAL) :: r730 in
  let r732 = [R 133] in
  let r733 = Sub (r1) :: r732 in
  let r734 = S (T T_IN) :: r733 in
  let r735 = Sub (r731) :: r734 in
  let r736 = Sub (r334) :: r735 in
  let r737 = R 278 :: r736 in
  let r738 = [R 334] in
  let r739 = S (N N_module_expr) :: r738 in
  let r740 = S (T T_EQUAL) :: r739 in
  let r741 = [R 335] in
  let r742 = [R 134] in
  let r743 = Sub (r1) :: r742 in
  let r744 = S (T T_IN) :: r743 in
  let r745 = R 278 :: r744 in
  let r746 = R 211 :: r745 in
  let r747 = Sub (r117) :: r746 in
  let r748 = R 278 :: r747 in
  let r749 = [R 199] in
  let r750 = Sub (r1) :: r749 in
  let r751 = [R 707] in
  let r752 = [R 253] in
  let r753 = Sub (r1) :: r752 in
  let r754 = S (T T_EQUAL) :: r753 in
  let r755 = Sub (r69) :: r754 in
  let r756 = S (T T_DOT) :: r755 in
  let r757 = [R 252] in
  let r758 = Sub (r1) :: r757 in
  let r759 = S (T T_EQUAL) :: r758 in
  let r760 = Sub (r69) :: r759 in
  let r761 = [R 251] in
  let r762 = Sub (r1) :: r761 in
  let r763 = [R 449] in
  let r764 = S (T T_RPAREN) :: r763 in
  let r765 = [R 447] in
  let r766 = S (T T_RPAREN) :: r765 in
  let r767 = [R 448] in
  let r768 = S (T T_RPAREN) :: r767 in
  let r769 = [R 59] in
  let r770 = S (T T_RPAREN) :: r769 in
  let r771 = [R 769] in
  let r772 = Sub (r1) :: r771 in
  let r773 = S (T T_EQUAL) :: r772 in
  let r774 = S (T T_LIDENT) :: r773 in
  let r775 = R 361 :: r774 in
  let r776 = R 278 :: r775 in
  let r777 = [R 45] in
  let r778 = R 284 :: r777 in
  let r779 = [R 770] in
  let r780 = Sub (r1) :: r779 in
  let r781 = S (T T_EQUAL) :: r780 in
  let r782 = S (T T_LIDENT) :: r781 in
  let r783 = R 361 :: r782 in
  let r784 = [R 772] in
  let r785 = Sub (r1) :: r784 in
  let r786 = [R 768] in
  let r787 = Sub (r69) :: r786 in
  let r788 = S (T T_COLON) :: r787 in
  let r789 = [R 771] in
  let r790 = Sub (r1) :: r789 in
  let r791 = [R 322] in
  let r792 = Sub (r424) :: r791 in
  let r793 = S (T T_LIDENT) :: r792 in
  let r794 = R 488 :: r793 in
  let r795 = R 278 :: r794 in
  let r796 = [R 46] in
  let r797 = R 284 :: r796 in
  let r798 = [R 323] in
  let r799 = Sub (r424) :: r798 in
  let r800 = S (T T_LIDENT) :: r799 in
  let r801 = R 488 :: r800 in
  let r802 = [R 482] in
  let r803 = Sub (r69) :: r802 in
  let r804 = [R 325] in
  let r805 = Sub (r1) :: r804 in
  let r806 = S (T T_EQUAL) :: r805 in
  let r807 = [R 327] in
  let r808 = Sub (r1) :: r807 in
  let r809 = S (T T_EQUAL) :: r808 in
  let r810 = Sub (r69) :: r809 in
  let r811 = S (T T_DOT) :: r810 in
  let r812 = [R 483] in
  let r813 = Sub (r69) :: r812 in
  let r814 = [R 321] in
  let r815 = Sub (r803) :: r814 in
  let r816 = S (T T_COLON) :: r815 in
  let r817 = [R 324] in
  let r818 = Sub (r1) :: r817 in
  let r819 = S (T T_EQUAL) :: r818 in
  let r820 = [R 326] in
  let r821 = Sub (r1) :: r820 in
  let r822 = S (T T_EQUAL) :: r821 in
  let r823 = Sub (r69) :: r822 in
  let r824 = S (T T_DOT) :: r823 in
  let r825 = [R 227] in
  let r826 = S (T T_RBRACKET) :: r825 in
  let r827 = Sub (r15) :: r826 in
  let r828 = [R 480] in
  let r829 = [R 481] in
  let r830 = [R 722] in
  let r831 = R 284 :: r830 in
  let r832 = Sub (r731) :: r831 in
  let r833 = Sub (r334) :: r832 in
  let r834 = R 278 :: r833 in
  let r835 = [R 358] in
  let r836 = R 284 :: r835 in
  let r837 = R 434 :: r836 in
  let r838 = Sub (r91) :: r837 in
  let r839 = R 278 :: r838 in
  let r840 = [R 435] in
  let r841 = [R 723] in
  let r842 = R 274 :: r841 in
  let r843 = R 284 :: r842 in
  let r844 = Sub (r731) :: r843 in
  let r845 = [R 275] in
  let r846 = R 274 :: r845 in
  let r847 = R 284 :: r846 in
  let r848 = Sub (r731) :: r847 in
  let r849 = Sub (r334) :: r848 in
  let r850 = [R 195] in
  let r851 = S (T T_RBRACKET) :: r850 in
  let r852 = Sub (r15) :: r851 in
  let r853 = [R 728] in
  let r854 = R 284 :: r853 in
  let r855 = S (N N_module_expr) :: r854 in
  let r856 = R 278 :: r855 in
  let r857 = [R 371] in
  let r858 = S (T T_STRING) :: r857 in
  let r859 = [R 487] in
  let r860 = R 284 :: r859 in
  let r861 = Sub (r858) :: r860 in
  let r862 = S (T T_EQUAL) :: r861 in
  let r863 = Sub (r69) :: r862 in
  let r864 = S (T T_COLON) :: r863 in
  let r865 = Sub (r59) :: r864 in
  let r866 = R 278 :: r865 in
  let r867 = [R 705] in
  let r868 = R 284 :: r867 in
  let r869 = R 278 :: r868 in
  let r870 = Sub (r299) :: r869 in
  let r871 = S (T T_EQUAL) :: r870 in
  let r872 = Sub (r117) :: r871 in
  let r873 = R 278 :: r872 in
  let r874 = [R 560] in
  let r875 = R 284 :: r874 in
  let r876 = R 278 :: r875 in
  let r877 = R 211 :: r876 in
  let r878 = Sub (r117) :: r877 in
  let r879 = R 278 :: r878 in
  let r880 = R 190 :: r879 in
  let r881 = [R 118] in
  let r882 = R 284 :: r881 in
  let r883 = Sub (r299) :: r882 in
  let r884 = S (T T_EQUAL) :: r883 in
  let r885 = R 278 :: r884 in
  let r886 = Sub (r117) :: r885 in
  let r887 = [R 122] in
  let r888 = Sub (r886) :: r887 in
  let r889 = [R 713] in
  let r890 = [R 116] in
  let r891 = Sub (r61) :: r890 in
  let r892 = [R 120] in
  let r893 = Sub (r61) :: r892 in
  let r894 = [R 115] in
  let r895 = Sub (r61) :: r894 in
  let r896 = [R 117] in
  let r897 = R 284 :: r896 in
  let r898 = [R 478] in
  let r899 = [R 287] in
  let r900 = [R 389] in
  let r901 = R 284 :: r900 in
  let r902 = Sub (r196) :: r901 in
  let r903 = R 278 :: r902 in
  let r904 = [R 390] in
  let r905 = R 284 :: r904 in
  let r906 = Sub (r196) :: r905 in
  let r907 = R 278 :: r906 in
  let r908 = [R 336] in
  let r909 = S (N N_module_type) :: r908 in
  let r910 = S (T T_COLON) :: r909 in
  let r911 = [R 572] in
  let r912 = R 284 :: r911 in
  let r913 = Sub (r910) :: r912 in
  let r914 = Sub (r334) :: r913 in
  let r915 = R 278 :: r914 in
  let r916 = [R 348] in
  let r917 = R 284 :: r916 in
  let r918 = [R 575] in
  let r919 = R 276 :: r918 in
  let r920 = R 284 :: r919 in
  let r921 = S (N N_module_type) :: r920 in
  let r922 = S (T T_COLON) :: r921 in
  let r923 = [R 277] in
  let r924 = R 276 :: r923 in
  let r925 = R 284 :: r924 in
  let r926 = S (N N_module_type) :: r925 in
  let r927 = S (T T_COLON) :: r926 in
  let r928 = Sub (r334) :: r927 in
  let r929 = [R 573] in
  let r930 = R 284 :: r929 in
  let r931 = [R 337] in
  let r932 = [R 578] in
  let r933 = R 284 :: r932 in
  let r934 = S (N N_module_type) :: r933 in
  let r935 = R 278 :: r934 in
  let r936 = S (T T_COLON) :: r891 in
  let r937 = Sub (r936) :: r897 in
  let r938 = R 278 :: r937 in
  let r939 = Sub (r117) :: r938 in
  let r940 = [R 563] in
  let r941 = [R 86] in
  let r942 = S (T T_LIDENT) :: r941 in
  let r943 = [R 69] in
  let r944 = Sub (r942) :: r943 in
  let r945 = [R 81] in
  let r946 = Sub (r944) :: r945 in
  let r947 = [R 579] in
  let r948 = R 270 :: r947 in
  let r949 = R 284 :: r948 in
  let r950 = Sub (r946) :: r949 in
  let r951 = S (T T_COLON) :: r950 in
  let r952 = S (T T_LIDENT) :: r951 in
  let r953 = R 196 :: r952 in
  let r954 = R 774 :: r953 in
  let r955 = R 278 :: r954 in
  let r956 = [R 85] in
  let r957 = R 272 :: r956 in
  let r958 = R 284 :: r957 in
  let r959 = Sub (r944) :: r958 in
  let r960 = S (T T_EQUAL) :: r959 in
  let r961 = S (T T_LIDENT) :: r960 in
  let r962 = R 196 :: r961 in
  let r963 = R 774 :: r962 in
  let r964 = R 278 :: r963 in
  let r965 = R 190 :: r964 in
  let r966 = [R 197] in
  let r967 = S (T T_RBRACKET) :: r966 in
  let r968 = [R 72] in
  let r969 = S (T T_END) :: r968 in
  let r970 = R 293 :: r969 in
  let r971 = R 62 :: r970 in
  let r972 = [R 61] in
  let r973 = S (T T_RPAREN) :: r972 in
  let r974 = [R 64] in
  let r975 = R 284 :: r974 in
  let r976 = Sub (r69) :: r975 in
  let r977 = S (T T_COLON) :: r976 in
  let r978 = S (T T_LIDENT) :: r977 in
  let r979 = R 363 :: r978 in
  let r980 = [R 65] in
  let r981 = R 284 :: r980 in
  let r982 = Sub (r803) :: r981 in
  let r983 = S (T T_COLON) :: r982 in
  let r984 = S (T T_LIDENT) :: r983 in
  let r985 = R 490 :: r984 in
  let r986 = [R 63] in
  let r987 = R 284 :: r986 in
  let r988 = Sub (r944) :: r987 in
  let r989 = [R 74] in
  let r990 = Sub (r944) :: r989 in
  let r991 = S (T T_IN) :: r990 in
  let r992 = Sub (r494) :: r991 in
  let r993 = R 278 :: r992 in
  let r994 = [R 75] in
  let r995 = Sub (r944) :: r994 in
  let r996 = S (T T_IN) :: r995 in
  let r997 = Sub (r494) :: r996 in
  let r998 = [R 530] in
  let r999 = Sub (r69) :: r998 in
  let r1000 = [R 70] in
  let r1001 = Sub (r942) :: r1000 in
  let r1002 = S (T T_RBRACKET) :: r1001 in
  let r1003 = Sub (r999) :: r1002 in
  let r1004 = [R 87] in
  let r1005 = S (T T_LIDENT) :: r1004 in
  let r1006 = S (T T_DOT) :: r1005 in
  let r1007 = [R 531] in
  let r1008 = [R 105] in
  let r1009 = Sub (r69) :: r1008 in
  let r1010 = S (T T_EQUAL) :: r1009 in
  let r1011 = Sub (r69) :: r1010 in
  let r1012 = [R 66] in
  let r1013 = R 284 :: r1012 in
  let r1014 = Sub (r1011) :: r1013 in
  let r1015 = [R 67] in
  let r1016 = [R 294] in
  let r1017 = [R 273] in
  let r1018 = R 272 :: r1017 in
  let r1019 = R 284 :: r1018 in
  let r1020 = Sub (r944) :: r1019 in
  let r1021 = S (T T_EQUAL) :: r1020 in
  let r1022 = S (T T_LIDENT) :: r1021 in
  let r1023 = R 196 :: r1022 in
  let r1024 = R 774 :: r1023 in
  let r1025 = [R 83] in
  let r1026 = Sub (r946) :: r1025 in
  let r1027 = S (T T_MINUSGREATER) :: r1026 in
  let r1028 = Sub (r63) :: r1027 in
  let r1029 = [R 84] in
  let r1030 = Sub (r946) :: r1029 in
  let r1031 = [R 82] in
  let r1032 = Sub (r946) :: r1031 in
  let r1033 = S (T T_MINUSGREATER) :: r1032 in
  let r1034 = [R 271] in
  let r1035 = R 270 :: r1034 in
  let r1036 = R 284 :: r1035 in
  let r1037 = Sub (r946) :: r1036 in
  let r1038 = S (T T_COLON) :: r1037 in
  let r1039 = S (T T_LIDENT) :: r1038 in
  let r1040 = R 196 :: r1039 in
  let r1041 = R 774 :: r1040 in
  let r1042 = [R 288] in
  let r1043 = [R 562] in
  let r1044 = [R 567] in
  let r1045 = [R 281] in
  let r1046 = R 280 :: r1045 in
  let r1047 = R 284 :: r1046 in
  let r1048 = R 511 :: r1047 in
  let r1049 = R 749 :: r1048 in
  let r1050 = S (T T_LIDENT) :: r1049 in
  let r1051 = R 754 :: r1050 in
  let r1052 = [R 568] in
  let r1053 = [R 283] in
  let r1054 = R 282 :: r1053 in
  let r1055 = R 284 :: r1054 in
  let r1056 = R 511 :: r1055 in
  let r1057 = Sub (r156) :: r1056 in
  let r1058 = S (T T_COLONEQUAL) :: r1057 in
  let r1059 = S (T T_LIDENT) :: r1058 in
  let r1060 = R 754 :: r1059 in
  let r1061 = [R 77] in
  let r1062 = Sub (r44) :: r1061 in
  let r1063 = [R 35] in
  let r1064 = Sub (r1062) :: r1063 in
  let r1065 = [R 51] in
  let r1066 = Sub (r1064) :: r1065 in
  let r1067 = S (T T_EQUAL) :: r1066 in
  let r1068 = [R 726] in
  let r1069 = R 268 :: r1068 in
  let r1070 = R 284 :: r1069 in
  let r1071 = Sub (r1067) :: r1070 in
  let r1072 = S (T T_LIDENT) :: r1071 in
  let r1073 = R 196 :: r1072 in
  let r1074 = R 774 :: r1073 in
  let r1075 = R 278 :: r1074 in
  let r1076 = [R 80] in
  let r1077 = S (T T_END) :: r1076 in
  let r1078 = R 295 :: r1077 in
  let r1079 = R 60 :: r1078 in
  let r1080 = [R 48] in
  let r1081 = R 284 :: r1080 in
  let r1082 = Sub (r1) :: r1081 in
  let r1083 = [R 43] in
  let r1084 = R 284 :: r1083 in
  let r1085 = R 428 :: r1084 in
  let r1086 = Sub (r1064) :: r1085 in
  let r1087 = [R 44] in
  let r1088 = R 284 :: r1087 in
  let r1089 = R 428 :: r1088 in
  let r1090 = Sub (r1064) :: r1089 in
  let r1091 = [R 76] in
  let r1092 = S (T T_RPAREN) :: r1091 in
  let r1093 = [R 38] in
  let r1094 = Sub (r1064) :: r1093 in
  let r1095 = S (T T_IN) :: r1094 in
  let r1096 = Sub (r494) :: r1095 in
  let r1097 = R 278 :: r1096 in
  let r1098 = [R 259] in
  let r1099 = R 284 :: r1098 in
  let r1100 = Sub (r369) :: r1099 in
  let r1101 = R 495 :: r1100 in
  let r1102 = R 278 :: r1101 in
  let r1103 = [R 39] in
  let r1104 = Sub (r1064) :: r1103 in
  let r1105 = S (T T_IN) :: r1104 in
  let r1106 = Sub (r494) :: r1105 in
  let r1107 = [R 78] in
  let r1108 = Sub (r44) :: r1107 in
  let r1109 = S (T T_RBRACKET) :: r1108 in
  let r1110 = [R 54] in
  let r1111 = Sub (r1064) :: r1110 in
  let r1112 = S (T T_MINUSGREATER) :: r1111 in
  let r1113 = Sub (r556) :: r1112 in
  let r1114 = [R 36] in
  let r1115 = Sub (r1113) :: r1114 in
  let r1116 = [R 37] in
  let r1117 = Sub (r1064) :: r1116 in
  let r1118 = [R 258] in
  let r1119 = R 284 :: r1118 in
  let r1120 = Sub (r369) :: r1119 in
  let r1121 = [R 79] in
  let r1122 = S (T T_RPAREN) :: r1121 in
  let r1123 = [R 429] in
  let r1124 = [R 47] in
  let r1125 = R 284 :: r1124 in
  let r1126 = Sub (r1011) :: r1125 in
  let r1127 = [R 49] in
  let r1128 = [R 296] in
  let r1129 = [R 52] in
  let r1130 = Sub (r1064) :: r1129 in
  let r1131 = S (T T_EQUAL) :: r1130 in
  let r1132 = [R 53] in
  let r1133 = [R 269] in
  let r1134 = R 268 :: r1133 in
  let r1135 = R 284 :: r1134 in
  let r1136 = Sub (r1067) :: r1135 in
  let r1137 = S (T T_LIDENT) :: r1136 in
  let r1138 = R 196 :: r1137 in
  let r1139 = R 774 :: r1138 in
  let r1140 = [R 292] in
  let r1141 = [R 714] in
  let r1142 = [R 718] in
  let r1143 = [R 710] in
  let r1144 = R 289 :: r1143 in
  let r1145 = [R 291] in
  let r1146 = R 289 :: r1145 in
  let r1147 = [R 217] in
  let r1148 = R 284 :: r1147 in
  let r1149 = R 511 :: r1148 in
  let r1150 = [R 606] in
  let r1151 = S (T T_RPAREN) :: r1150 in
  let r1152 = S (N N_module_expr) :: r1151 in
  let r1153 = R 278 :: r1152 in
  let r1154 = [R 607] in
  let r1155 = S (T T_RPAREN) :: r1154 in
  let r1156 = [R 593] in
  let r1157 = [R 126] in
  let r1158 = [R 128] in
  let r1159 = [R 127] in
  let r1160 = [R 223] in
  let r1161 = [R 226] in
  let r1162 = [R 450] in
  let r1163 = [R 451] in
  let r1164 = [R 452] in
  let r1165 = [R 733] in
  let r1166 = [R 742] in
  let r1167 = [R 298] in
  let r1168 = [R 740] in
  let r1169 = S (T T_SEMISEMI) :: r1168 in
  let r1170 = [R 741] in
  let r1171 = [R 300] in
  let r1172 = [R 303] in
  let r1173 = [R 302] in
  let r1174 = [R 301] in
  let r1175 = R 299 :: r1174 in
  let r1176 = [R 763] in
  let r1177 = S (T T_EOF) :: r1176 in
  let r1178 = R 299 :: r1177 in
  let r1179 = [R 762] in
  function
  | 0 | 1751 | 1755 | 1759 | 1763 | 1767 | 1788 -> Nothing
  | 1750 -> One ([R 0])
  | 1754 -> One ([R 1])
  | 1756 -> One ([R 2])
  | 1762 -> One ([R 3])
  | 1766 -> One ([R 4])
  | 1778 -> One ([R 5])
  | 1798 -> One ([R 6])
  | 434 -> One ([R 7])
  | 433 -> One ([R 8])
  | 203 -> One ([R 16])
  | 220 -> One ([R 17])
  | 216 -> One ([R 31])
  | 1585 -> One ([R 40])
  | 1582 -> One ([R 41])
  | 1580 -> One ([R 42])
  | 1621 -> One ([R 50])
  | 1588 -> One ([R 55])
  | 1451 -> One ([R 68])
  | 1430 | 1486 -> One ([R 71])
  | 1433 -> One ([R 73])
  | 505 -> One ([R 89])
  | 73 -> One ([R 90])
  | 504 -> One ([R 91])
  | 178 | 320 -> One ([R 92])
  | 179 -> One ([R 97])
  | 402 -> One ([R 98])
  | 72 -> One ([R 104])
  | 319 -> One ([R 109])
  | 340 -> One ([R 110])
  | 250 -> One ([R 112])
  | 996 -> One ([R 113])
  | 1300 -> One ([R 121])
  | 750 -> One ([R 132])
  | 936 -> One ([R 149])
  | 763 -> One ([R 150])
  | 784 -> One ([R 151])
  | 766 -> One ([R 152])
  | 782 -> One ([R 189])
  | 1 -> One (R 190 :: r7)
  | 62 -> One (R 190 :: r24)
  | 66 -> One (R 190 :: r28)
  | 69 -> One (R 190 :: r39)
  | 76 -> One (R 190 :: r47)
  | 94 -> One (R 190 :: r75)
  | 435 -> One (R 190 :: r313)
  | 436 -> One (R 190 :: r317)
  | 441 -> One (R 190 :: r325)
  | 454 -> One (R 190 :: r338)
  | 471 -> One (R 190 :: r354)
  | 474 -> One (R 190 :: r359)
  | 479 -> One (R 190 :: r374)
  | 498 -> One (R 190 :: r397)
  | 519 -> One (R 190 :: r410)
  | 603 -> One (R 190 :: r479)
  | 683 -> One (R 190 :: r539)
  | 686 -> One (R 190 :: r542)
  | 689 -> One (R 190 :: r547)
  | 692 -> One (R 190 :: r550)
  | 698 -> One (R 190 :: r563)
  | 706 -> One (R 190 :: r574)
  | 711 -> One (R 190 :: r586)
  | 727 -> One (R 190 :: r597)
  | 741 -> One (R 190 :: r603)
  | 1079 -> One (R 190 :: r737)
  | 1094 -> One (R 190 :: r748)
  | 1243 -> One (R 190 :: r834)
  | 1244 -> One (R 190 :: r839)
  | 1270 -> One (R 190 :: r856)
  | 1275 -> One (R 190 :: r866)
  | 1318 -> One (R 190 :: r903)
  | 1319 -> One (R 190 :: r907)
  | 1328 -> One (R 190 :: r915)
  | 1358 -> One (R 190 :: r935)
  | 1371 -> One (R 190 :: r955)
  | 1715 -> One (R 190 :: r1153)
  | 615 -> One ([R 210])
  | 144 | 648 -> One ([R 221])
  | 123 -> One (R 224 :: r86)
  | 127 -> One (R 224 :: r88)
  | 314 -> One ([R 231])
  | 315 -> One ([R 232])
  | 935 -> One ([R 237])
  | 857 -> One ([R 257])
  | 1586 -> One ([R 260])
  | 584 -> One ([R 261])
  | 85 -> One (R 278 :: r51)
  | 156 -> One (R 278 :: r105)
  | 274 -> One (R 278 :: r224)
  | 439 -> One (R 278 :: r320)
  | 467 -> One (R 278 :: r349)
  | 606 -> One (R 278 :: r483)
  | 613 -> One (R 278 :: r493)
  | 832 -> One (R 278 :: r656)
  | 1166 -> One (R 278 :: r783)
  | 1194 -> One (R 278 :: r801)
  | 1258 -> One (R 278 :: r849)
  | 1340 -> One (R 278 :: r928)
  | 1383 -> One (R 278 :: r971)
  | 1389 -> One (R 278 :: r979)
  | 1400 -> One (R 278 :: r985)
  | 1411 -> One (R 278 :: r988)
  | 1416 -> One (R 278 :: r997)
  | 1440 -> One (R 278 :: r1014)
  | 1456 -> One (R 278 :: r1024)
  | 1493 -> One (R 278 :: r1041)
  | 1514 -> One (R 278 :: r1051)
  | 1524 -> One (R 278 :: r1060)
  | 1547 -> One (R 278 :: r1079)
  | 1550 -> One (R 278 :: r1082)
  | 1554 -> One (R 278 :: r1086)
  | 1555 -> One (R 278 :: r1090)
  | 1566 -> One (R 278 :: r1106)
  | 1574 -> One (R 278 :: r1115)
  | 1613 -> One (R 278 :: r1126)
  | 1633 -> One (R 278 :: r1139)
  | 1513 -> One (R 280 :: r1044)
  | 1655 -> One (R 280 :: r1142)
  | 1523 -> One (R 282 :: r1052)
  | 386 -> One (R 284 :: r297)
  | 1449 -> One (R 284 :: r1015)
  | 1511 -> One (R 284 :: r1043)
  | 1619 -> One (R 284 :: r1127)
  | 1653 -> One (R 284 :: r1141)
  | 1660 -> One (R 284 :: r1144)
  | 1680 -> One (R 284 :: r1146)
  | 1783 -> One (R 284 :: r1169)
  | 1794 -> One (R 284 :: r1175)
  | 1799 -> One (R 284 :: r1178)
  | 1317 -> One (R 286 :: r899)
  | 1504 -> One (R 286 :: r1042)
  | 432 -> One (R 289 :: r309)
  | 1643 -> One (R 289 :: r1140)
  | 1452 -> One (R 293 :: r1016)
  | 1622 -> One (R 295 :: r1128)
  | 1781 -> One (R 297 :: r1167)
  | 1789 -> One (R 299 :: r1171)
  | 1790 -> One (R 299 :: r1172)
  | 1791 -> One (R 299 :: r1173)
  | 558 -> One ([R 305])
  | 562 -> One ([R 307])
  | 773 -> One ([R 309])
  | 858 -> One ([R 310])
  | 1040 -> One ([R 313])
  | 277 -> One ([R 314])
  | 280 -> One ([R 315])
  | 279 -> One ([R 317])
  | 278 -> One ([R 319])
  | 276 -> One ([R 320])
  | 659 -> One ([R 340])
  | 669 -> One ([R 341])
  | 670 -> One ([R 342])
  | 668 -> One ([R 343])
  | 671 -> One ([R 345])
  | 458 | 1331 -> One ([R 346])
  | 645 -> One ([R 353])
  | 619 -> One ([R 354])
  | 651 -> One ([R 357])
  | 650 -> One ([R 359])
  | 304 | 1180 -> One ([R 362])
  | 1393 -> One ([R 364])
  | 1391 -> One ([R 365])
  | 1394 -> One ([R 366])
  | 1392 -> One ([R 367])
  | 595 -> One ([R 370])
  | 1283 -> One ([R 372])
  | 355 -> One ([R 373])
  | 345 -> One ([R 374])
  | 368 -> One ([R 375])
  | 346 -> One ([R 376])
  | 367 -> One ([R 377])
  | 362 -> One ([R 378])
  | 90 | 98 -> One ([R 391])
  | 106 | 736 -> One ([R 392])
  | 134 -> One ([R 393])
  | 122 -> One ([R 395])
  | 126 -> One ([R 397])
  | 130 -> One ([R 399])
  | 113 -> One ([R 400])
  | 133 | 958 -> One ([R 401])
  | 112 -> One ([R 402])
  | 111 -> One ([R 403])
  | 110 -> One ([R 404])
  | 109 -> One ([R 405])
  | 108 -> One ([R 406])
  | 101 | 453 | 726 -> One ([R 407])
  | 100 | 725 -> One ([R 408])
  | 99 -> One ([R 409])
  | 105 | 735 | 1027 -> One ([R 410])
  | 104 | 734 -> One ([R 411])
  | 88 -> One ([R 412])
  | 102 -> One ([R 413])
  | 115 -> One ([R 414])
  | 107 -> One ([R 415])
  | 114 -> One ([R 416])
  | 103 -> One ([R 417])
  | 132 -> One ([R 418])
  | 135 -> One ([R 419])
  | 131 -> One ([R 421])
  | 237 -> One ([R 422])
  | 236 -> One (R 423 :: r210)
  | 191 -> One (R 424 :: r177)
  | 192 -> One ([R 425])
  | 559 -> One (R 426 :: r420)
  | 560 -> One ([R 427])
  | 983 -> One ([R 441])
  | 150 -> One ([R 442])
  | 534 -> One ([R 454])
  | 528 -> One ([R 455])
  | 529 -> One ([R 457])
  | 527 | 737 -> One ([R 465])
  | 850 -> One ([R 470])
  | 852 -> One ([R 473])
  | 590 -> One ([R 475])
  | 1539 -> One ([R 479])
  | 391 | 1218 -> One ([R 489])
  | 1404 -> One ([R 491])
  | 1402 -> One ([R 492])
  | 1405 -> One ([R 493])
  | 1403 -> One ([R 494])
  | 1595 -> One (R 495 :: r1120)
  | 482 -> One ([R 496])
  | 343 -> One ([R 499])
  | 344 -> One ([R 500])
  | 342 -> One ([R 501])
  | 415 -> One ([R 503])
  | 414 -> One ([R 504])
  | 416 -> One ([R 505])
  | 411 -> One ([R 506])
  | 412 -> One ([R 507])
  | 1694 -> One ([R 509])
  | 1692 -> One ([R 510])
  | 652 -> One ([R 513])
  | 616 -> One ([R 514])
  | 938 -> One ([R 515])
  | 937 -> One ([R 516])
  | 265 -> One ([R 518])
  | 229 -> One ([R 542])
  | 872 -> One ([R 545])
  | 873 -> One ([R 546])
  | 1063 -> One ([R 548])
  | 1064 -> One ([R 549])
  | 553 -> One ([R 551])
  | 554 -> One ([R 552])
  | 986 -> One ([R 554])
  | 987 -> One ([R 555])
  | 787 -> One ([R 557])
  | 791 -> One ([R 558])
  | 1534 -> One ([R 564])
  | 1503 -> One ([R 565])
  | 1506 -> One ([R 566])
  | 1505 -> One ([R 571])
  | 1509 -> One ([R 574])
  | 1508 -> One ([R 576])
  | 1507 -> One ([R 577])
  | 1535 -> One ([R 580])
  | 451 -> One ([R 583])
  | 448 -> One ([R 585])
  | 717 -> One ([R 608])
  | 769 -> One ([R 609])
  | 768 | 783 -> One ([R 610])
  | 720 | 765 -> One ([R 611])
  | 880 | 932 -> One ([R 616])
  | 767 -> One ([R 621])
  | 506 -> One ([R 634])
  | 509 -> One ([R 637])
  | 510 -> One ([R 641])
  | 526 -> One ([R 643])
  | 514 -> One ([R 644])
  | 555 -> One ([R 646])
  | 525 -> One ([R 651])
  | 28 -> One ([R 652])
  | 8 -> One ([R 653])
  | 53 -> One ([R 655])
  | 52 -> One ([R 656])
  | 51 -> One ([R 657])
  | 50 -> One ([R 658])
  | 49 -> One ([R 659])
  | 48 -> One ([R 660])
  | 47 -> One ([R 661])
  | 46 -> One ([R 662])
  | 45 -> One ([R 663])
  | 44 -> One ([R 664])
  | 43 -> One ([R 665])
  | 42 -> One ([R 666])
  | 41 -> One ([R 667])
  | 40 -> One ([R 668])
  | 39 -> One ([R 669])
  | 38 -> One ([R 670])
  | 37 -> One ([R 671])
  | 36 -> One ([R 672])
  | 35 -> One ([R 673])
  | 34 -> One ([R 674])
  | 33 -> One ([R 675])
  | 32 -> One ([R 676])
  | 31 -> One ([R 677])
  | 30 -> One ([R 678])
  | 29 -> One ([R 679])
  | 27 -> One ([R 680])
  | 26 -> One ([R 681])
  | 25 -> One ([R 682])
  | 24 -> One ([R 683])
  | 23 -> One ([R 684])
  | 22 -> One ([R 685])
  | 21 -> One ([R 686])
  | 20 -> One ([R 687])
  | 19 -> One ([R 688])
  | 18 -> One ([R 689])
  | 17 -> One ([R 690])
  | 16 -> One ([R 691])
  | 15 -> One ([R 692])
  | 14 -> One ([R 693])
  | 13 -> One ([R 694])
  | 12 -> One ([R 695])
  | 11 -> One ([R 696])
  | 10 -> One ([R 697])
  | 9 -> One ([R 698])
  | 7 -> One ([R 699])
  | 6 -> One ([R 700])
  | 5 -> One ([R 701])
  | 4 -> One ([R 702])
  | 3 -> One ([R 703])
  | 1646 -> One ([R 704])
  | 1666 -> One ([R 709])
  | 1650 | 1665 -> One ([R 711])
  | 1652 | 1667 -> One ([R 712])
  | 1657 -> One ([R 715])
  | 1647 -> One ([R 716])
  | 1642 -> One ([R 717])
  | 1645 -> One ([R 721])
  | 1649 -> One ([R 724])
  | 1648 -> One ([R 725])
  | 1658 -> One ([R 727])
  | 470 -> One ([R 729])
  | 469 -> One ([R 730])
  | 1771 -> One ([R 734])
  | 1772 -> One ([R 735])
  | 1774 -> One ([R 736])
  | 1775 -> One ([R 737])
  | 1773 -> One ([R 738])
  | 1770 -> One ([R 739])
  | 1777 -> One ([R 743])
  | 206 -> One ([R 745])
  | 622 -> One (R 754 :: r506)
  | 421 -> One ([R 755])
  | 161 -> One ([R 760])
  | 163 -> One ([R 761])
  | 718 -> One ([R 766])
  | 994 -> One ([R 767])
  | 1375 -> One ([R 775])
  | 1178 -> One ([R 776])
  | 1181 -> One ([R 777])
  | 1179 -> One ([R 778])
  | 1216 -> One ([R 779])
  | 1219 -> One ([R 780])
  | 1217 -> One ([R 781])
  | 625 -> One ([R 786])
  | 626 -> One ([R 787])
  | 973 -> One (S (T T_WITH) :: r704)
  | 636 | 1776 -> One (S (T T_UIDENT) :: r50)
  | 212 -> One (S (T T_UIDENT) :: r199)
  | 462 -> One (S (T T_TYPE) :: r344)
  | 592 -> One (S (T T_TYPE) :: r461)
  | 328 -> One (S (T T_STAR) :: r259)
  | 1307 -> One (S (T T_STAR) :: r893)
  | 1779 -> One (S (T T_SEMISEMI) :: r1166)
  | 1786 -> One (S (T T_SEMISEMI) :: r1170)
  | 396 -> One (S (T T_RPAREN) :: r54)
  | 181 | 321 -> One (S (T T_RPAREN) :: r159)
  | 288 -> One (S (T T_RPAREN) :: r226)
  | 290 -> One (S (T T_RPAREN) :: r228)
  | 297 -> One (S (T T_RPAREN) :: r231)
  | 397 -> One (S (T T_RPAREN) :: r302)
  | 517 -> One (S (T T_RPAREN) :: r407)
  | 540 -> One (S (T T_RPAREN) :: r416)
  | 608 -> One (S (T T_RPAREN) :: r484)
  | 661 -> One (S (T T_RPAREN) :: r514)
  | 959 -> One (S (T T_RPAREN) :: r693)
  | 1725 -> One (S (T T_RPAREN) :: r1156)
  | 194 -> One (S (T T_RBRACKET) :: r178)
  | 301 | 322 -> One (S (T T_RBRACKET) :: r233)
  | 399 -> One (S (T T_RBRACKET) :: r303)
  | 965 -> One (S (T T_RBRACKET) :: r696)
  | 967 -> One (S (T T_RBRACKET) :: r697)
  | 243 -> One (S (T T_QUOTE) :: r213)
  | 1414 -> One (S (T T_OPEN) :: r993)
  | 1558 -> One (S (T T_OPEN) :: r1097)
  | 151 -> One (S (T T_MODULE) :: r100)
  | 334 -> One (S (T T_MINUSGREATER) :: r262)
  | 1310 -> One (S (T T_MINUSGREATER) :: r895)
  | 1478 -> One (S (T T_MINUSGREATER) :: r1030)
  | 116 -> One (S (T T_LPAREN) :: r83)
  | 403 -> One (S (T T_LPAREN) :: r306)
  | 147 -> One (S (T T_LIDENT) :: r95)
  | 305 -> One (S (T T_LIDENT) :: r249)
  | 567 -> One (S (T T_LIDENT) :: r426)
  | 575 -> One (S (T T_LIDENT) :: r432)
  | 751 -> One (S (T T_LIDENT) :: r613)
  | 753 -> One (S (T T_LIDENT) :: r614)
  | 757 -> One (S (T T_LIDENT) :: r616)
  | 1182 -> One (S (T T_LIDENT) :: r788)
  | 1220 -> One (S (T T_LIDENT) :: r816)
  | 1605 -> One (S (T T_LIDENT) :: r1123)
  | 446 -> One (S (T T_INT) :: r329)
  | 449 -> One (S (T T_INT) :: r330)
  | 770 -> One (S (T T_IN) :: r626)
  | 774 -> One (S (T T_IN) :: r628)
  | 1578 -> One (S (T T_IN) :: r1117)
  | 676 -> One (S (T T_GREATERRBRACE) :: r522)
  | 1066 -> One (S (T T_GREATERRBRACE) :: r727)
  | 186 -> One (S (T T_GREATER) :: r164)
  | 283 -> One (S (T T_GREATER) :: r225)
  | 1108 -> One (S (T T_EQUAL) :: r750)
  | 1132 -> One (S (T T_EQUAL) :: r762)
  | 1172 -> One (S (T T_EQUAL) :: r785)
  | 1190 -> One (S (T T_EQUAL) :: r790)
  | 1748 -> One (S (T T_EOF) :: r1160)
  | 1752 -> One (S (T T_EOF) :: r1161)
  | 1757 -> One (S (T T_EOF) :: r1162)
  | 1760 -> One (S (T T_EOF) :: r1163)
  | 1764 -> One (S (T T_EOF) :: r1164)
  | 1803 -> One (S (T T_EOF) :: r1179)
  | 1053 -> One (S (T T_END) :: r726)
  | 118 -> One (S (T T_DOTDOT) :: r84)
  | 180 -> One (S (T T_DOTDOT) :: r158)
  | 356 -> One (S (T T_DOTDOT) :: r266)
  | 357 -> One (S (T T_DOTDOT) :: r267)
  | 80 -> One (S (T T_DOT) :: r49)
  | 208 -> One (S (T T_DOT) :: r195)
  | 267 -> One (S (T T_DOT) :: r222)
  | 488 | 866 | 915 -> One (S (T T_DOT) :: r389)
  | 646 -> One (S (T T_DOT) :: r513)
  | 1127 -> One (S (T T_DOT) :: r760)
  | 1205 -> One (S (T T_DOT) :: r813)
  | 187 -> One (S (T T_COLON) :: r169)
  | 610 -> One (S (T T_COLON) :: r487)
  | 1472 -> One (S (T T_COLON) :: r1028)
  | 484 -> One (S (T T_BARRBRACKET) :: r375)
  | 564 -> One (S (T T_BARRBRACKET) :: r421)
  | 674 -> One (S (T T_BARRBRACKET) :: r517)
  | 961 -> One (S (T T_BARRBRACKET) :: r694)
  | 963 -> One (S (T T_BARRBRACKET) :: r695)
  | 1071 -> One (S (T T_BARRBRACKET) :: r728)
  | 254 -> One (S (T T_BAR) :: r216)
  | 444 -> One (S (N N_pattern) :: r327)
  | 701 | 1015 -> One (S (N N_pattern) :: r332)
  | 497 -> One (S (N N_pattern) :: r391)
  | 530 -> One (S (N N_pattern) :: r413)
  | 532 -> One (S (N N_pattern) :: r414)
  | 542 -> One (S (N N_pattern) :: r417)
  | 544 -> One (S (N N_pattern) :: r418)
  | 842 -> One (S (N N_pattern) :: r660)
  | 844 -> One (S (N N_pattern) :: r661)
  | 846 -> One (S (N N_pattern) :: r662)
  | 853 -> One (S (N N_pattern) :: r664)
  | 1239 -> One (S (N N_pattern) :: r828)
  | 461 -> One (S (N N_module_type) :: r340)
  | 612 -> One (S (N N_module_type) :: r489)
  | 643 -> One (S (N N_module_type) :: r511)
  | 665 -> One (S (N N_module_type) :: r516)
  | 1085 -> One (S (N N_module_type) :: r740)
  | 1147 -> One (S (N N_module_type) :: r764)
  | 1150 -> One (S (N N_module_type) :: r766)
  | 1153 -> One (S (N N_module_type) :: r768)
  | 1248 -> One (S (N N_module_type) :: r840)
  | 1720 -> One (S (N N_module_type) :: r1155)
  | 466 -> One (S (N N_module_expr) :: r346)
  | 583 -> One (S (N N_let_pattern) :: r452)
  | 478 -> One (S (N N_expr) :: r360)
  | 678 -> One (S (N N_expr) :: r525)
  | 682 -> One (S (N N_expr) :: r536)
  | 749 -> One (S (N N_expr) :: r612)
  | 764 -> One (S (N N_expr) :: r624)
  | 778 -> One (S (N N_expr) :: r629)
  | 780 -> One (S (N N_expr) :: r630)
  | 785 -> One (S (N N_expr) :: r631)
  | 792 -> One (S (N N_expr) :: r634)
  | 794 -> One (S (N N_expr) :: r635)
  | 796 -> One (S (N N_expr) :: r636)
  | 798 -> One (S (N N_expr) :: r637)
  | 800 -> One (S (N N_expr) :: r638)
  | 802 -> One (S (N N_expr) :: r639)
  | 804 -> One (S (N N_expr) :: r640)
  | 806 -> One (S (N N_expr) :: r641)
  | 808 -> One (S (N N_expr) :: r642)
  | 810 -> One (S (N N_expr) :: r643)
  | 812 -> One (S (N N_expr) :: r644)
  | 814 -> One (S (N N_expr) :: r645)
  | 816 -> One (S (N N_expr) :: r646)
  | 818 -> One (S (N N_expr) :: r647)
  | 820 -> One (S (N N_expr) :: r648)
  | 822 -> One (S (N N_expr) :: r649)
  | 824 -> One (S (N N_expr) :: r650)
  | 826 -> One (S (N N_expr) :: r651)
  | 828 -> One (S (N N_expr) :: r652)
  | 830 -> One (S (N N_expr) :: r653)
  | 887 -> One (S (N N_expr) :: r679)
  | 892 -> One (S (N N_expr) :: r680)
  | 897 -> One (S (N N_expr) :: r684)
  | 903 -> One (S (N N_expr) :: r685)
  | 908 -> One (S (N N_expr) :: r686)
  | 913 -> One (S (N N_expr) :: r687)
  | 920 -> One (S (N N_expr) :: r688)
  | 925 -> One (S (N N_expr) :: r689)
  | 930 -> One (S (N N_expr) :: r690)
  | 933 -> One (S (N N_expr) :: r691)
  | 1050 -> One (S (N N_expr) :: r725)
  | 578 -> One (Sub (r1) :: r436)
  | 697 -> One (Sub (r1) :: r554)
  | 1007 -> One (Sub (r1) :: r714)
  | 1241 -> One (Sub (r1) :: r829)
  | 1733 -> One (Sub (r1) :: r1158)
  | 1735 -> One (Sub (r1) :: r1159)
  | 2 -> One (Sub (r11) :: r12)
  | 56 -> One (Sub (r11) :: r13)
  | 60 -> One (Sub (r11) :: r18)
  | 92 -> One (Sub (r11) :: r58)
  | 372 -> One (Sub (r11) :: r277)
  | 788 -> One (Sub (r11) :: r633)
  | 1237 -> One (Sub (r11) :: r827)
  | 1268 -> One (Sub (r11) :: r852)
  | 1559 -> One (Sub (r11) :: r1102)
  | 695 -> One (Sub (r32) :: r551)
  | 1044 -> One (Sub (r32) :: r724)
  | 1731 -> One (Sub (r34) :: r1157)
  | 75 -> One (Sub (r41) :: r42)
  | 681 -> One (Sub (r41) :: r534)
  | 716 -> One (Sub (r41) :: r587)
  | 745 -> One (Sub (r41) :: r604)
  | 755 -> One (Sub (r41) :: r615)
  | 881 -> One (Sub (r41) :: r678)
  | 198 -> One (Sub (r44) :: r189)
  | 218 -> One (Sub (r44) :: r200)
  | 292 -> One (Sub (r44) :: r229)
  | 546 -> One (Sub (r59) :: r419)
  | 848 -> One (Sub (r59) :: r663)
  | 207 -> One (Sub (r61) :: r193)
  | 226 -> One (Sub (r61) :: r204)
  | 333 -> One (Sub (r61) :: r260)
  | 1019 -> One (Sub (r61) :: r720)
  | 221 -> One (Sub (r63) :: r203)
  | 1480 -> One (Sub (r63) :: r1033)
  | 205 -> One (Sub (r65) :: r192)
  | 240 -> One (Sub (r67) :: r211)
  | 629 -> One (Sub (r67) :: r508)
  | 295 -> One (Sub (r69) :: r230)
  | 299 -> One (Sub (r69) :: r232)
  | 382 -> One (Sub (r69) :: r296)
  | 494 -> One (Sub (r69) :: r390)
  | 570 -> One (Sub (r69) :: r431)
  | 585 -> One (Sub (r69) :: r453)
  | 738 -> One (Sub (r69) :: r600)
  | 835 -> One (Sub (r69) :: r659)
  | 977 -> One (Sub (r69) :: r705)
  | 981 -> One (Sub (r69) :: r708)
  | 1030 -> One (Sub (r69) :: r723)
  | 1161 -> One (Sub (r69) :: r770)
  | 1385 -> One (Sub (r69) :: r973)
  | 1427 -> One (Sub (r69) :: r1007)
  | 167 -> One (Sub (r91) :: r153)
  | 268 -> One (Sub (r91) :: r223)
  | 1768 -> One (Sub (r91) :: r1165)
  | 1316 -> One (Sub (r102) :: r898)
  | 502 -> One (Sub (r115) :: r399)
  | 173 -> One (Sub (r148) :: r154)
  | 164 -> One (Sub (r150) :: r152)
  | 1377 -> One (Sub (r150) :: r967)
  | 177 -> One (Sub (r156) :: r157)
  | 369 -> One (Sub (r156) :: r274)
  | 1697 -> One (Sub (r156) :: r1149)
  | 233 -> One (Sub (r172) :: r205)
  | 196 -> One (Sub (r174) :: r180)
  | 200 -> One (Sub (r174) :: r191)
  | 197 -> One (Sub (r186) :: r188)
  | 209 -> One (Sub (r196) :: r198)
  | 637 -> One (Sub (r196) :: r509)
  | 1332 -> One (Sub (r196) :: r917)
  | 262 -> One (Sub (r218) :: r220)
  | 303 -> One (Sub (r241) :: r243)
  | 325 -> One (Sub (r241) :: r257)
  | 350 -> One (Sub (r241) :: r265)
  | 358 -> One (Sub (r241) :: r269)
  | 363 -> One (Sub (r241) :: r271)
  | 324 -> One (Sub (r254) :: r255)
  | 395 -> One (Sub (r299) :: r301)
  | 418 -> One (Sub (r299) :: r308)
  | 1254 -> One (Sub (r334) :: r844)
  | 1335 -> One (Sub (r334) :: r922)
  | 955 -> One (Sub (r369) :: r692)
  | 486 -> One (Sub (r385) :: r387)
  | 522 -> One (Sub (r394) :: r412)
  | 598 -> One (Sub (r394) :: r462)
  | 511 -> One (Sub (r402) :: r403)
  | 566 -> One (Sub (r424) :: r425)
  | 580 -> One (Sub (r424) :: r446)
  | 568 -> One (Sub (r428) :: r430)
  | 576 -> One (Sub (r428) :: r435)
  | 579 -> One (Sub (r442) :: r445)
  | 581 -> One (Sub (r448) :: r449)
  | 702 -> One (Sub (r455) :: r566)
  | 1016 -> One (Sub (r455) :: r717)
  | 1121 -> One (Sub (r455) :: r756)
  | 1199 -> One (Sub (r455) :: r811)
  | 1227 -> One (Sub (r455) :: r824)
  | 1112 -> One (Sub (r457) :: r751)
  | 1349 -> One (Sub (r494) :: r930)
  | 641 -> One (Sub (r499) :: r510)
  | 621 -> One (Sub (r501) :: r502)
  | 679 -> One (Sub (r531) :: r533)
  | 972 -> One (Sub (r531) :: r702)
  | 1024 -> One (Sub (r559) :: r721)
  | 969 -> One (Sub (r698) :: r700)
  | 1092 -> One (Sub (r731) :: r741)
  | 1165 -> One (Sub (r776) :: r778)
  | 1193 -> One (Sub (r795) :: r797)
  | 1198 -> One (Sub (r803) :: r806)
  | 1226 -> One (Sub (r803) :: r819)
  | 1297 -> One (Sub (r888) :: r889)
  | 1356 -> One (Sub (r910) :: r931)
  | 1367 -> One (Sub (r939) :: r940)
  | 1601 -> One (Sub (r946) :: r1122)
  | 1625 -> One (Sub (r946) :: r1131)
  | 1570 -> One (Sub (r999) :: r1109)
  | 1557 -> One (Sub (r1064) :: r1092)
  | 1629 -> One (Sub (r1067) :: r1132)
  | 777 -> One (r0)
  | 1747 -> One (r2)
  | 1746 -> One (r3)
  | 1745 -> One (r4)
  | 1744 -> One (r5)
  | 1743 -> One (r6)
  | 59 -> One (r7)
  | 54 -> One (r8)
  | 55 -> One (r10)
  | 58 -> One (r12)
  | 57 -> One (r13)
  | 1659 -> One (r14)
  | 1742 -> One (r16)
  | 1741 -> One (r17)
  | 61 -> One (r18)
  | 1740 -> One (r19)
  | 1739 -> One (r20)
  | 1738 -> One (r21)
  | 1737 -> One (r22)
  | 64 -> One (r23)
  | 63 -> One (r24)
  | 65 -> One (r25)
  | 1730 -> One (r26)
  | 68 -> One (r27)
  | 67 -> One (r28)
  | 1041 -> One (r29)
  | 1039 -> One (r30)
  | 696 -> One (r31)
  | 1046 -> One (r33)
  | 1729 -> One (r35)
  | 1728 -> One (r36)
  | 1727 -> One (r37)
  | 71 -> One (r38)
  | 70 -> One (r39)
  | 74 -> One (r40)
  | 1714 -> One (r42)
  | 79 -> One (r43)
  | 84 -> One (r45)
  | 78 -> One (r46)
  | 77 -> One (r47)
  | 83 -> One (r48)
  | 81 -> One (r49)
  | 82 -> One (r50)
  | 86 -> One (r51)
  | 1724 -> One (r52)
  | 1723 -> One (r53)
  | 89 -> One (r54)
  | 91 | 477 | 680 | 993 -> One (r55)
  | 1713 -> One (r56)
  | 1712 -> One (r57)
  | 93 -> One (r58)
  | 141 -> One (r60)
  | 225 -> One (r62)
  | 204 -> One (r64)
  | 241 -> One (r66)
  | 251 -> One (r68)
  | 1711 -> One (r70)
  | 1710 -> One (r71)
  | 140 -> One (r72)
  | 139 -> One (r73)
  | 96 -> One (r74)
  | 95 -> One (r75)
  | 136 -> One (r76)
  | 138 -> One (r78)
  | 137 -> One (r79)
  | 97 -> One (r80)
  | 121 -> One (r81)
  | 120 -> One (r82)
  | 117 -> One (r83)
  | 119 -> One (r84)
  | 125 -> One (r85)
  | 124 -> One (r86)
  | 129 -> One (r87)
  | 128 -> One (r88)
  | 142 | 155 -> One (r89)
  | 145 -> One (r90)
  | 146 -> One (r92)
  | 143 -> One (r93)
  | 149 -> One (r94)
  | 148 -> One (r95)
  | 1709 -> One (r96)
  | 1708 -> One (r97)
  | 154 -> One (r98)
  | 153 -> One (r99)
  | 152 -> One (r100)
  | 1538 -> One (r101)
  | 1707 -> One (r103)
  | 1706 -> One (r104)
  | 157 -> One (r105)
  | 426 -> One (r106)
  | 425 -> One (r107)
  | 424 -> One (r108)
  | 185 -> One (r114)
  | 317 -> One (r116)
  | 349 -> One (r118)
  | 348 -> One (r119)
  | 347 | 417 -> One (r120)
  | 1693 -> One (r122)
  | 1705 -> One (r124)
  | 1704 -> One (r125)
  | 1703 -> One (r126)
  | 1702 -> One (r127)
  | 1701 -> One (r128)
  | 388 -> One (r132)
  | 381 -> One (r133)
  | 380 -> One (r134)
  | 1691 -> One (r138)
  | 1690 -> One (r139)
  | 1689 -> One (r140)
  | 1688 -> One (r141)
  | 1687 -> One (r142)
  | 166 -> One (r144)
  | 169 -> One (r146)
  | 165 -> One (r147)
  | 170 -> One (r149)
  | 172 -> One (r151)
  | 171 -> One (r152)
  | 168 -> One (r153)
  | 174 -> One (r154)
  | 353 -> One (r155)
  | 354 -> One (r157)
  | 318 -> One (r158)
  | 182 -> One (r159)
  | 287 -> One (r160)
  | 286 -> One (r161)
  | 285 -> One (r162)
  | 184 -> One (r163)
  | 282 -> One (r164)
  | 281 -> One (r165)
  | 273 -> One (r167)
  | 272 -> One (r168)
  | 188 -> One (r169)
  | 249 -> One (r171)
  | 230 -> One (r173)
  | 261 -> One (r175)
  | 260 -> One (r176)
  | 193 -> One (r177)
  | 195 -> One (r178)
  | 259 -> One (r179)
  | 258 -> One (r180)
  | 202 -> One (r181)
  | 201 -> One (r182)
  | 248 -> One (r184)
  | 235 -> One (r185)
  | 253 -> One (r187)
  | 252 -> One (r188)
  | 199 -> One (r189)
  | 232 -> One (r190)
  | 231 -> One (r191)
  | 228 -> One (r192)
  | 217 -> One (r193)
  | 215 -> One (r194)
  | 214 -> One (r195)
  | 211 -> One (r197)
  | 210 -> One (r198)
  | 213 -> One (r199)
  | 219 -> One (r200)
  | 224 -> One (r201)
  | 223 -> One (r202)
  | 222 -> One (r203)
  | 227 -> One (r204)
  | 234 -> One (r205)
  | 247 -> One (r206)
  | 246 -> One (r208)
  | 239 -> One (r209)
  | 238 -> One (r210)
  | 242 -> One (r211)
  | 245 -> One (r212)
  | 244 -> One (r213)
  | 257 -> One (r214)
  | 256 -> One (r215)
  | 255 -> One (r216)
  | 266 -> One (r217)
  | 264 -> One (r219)
  | 263 -> One (r220)
  | 271 -> One (r221)
  | 270 -> One (r222)
  | 269 -> One (r223)
  | 275 -> One (r224)
  | 284 -> One (r225)
  | 289 -> One (r226)
  | 294 -> One (r227)
  | 291 -> One (r228)
  | 293 -> One (r229)
  | 296 -> One (r230)
  | 298 -> One (r231)
  | 300 -> One (r232)
  | 302 -> One (r233)
  | 316 -> One (r240)
  | 313 -> One (r242)
  | 312 -> One (r243)
  | 311 -> One (r244)
  | 310 -> One (r245)
  | 309 -> One (r246)
  | 308 -> One (r247)
  | 307 -> One (r248)
  | 306 -> One (r249)
  | 339 -> One (r250)
  | 338 -> One (r251)
  | 323 | 394 -> One (r252)
  | 332 -> One (r253)
  | 331 -> One (r255)
  | 327 -> One (r256)
  | 326 -> One (r257)
  | 330 -> One (r258)
  | 329 -> One (r259)
  | 337 -> One (r260)
  | 336 -> One (r261)
  | 335 -> One (r262)
  | 341 | 393 -> One (r263)
  | 352 -> One (r264)
  | 351 -> One (r265)
  | 366 -> One (r266)
  | 361 -> One (r267)
  | 360 -> One (r268)
  | 359 -> One (r269)
  | 365 -> One (r270)
  | 364 -> One (r271)
  | 1686 -> One (r272)
  | 371 -> One (r273)
  | 370 -> One (r274)
  | 1685 -> One (r275)
  | 1684 -> One (r276)
  | 373 -> One (r277)
  | 413 -> One (r278)
  | 431 -> One (r280)
  | 430 -> One (r281)
  | 429 -> One (r282)
  | 428 -> One (r283)
  | 427 -> One (r284)
  | 410 -> One (r288)
  | 409 -> One (r289)
  | 392 -> One (r290)
  | 390 -> One (r291)
  | 389 -> One (r292)
  | 385 -> One (r294)
  | 384 -> One (r295)
  | 383 -> One (r296)
  | 387 -> One (r297)
  | 401 -> One (r298)
  | 408 -> One (r300)
  | 407 -> One (r301)
  | 398 -> One (r302)
  | 400 -> One (r303)
  | 406 -> One (r304)
  | 405 -> One (r305)
  | 404 -> One (r306)
  | 420 -> One (r307)
  | 419 -> One (r308)
  | 1683 -> One (r309)
  | 1679 -> One (r310)
  | 1678 -> One (r311)
  | 1677 -> One (r312)
  | 1676 -> One (r313)
  | 1675 -> One (r314)
  | 1674 -> One (r315)
  | 438 -> One (r316)
  | 437 -> One (r317)
  | 1673 -> One (r318)
  | 1672 -> One (r319)
  | 440 -> One (r320)
  | 1671 -> One (r321)
  | 1670 -> One (r322)
  | 1164 -> One (r323)
  | 443 -> One (r324)
  | 442 -> One (r325)
  | 1160 -> One (r326)
  | 1159 -> One (r327)
  | 445 -> One (r328)
  | 447 -> One (r329)
  | 450 -> One (r330)
  | 1029 -> One (r331)
  | 1028 -> One (r332)
  | 457 -> One (r333)
  | 460 -> One (r335)
  | 459 -> One (r336)
  | 456 -> One (r337)
  | 455 -> One (r338)
  | 1158 -> One (r339)
  | 1157 -> One (r340)
  | 1156 -> One (r341)
  | 465 -> One (r342)
  | 464 -> One (r343)
  | 463 -> One (r344)
  | 664 -> One (r345)
  | 663 -> One (r346)
  | 1146 -> One (r347)
  | 1145 -> One (r348)
  | 468 -> One (r349)
  | 1144 -> One (r350)
  | 1143 -> One (r351)
  | 1142 -> One (r352)
  | 473 -> One (r353)
  | 472 -> One (r354)
  | 1141 -> One (r355)
  | 1140 -> One (r356)
  | 1139 -> One (r357)
  | 476 -> One (r358)
  | 475 -> One (r359)
  | 1138 -> One (r360)
  | 538 -> One (r361)
  | 851 -> One (r364)
  | 841 -> One (r366)
  | 840 -> One (r367)
  | 839 -> One (r368)
  | 1137 -> One (r370)
  | 1136 -> One (r371)
  | 483 -> One (r372)
  | 481 -> One (r373)
  | 480 -> One (r374)
  | 563 -> One (r375)
  | 552 -> One (r376)
  | 551 -> One (r378)
  | 550 -> One (r379)
  | 487 -> One (r380)
  | 557 -> One (r382)
  | 496 -> One (r383)
  | 493 -> One (r384)
  | 492 -> One (r386)
  | 491 -> One (r387)
  | 490 -> One (r388)
  | 489 -> One (r389)
  | 495 -> One (r390)
  | 556 -> One (r391)
  | 507 | 834 -> One (r393)
  | 508 -> One (r395)
  | 500 -> One (r396)
  | 499 -> One (r397)
  | 501 -> One (r398)
  | 503 -> One (r399)
  | 513 -> One (r401)
  | 512 -> One (r403)
  | 549 -> One (r404)
  | 548 -> One (r405)
  | 516 -> One (r406)
  | 518 -> One (r407)
  | 539 -> One (r408)
  | 521 -> One (r409)
  | 520 -> One (r410)
  | 524 -> One (r411)
  | 523 -> One (r412)
  | 531 -> One (r413)
  | 533 -> One (r414)
  | 536 -> One (r415)
  | 541 -> One (r416)
  | 543 -> One (r417)
  | 545 -> One (r418)
  | 547 -> One (r419)
  | 561 -> One (r420)
  | 565 -> One (r421)
  | 1107 -> One (r422)
  | 600 -> One (r423)
  | 1135 -> One (r425)
  | 574 -> One (r426)
  | 569 -> One (r427)
  | 573 -> One (r429)
  | 572 -> One (r430)
  | 571 -> One (r431)
  | 1119 -> One (r432)
  | 1118 -> One (r433)
  | 1117 -> One (r434)
  | 577 -> One (r435)
  | 1116 -> One (r436)
  | 951 -> One (r437)
  | 950 -> One (r438)
  | 949 -> One (r439)
  | 957 -> One (r441)
  | 954 -> One (r443)
  | 953 -> One (r444)
  | 952 -> One (r445)
  | 1115 -> One (r446)
  | 582 -> One (r447)
  | 591 -> One (r449)
  | 589 -> One (r450)
  | 588 -> One (r451)
  | 587 -> One (r452)
  | 586 -> One (r453)
  | 594 -> One (r454)
  | 1111 -> One (r456)
  | 1114 -> One (r458)
  | 597 -> One (r459)
  | 596 -> One (r460)
  | 593 -> One (r461)
  | 599 -> One (r462)
  | 1078 -> One (r463)
  | 1077 -> One (r464)
  | 1076 -> One (r465)
  | 1075 -> One (r466)
  | 1074 -> One (r467)
  | 602 -> One (r468)
  | 1106 -> One (r469)
  | 1105 -> One (r470)
  | 1104 -> One (r471)
  | 1103 -> One (r472)
  | 1102 -> One (r473)
  | 1644 -> One (r474)
  | 1073 -> One (r475)
  | 673 -> One (r476)
  | 672 -> One (r477)
  | 605 -> One (r478)
  | 604 -> One (r479)
  | 660 -> One (r480)
  | 658 -> One (r481)
  | 657 -> One (r482)
  | 607 -> One (r483)
  | 609 -> One (r484)
  | 656 -> One (r485)
  | 655 -> One (r486)
  | 611 -> One (r487)
  | 654 -> One (r488)
  | 653 -> One (r489)
  | 620 -> One (r490)
  | 618 -> One (r491)
  | 617 -> One (r492)
  | 614 -> One (r493)
  | 635 -> One (r495)
  | 634 -> One (r496)
  | 633 -> One (r497)
  | 632 -> One (r498)
  | 639 -> One (r500)
  | 640 -> One (r502)
  | 628 -> One (r503)
  | 627 -> One (r504)
  | 624 -> One (r505)
  | 623 -> One (r506)
  | 631 -> One (r507)
  | 630 -> One (r508)
  | 638 -> One (r509)
  | 642 -> One (r510)
  | 644 -> One (r511)
  | 649 -> One (r512)
  | 647 -> One (r513)
  | 662 -> One (r514)
  | 667 -> One (r515)
  | 666 -> One (r516)
  | 1070 -> One (r517)
  | 871 -> One (r518)
  | 1069 -> One (r520)
  | 1068 -> One (r521)
  | 1065 -> One (r522)
  | 1062 -> One (r523)
  | 677 -> One (r524)
  | 1061 -> One (r525)
  | 985 -> One (r526)
  | 984 -> One (r527)
  | 976 -> One (r528)
  | 988 -> One (r530)
  | 1060 -> One (r532)
  | 1059 -> One (r533)
  | 1058 -> One (r534)
  | 1057 -> One (r535)
  | 1056 -> One (r536)
  | 1055 -> One (r537)
  | 685 -> One (r538)
  | 684 -> One (r539)
  | 1052 -> One (r540)
  | 688 -> One (r541)
  | 687 -> One (r542)
  | 1049 -> One (r543)
  | 1048 -> One (r544)
  | 1047 -> One (r545)
  | 691 -> One (r546)
  | 690 -> One (r547)
  | 1043 -> One (r548)
  | 694 -> One (r549)
  | 693 -> One (r550)
  | 1042 -> One (r551)
  | 1038 -> One (r552)
  | 1037 -> One (r553)
  | 1036 -> One (r554)
  | 1023 -> One (r555)
  | 1014 -> One (r557)
  | 705 -> One (r558)
  | 1035 -> One (r560)
  | 1034 -> One (r561)
  | 700 -> One (r562)
  | 699 -> One (r563)
  | 1033 -> One (r564)
  | 704 -> One (r565)
  | 703 -> One (r566)
  | 1006 -> One (r567)
  | 1005 -> One (r568)
  | 1004 -> One (r569)
  | 1003 -> One (r570)
  | 710 -> One (r571)
  | 709 -> One (r572)
  | 708 -> One (r573)
  | 707 -> One (r574)
  | 997 -> One (r575)
  | 1002 -> One (r577)
  | 1001 -> One (r578)
  | 1000 -> One (r579)
  | 999 -> One (r580)
  | 998 -> One (r581)
  | 995 -> One (r582)
  | 715 -> One (r583)
  | 714 -> One (r584)
  | 713 -> One (r585)
  | 712 -> One (r586)
  | 719 -> One (r587)
  | 724 -> One (r588)
  | 723 -> One (r589)
  | 722 | 992 -> One (r590)
  | 991 -> One (r591)
  | 733 -> One (r592)
  | 732 -> One (r593)
  | 731 -> One (r594)
  | 730 -> One (r595)
  | 729 -> One (r596)
  | 728 -> One (r597)
  | 948 -> One (r598)
  | 740 -> One (r599)
  | 739 -> One (r600)
  | 744 -> One (r601)
  | 743 -> One (r602)
  | 742 -> One (r603)
  | 746 -> One (r604)
  | 891 | 944 -> One (r605)
  | 890 | 943 -> One (r606)
  | 889 | 942 -> One (r607)
  | 747 | 883 -> One (r608)
  | 886 | 941 -> One (r609)
  | 885 | 940 -> One (r610)
  | 748 | 884 -> One (r611)
  | 939 -> One (r612)
  | 752 -> One (r613)
  | 754 -> One (r614)
  | 756 -> One (r615)
  | 758 -> One (r616)
  | 865 | 912 -> One (r617)
  | 864 | 911 -> One (r618)
  | 863 | 910 -> One (r619)
  | 759 | 899 -> One (r620)
  | 762 | 902 -> One (r621)
  | 761 | 901 -> One (r622)
  | 760 | 900 -> One (r623)
  | 859 -> One (r624)
  | 772 -> One (r625)
  | 771 -> One (r626)
  | 776 -> One (r627)
  | 775 -> One (r628)
  | 779 -> One (r629)
  | 781 -> One (r630)
  | 786 -> One (r631)
  | 790 -> One (r632)
  | 789 -> One (r633)
  | 793 -> One (r634)
  | 795 -> One (r635)
  | 797 -> One (r636)
  | 799 -> One (r637)
  | 801 -> One (r638)
  | 803 -> One (r639)
  | 805 -> One (r640)
  | 807 -> One (r641)
  | 809 -> One (r642)
  | 811 -> One (r643)
  | 813 -> One (r644)
  | 815 -> One (r645)
  | 817 -> One (r646)
  | 819 -> One (r647)
  | 821 -> One (r648)
  | 823 -> One (r649)
  | 825 -> One (r650)
  | 827 -> One (r651)
  | 829 -> One (r652)
  | 831 -> One (r653)
  | 856 -> One (r654)
  | 855 -> One (r655)
  | 833 -> One (r656)
  | 838 -> One (r657)
  | 837 -> One (r658)
  | 836 -> One (r659)
  | 843 -> One (r660)
  | 845 -> One (r661)
  | 847 -> One (r662)
  | 849 -> One (r663)
  | 854 -> One (r664)
  | 862 | 907 -> One (r665)
  | 861 | 906 -> One (r666)
  | 860 | 905 -> One (r667)
  | 876 | 924 -> One (r668)
  | 875 | 923 -> One (r669)
  | 874 | 922 -> One (r670)
  | 867 | 916 -> One (r671)
  | 870 | 919 -> One (r672)
  | 869 | 918 -> One (r673)
  | 868 | 917 -> One (r674)
  | 879 | 929 -> One (r675)
  | 878 | 928 -> One (r676)
  | 877 | 927 -> One (r677)
  | 882 -> One (r678)
  | 888 -> One (r679)
  | 893 -> One (r680)
  | 896 | 947 -> One (r681)
  | 895 | 946 -> One (r682)
  | 894 | 945 -> One (r683)
  | 898 -> One (r684)
  | 904 -> One (r685)
  | 909 -> One (r686)
  | 914 -> One (r687)
  | 921 -> One (r688)
  | 926 -> One (r689)
  | 931 -> One (r690)
  | 934 -> One (r691)
  | 956 -> One (r692)
  | 960 -> One (r693)
  | 962 -> One (r694)
  | 964 -> One (r695)
  | 966 -> One (r696)
  | 968 -> One (r697)
  | 971 -> One (r699)
  | 970 -> One (r700)
  | 990 -> One (r701)
  | 989 -> One (r702)
  | 975 -> One (r703)
  | 974 -> One (r704)
  | 978 -> One (r705)
  | 980 -> One (r706)
  | 979 | 1120 -> One (r707)
  | 982 -> One (r708)
  | 1013 -> One (r709)
  | 1012 -> One (r710)
  | 1011 -> One (r711)
  | 1010 -> One (r712)
  | 1009 -> One (r713)
  | 1008 -> One (r714)
  | 1026 -> One (r715)
  | 1018 -> One (r716)
  | 1017 -> One (r717)
  | 1022 -> One (r718)
  | 1021 -> One (r719)
  | 1020 -> One (r720)
  | 1025 -> One (r721)
  | 1032 -> One (r722)
  | 1031 -> One (r723)
  | 1045 -> One (r724)
  | 1051 -> One (r725)
  | 1054 -> One (r726)
  | 1067 -> One (r727)
  | 1072 -> One (r728)
  | 1084 -> One (r729)
  | 1083 -> One (r730)
  | 1091 -> One (r732)
  | 1090 -> One (r733)
  | 1089 -> One (r734)
  | 1082 -> One (r735)
  | 1081 -> One (r736)
  | 1080 -> One (r737)
  | 1088 -> One (r738)
  | 1087 -> One (r739)
  | 1086 -> One (r740)
  | 1093 -> One (r741)
  | 1101 -> One (r742)
  | 1100 -> One (r743)
  | 1099 -> One (r744)
  | 1098 -> One (r745)
  | 1097 -> One (r746)
  | 1096 -> One (r747)
  | 1095 -> One (r748)
  | 1110 -> One (r749)
  | 1109 -> One (r750)
  | 1113 -> One (r751)
  | 1126 -> One (r752)
  | 1125 -> One (r753)
  | 1124 -> One (r754)
  | 1123 -> One (r755)
  | 1122 -> One (r756)
  | 1131 -> One (r757)
  | 1130 -> One (r758)
  | 1129 -> One (r759)
  | 1128 -> One (r760)
  | 1134 -> One (r761)
  | 1133 -> One (r762)
  | 1149 -> One (r763)
  | 1148 -> One (r764)
  | 1152 -> One (r765)
  | 1151 -> One (r766)
  | 1155 -> One (r767)
  | 1154 -> One (r768)
  | 1163 -> One (r769)
  | 1162 -> One (r770)
  | 1189 -> One (r771)
  | 1188 -> One (r772)
  | 1187 -> One (r773)
  | 1186 -> One (r774)
  | 1177 -> One (r775)
  | 1176 -> One (r777)
  | 1175 -> One (r778)
  | 1171 -> One (r779)
  | 1170 -> One (r780)
  | 1169 -> One (r781)
  | 1168 -> One (r782)
  | 1167 -> One (r783)
  | 1174 -> One (r784)
  | 1173 -> One (r785)
  | 1185 -> One (r786)
  | 1184 -> One (r787)
  | 1183 -> One (r788)
  | 1192 -> One (r789)
  | 1191 -> One (r790)
  | 1236 -> One (r791)
  | 1225 -> One (r792)
  | 1224 -> One (r793)
  | 1215 -> One (r794)
  | 1214 -> One (r796)
  | 1213 -> One (r797)
  | 1212 -> One (r798)
  | 1197 -> One (r799)
  | 1196 -> One (r800)
  | 1195 -> One (r801)
  | 1211 -> One (r802)
  | 1210 -> One (r804)
  | 1209 -> One (r805)
  | 1208 -> One (r806)
  | 1204 -> One (r807)
  | 1203 -> One (r808)
  | 1202 -> One (r809)
  | 1201 -> One (r810)
  | 1200 -> One (r811)
  | 1207 -> One (r812)
  | 1206 -> One (r813)
  | 1223 -> One (r814)
  | 1222 -> One (r815)
  | 1221 -> One (r816)
  | 1235 -> One (r817)
  | 1234 -> One (r818)
  | 1233 -> One (r819)
  | 1232 -> One (r820)
  | 1231 -> One (r821)
  | 1230 -> One (r822)
  | 1229 -> One (r823)
  | 1228 -> One (r824)
  | 1669 -> One (r825)
  | 1668 -> One (r826)
  | 1238 -> One (r827)
  | 1240 -> One (r828)
  | 1242 -> One (r829)
  | 1267 -> One (r830)
  | 1266 -> One (r831)
  | 1265 -> One (r832)
  | 1253 -> One (r833)
  | 1252 -> One (r834)
  | 1251 -> One (r835)
  | 1250 -> One (r836)
  | 1247 -> One (r837)
  | 1246 -> One (r838)
  | 1245 -> One (r839)
  | 1249 -> One (r840)
  | 1264 -> One (r841)
  | 1257 -> One (r842)
  | 1256 -> One (r843)
  | 1255 -> One (r844)
  | 1263 -> One (r845)
  | 1262 -> One (r846)
  | 1261 -> One (r847)
  | 1260 -> One (r848)
  | 1259 -> One (r849)
  | 1664 -> One (r850)
  | 1663 -> One (r851)
  | 1269 -> One (r852)
  | 1274 -> One (r853)
  | 1273 -> One (r854)
  | 1272 -> One (r855)
  | 1271 -> One (r856)
  | 1282 -> One (r857)
  | 1285 -> One (r859)
  | 1284 -> One (r860)
  | 1281 -> One (r861)
  | 1280 -> One (r862)
  | 1279 -> One (r863)
  | 1278 -> One (r864)
  | 1277 -> One (r865)
  | 1276 -> One (r866)
  | 1293 -> One (r867)
  | 1292 -> One (r868)
  | 1291 -> One (r869)
  | 1290 -> One (r870)
  | 1296 -> One (r874)
  | 1295 -> One (r875)
  | 1294 -> One (r876)
  | 1366 -> One (r877)
  | 1365 -> One (r878)
  | 1364 -> One (r879)
  | 1363 -> One (r880)
  | 1305 -> One (r881)
  | 1304 -> One (r882)
  | 1303 -> One (r883)
  | 1302 -> One (r884)
  | 1301 -> One (r885)
  | 1299 -> One (r887)
  | 1298 -> One (r889)
  | 1313 -> One (r890)
  | 1306 -> One (r891)
  | 1309 -> One (r892)
  | 1308 -> One (r893)
  | 1312 -> One (r894)
  | 1311 -> One (r895)
  | 1315 -> One (r896)
  | 1314 -> One (r897)
  | 1537 -> One (r898)
  | 1536 -> One (r899)
  | 1327 -> One (r900)
  | 1326 -> One (r901)
  | 1325 -> One (r902)
  | 1324 -> One (r903)
  | 1323 -> One (r904)
  | 1322 -> One (r905)
  | 1321 -> One (r906)
  | 1320 -> One (r907)
  | 1353 -> One (r908)
  | 1352 -> One (r909)
  | 1355 -> One (r911)
  | 1354 -> One (r912)
  | 1348 -> One (r913)
  | 1330 -> One (r914)
  | 1329 -> One (r915)
  | 1334 -> One (r916)
  | 1333 -> One (r917)
  | 1347 -> One (r918)
  | 1339 -> One (r919)
  | 1338 -> One (r920)
  | 1337 -> One (r921)
  | 1336 -> One (r922)
  | 1346 -> One (r923)
  | 1345 -> One (r924)
  | 1344 -> One (r925)
  | 1343 -> One (r926)
  | 1342 -> One (r927)
  | 1341 -> One (r928)
  | 1351 -> One (r929)
  | 1350 -> One (r930)
  | 1357 -> One (r931)
  | 1362 -> One (r932)
  | 1361 -> One (r933)
  | 1360 -> One (r934)
  | 1359 -> One (r935)
  | 1370 -> One (r937)
  | 1369 -> One (r938)
  | 1368 -> One (r940)
  | 1413 -> One (r941)
  | 1431 -> One (r943)
  | 1488 -> One (r945)
  | 1502 -> One (r947)
  | 1492 -> One (r948)
  | 1491 -> One (r949)
  | 1471 -> One (r950)
  | 1470 -> One (r951)
  | 1469 -> One (r952)
  | 1468 -> One (r953)
  | 1467 -> One (r954)
  | 1466 -> One (r955)
  | 1465 -> One (r956)
  | 1455 -> One (r957)
  | 1454 -> One (r958)
  | 1382 -> One (r959)
  | 1381 -> One (r960)
  | 1380 -> One (r961)
  | 1376 -> One (r962)
  | 1374 -> One (r963)
  | 1373 -> One (r964)
  | 1372 -> One (r965)
  | 1379 -> One (r966)
  | 1378 -> One (r967)
  | 1448 -> One (r968)
  | 1447 -> One (r969)
  | 1388 -> One (r970)
  | 1384 -> One (r971)
  | 1387 -> One (r972)
  | 1386 -> One (r973)
  | 1399 -> One (r974)
  | 1398 -> One (r975)
  | 1397 -> One (r976)
  | 1396 -> One (r977)
  | 1395 -> One (r978)
  | 1390 -> One (r979)
  | 1410 -> One (r980)
  | 1409 -> One (r981)
  | 1408 -> One (r982)
  | 1407 -> One (r983)
  | 1406 -> One (r984)
  | 1401 -> One (r985)
  | 1439 -> One (r986)
  | 1438 -> One (r987)
  | 1412 -> One (r988)
  | 1437 -> One (r989)
  | 1436 -> One (r990)
  | 1435 -> One (r991)
  | 1434 -> One (r992)
  | 1415 -> One (r993)
  | 1432 -> One (r994)
  | 1419 -> One (r995)
  | 1418 -> One (r996)
  | 1417 -> One (r997)
  | 1429 | 1477 -> One (r998)
  | 1426 -> One (r1000)
  | 1422 -> One (r1001)
  | 1421 -> One (r1002)
  | 1420 | 1476 -> One (r1003)
  | 1425 | 1485 -> One (r1004)
  | 1424 | 1484 -> One (r1005)
  | 1423 | 1483 -> One (r1006)
  | 1428 -> One (r1007)
  | 1444 -> One (r1008)
  | 1443 -> One (r1009)
  | 1442 -> One (r1010)
  | 1446 -> One (r1012)
  | 1445 -> One (r1013)
  | 1441 -> One (r1014)
  | 1450 -> One (r1015)
  | 1453 -> One (r1016)
  | 1464 -> One (r1017)
  | 1463 -> One (r1018)
  | 1462 -> One (r1019)
  | 1461 -> One (r1020)
  | 1460 -> One (r1021)
  | 1459 -> One (r1022)
  | 1458 -> One (r1023)
  | 1457 -> One (r1024)
  | 1490 -> One (r1025)
  | 1475 -> One (r1026)
  | 1474 -> One (r1027)
  | 1473 -> One (r1028)
  | 1489 -> One (r1029)
  | 1479 -> One (r1030)
  | 1487 -> One (r1031)
  | 1482 -> One (r1032)
  | 1481 -> One (r1033)
  | 1501 -> One (r1034)
  | 1500 -> One (r1035)
  | 1499 -> One (r1036)
  | 1498 -> One (r1037)
  | 1497 -> One (r1038)
  | 1496 -> One (r1039)
  | 1495 -> One (r1040)
  | 1494 -> One (r1041)
  | 1510 -> One (r1042)
  | 1512 -> One (r1043)
  | 1522 -> One (r1044)
  | 1521 -> One (r1045)
  | 1520 -> One (r1046)
  | 1519 -> One (r1047)
  | 1518 -> One (r1048)
  | 1517 -> One (r1049)
  | 1516 -> One (r1050)
  | 1515 -> One (r1051)
  | 1533 -> One (r1052)
  | 1532 -> One (r1053)
  | 1531 -> One (r1054)
  | 1530 -> One (r1055)
  | 1529 -> One (r1056)
  | 1528 -> One (r1057)
  | 1527 -> One (r1058)
  | 1526 -> One (r1059)
  | 1525 -> One (r1060)
  | 1583 -> One (r1061)
  | 1581 -> One (r1063)
  | 1624 -> One (r1065)
  | 1546 -> One (r1066)
  | 1641 -> One (r1068)
  | 1632 -> One (r1069)
  | 1631 -> One (r1070)
  | 1545 -> One (r1071)
  | 1544 -> One (r1072)
  | 1543 -> One (r1073)
  | 1542 -> One (r1074)
  | 1541 -> One (r1075)
  | 1618 -> One (r1076)
  | 1617 -> One (r1077)
  | 1549 -> One (r1078)
  | 1548 -> One (r1079)
  | 1553 -> One (r1080)
  | 1552 -> One (r1081)
  | 1551 -> One (r1082)
  | 1612 -> One (r1083)
  | 1611 -> One (r1084)
  | 1610 -> One (r1085)
  | 1609 -> One (r1086)
  | 1608 -> One (r1087)
  | 1607 -> One (r1088)
  | 1604 -> One (r1089)
  | 1556 -> One (r1090)
  | 1600 -> One (r1091)
  | 1599 -> One (r1092)
  | 1594 -> One (r1093)
  | 1593 -> One (r1094)
  | 1592 -> One (r1095)
  | 1591 -> One (r1096)
  | 1565 -> One (r1097)
  | 1564 -> One (r1098)
  | 1563 -> One (r1099)
  | 1562 -> One (r1100)
  | 1561 -> One (r1101)
  | 1560 -> One (r1102)
  | 1590 -> One (r1103)
  | 1569 -> One (r1104)
  | 1568 -> One (r1105)
  | 1567 -> One (r1106)
  | 1573 -> One (r1107)
  | 1572 -> One (r1108)
  | 1571 -> One (r1109)
  | 1587 -> One (r1110)
  | 1577 -> One (r1111)
  | 1576 -> One (r1112)
  | 1589 -> One (r1114)
  | 1575 -> One (r1115)
  | 1584 -> One (r1116)
  | 1579 -> One (r1117)
  | 1598 -> One (r1118)
  | 1597 -> One (r1119)
  | 1596 -> One (r1120)
  | 1603 -> One (r1121)
  | 1602 -> One (r1122)
  | 1606 -> One (r1123)
  | 1616 -> One (r1124)
  | 1615 -> One (r1125)
  | 1614 -> One (r1126)
  | 1620 -> One (r1127)
  | 1623 -> One (r1128)
  | 1628 -> One (r1129)
  | 1627 -> One (r1130)
  | 1626 -> One (r1131)
  | 1630 -> One (r1132)
  | 1640 -> One (r1133)
  | 1639 -> One (r1134)
  | 1638 -> One (r1135)
  | 1637 -> One (r1136)
  | 1636 -> One (r1137)
  | 1635 -> One (r1138)
  | 1634 -> One (r1139)
  | 1651 -> One (r1140)
  | 1654 -> One (r1141)
  | 1656 -> One (r1142)
  | 1662 -> One (r1143)
  | 1661 -> One (r1144)
  | 1682 -> One (r1145)
  | 1681 -> One (r1146)
  | 1700 -> One (r1147)
  | 1699 -> One (r1148)
  | 1698 -> One (r1149)
  | 1719 -> One (r1150)
  | 1718 -> One (r1151)
  | 1717 -> One (r1152)
  | 1716 -> One (r1153)
  | 1722 -> One (r1154)
  | 1721 -> One (r1155)
  | 1726 -> One (r1156)
  | 1732 -> One (r1157)
  | 1734 -> One (r1158)
  | 1736 -> One (r1159)
  | 1749 -> One (r1160)
  | 1753 -> One (r1161)
  | 1758 -> One (r1162)
  | 1761 -> One (r1163)
  | 1765 -> One (r1164)
  | 1769 -> One (r1165)
  | 1780 -> One (r1166)
  | 1782 -> One (r1167)
  | 1785 -> One (r1168)
  | 1784 -> One (r1169)
  | 1787 -> One (r1170)
  | 1797 -> One (r1171)
  | 1793 -> One (r1172)
  | 1792 -> One (r1173)
  | 1796 -> One (r1174)
  | 1795 -> One (r1175)
  | 1802 -> One (r1176)
  | 1801 -> One (r1177)
  | 1800 -> One (r1178)
  | 1804 -> One (r1179)
  | 515 -> Select (function
    | -1 -> [R 98]
    | _ -> S (T T_DOT) :: r406)
  | 721 -> Select (function
    | -1 -> [R 98]
    | _ -> r591)
  | 158 -> Select (function
    | -1 -> r113
    | _ -> R 190 :: r131)
  | 374 -> Select (function
    | -1 -> r113
    | _ -> R 190 :: r287)
  | 1286 -> Select (function
    | -1 -> r880
    | _ -> R 190 :: r873)
  | 1540 -> Select (function
    | -1 -> S (T T_TYPE) :: r965
    | _ -> R 190 :: r1075)
  | 537 -> Select (function
    | -1 -> [R 643]
    | _ -> r362)
  | 535 -> Select (function
    | -1 -> [R 644]
    | _ -> S (N N_pattern) :: r415)
  | 162 -> Select (function
    | -1 -> r137
    | _ -> R 754 :: r143)
  | 377 -> Select (function
    | -1 -> r137
    | _ -> R 754 :: r293)
  | 452 -> Select (function
    | 483 | 579 | 736 | 833 | 955 | 1104 | 1562 | 1596 -> r80
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (N N_pattern) :: r332)
  | 87 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> Sub (r1) :: r53)
  | 485 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r303
    | _ -> Sub (r377) :: r379)
  | 675 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r303
    | _ -> Sub (r519) :: r521)
  | 601 -> Select (function
    | 61 | 93 | 373 | 440 | 1238 | 1269 -> r474
    | _ -> S (T T_OPEN) :: r468)
  | 183 -> Select (function
    | -1 -> r114
    | _ -> S (T T_COLON) :: r163)
  | 189 -> Select (function
    | 1120 -> r93
    | _ -> Sub (r91) :: r170)
  | 190 -> Select (function
    | 1120 -> r92
    | _ -> r170)
  | 423 -> Select (function
    | -1 -> r109
    | _ -> r114)
  | 1696 -> Select (function
    | -1 -> r109
    | _ -> r114)
  | 1695 -> Select (function
    | -1 -> r110
    | _ -> r129)
  | 422 -> Select (function
    | -1 -> r110
    | _ -> r285)
  | 160 -> Select (function
    | -1 -> r111
    | _ -> r130)
  | 376 -> Select (function
    | -1 -> r111
    | _ -> r286)
  | 159 -> Select (function
    | -1 -> r112
    | _ -> r131)
  | 375 -> Select (function
    | -1 -> r112
    | _ -> r287)
  | 379 -> Select (function
    | -1 -> r135
    | _ -> r114)
  | 176 -> Select (function
    | -1 -> r135
    | _ -> r114)
  | 175 -> Select (function
    | -1 -> r136
    | _ -> r143)
  | 378 -> Select (function
    | -1 -> r136
    | _ -> r293)
  | 1289 -> Select (function
    | -1 -> r877
    | _ -> r871)
  | 1288 -> Select (function
    | -1 -> r878
    | _ -> r872)
  | 1287 -> Select (function
    | -1 -> r879
    | _ -> r873)
  | _ -> raise Not_found
