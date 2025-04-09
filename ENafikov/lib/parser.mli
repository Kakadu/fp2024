(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val start_parsing : 'a Angstrom.t -> string -> ('a, string) result
val is_char : char -> bool
val is_digit : char -> bool
val is_keyword : string -> bool
val is_whitespace : char -> bool
val is_underscore : char -> bool
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val parse_white_space : string Angstrom.t
val parse_white_space1 : string Angstrom.t
val parse_token : 'a Angstrom.t -> 'a Angstrom.t
val parse_token1 : 'a Angstrom.t -> 'a Angstrom.t
val pstrtoken : string -> string Angstrom.t
val pstrtoken1 : string -> string Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t
val parse_bool : Ast.const Angstrom.t
val parse_int : Ast.const Angstrom.t
val parse_string : Ast.const Angstrom.t
val parse_const : Ast.const Angstrom.t
val check_var : (char -> bool) -> string Angstrom.t
val parse_var : string Angstrom.t
val parse_pattern_var : Ast.pattern Angstrom.t
val parse_wild : Ast.pattern Angstrom.t
val parse_pattern_const : Ast.pattern Angstrom.t
val parse_tuple : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val constr_con : Ast.pattern list -> Ast.pattern
val parser_con : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_con_2 : 'a Angstrom.t -> ('a list -> 'b) -> 'b Angstrom.t
val parse_pattern : Ast.pattern Angstrom.t
val p_op : string -> Ast.bin_op -> (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val pmulti : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val pcons : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val padd : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val pcomp : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val peq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val pconj : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val pdisj : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val constr_case : 'a -> 'b -> 'a * 'b
val constr_efun : Ast.pattern list -> Ast.expr -> Ast.expr
val parse_econst : Ast.expr Angstrom.t
val parse_evar : Ast.expr Angstrom.t
val parse_cons_semicolon_expr : 'a Angstrom.t -> ('a list -> 'b) -> 'b Angstrom.t
val parse_tuple_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val plet_body : Ast.pattern list Angstrom.t -> Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_fun_args : Ast.pattern list Angstrom.t

type edispatch =
  { list_e : edispatch -> Ast.expr Angstrom.t
  ; tuple_e : edispatch -> Ast.expr Angstrom.t
  ; fun_e : edispatch -> Ast.expr Angstrom.t
  ; app_e : edispatch -> Ast.expr Angstrom.t
  ; if_e : edispatch -> Ast.expr Angstrom.t
  ; let_in_e : edispatch -> Ast.expr Angstrom.t
  ; matching_e : edispatch -> Ast.expr Angstrom.t
  ; bin_e : edispatch -> Ast.expr Angstrom.t
  ; expr_parsers : edispatch -> Ast.expr Angstrom.t
  }

val pack : edispatch
val parse_expression : Ast.expr Angstrom.t
val let_e : Ast.expr Angstrom.t -> Ast.struct_prog Angstrom.t
val expr_main : Ast.struct_prog Angstrom.t
val parse_bind : Ast.struct_prog Angstrom.t
val program : Ast.struct_prog list Angstrom.t
val main_parse : string -> (Ast.struct_prog list, string) result
