(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string [@@deriving show { with_path = false }]

type constant =
  | Const_int of int
  | Const_bool of bool
  | Const_char of char
  | Const_string of string
  | Const_unit
[@@deriving show { with_path = false }]

type ttype =
  | Type_int
  | Type_bool
  | Type_char
  | Type_string
  | Type_unit
  | Type_list of ttype
  | Type_tuple of ttype list

type pattern =
  | Pattern_any (** The pattern [ _ ]. *)
  | Pattern_const of constant
  | Pattern_var of string (** A variable pattern such as [x] *)
  | Pattern_option of pattern option
  | Pattern_tuple of pattern list (** Represnts n-tuples (x1, x2, ... ,xn) *)
  | Pattern_list_sugar_case of pattern list
  | Pattern_list_constructor_case of pattern list
[@@deriving show { with_path = false }]

type binary_op =
  | Plus (** [+] *)
  | Sub (** [-] *)
  | Mul (** [*] *)
  | Div (** [/] *)
  | And (** [&&]*)
  | Or (** [||]*)
  | Equal (** [=]*)
  | NotEqual (** [<>] or [!=]*)
  | Less (** [<]*)
  | LessEqual (** [<=]*)
  | Greater (** [>]*)
  | GreaterEqual (** [>=]*)
[@@deriving show { with_path = false }]

type expression =
  | Expr_var of ident
  | Expr_const of constant
  | Expr_list of expression list
  | Expr_tuple of expression list
  | Expr_binary_op of binary_op * expression * expression
  | Expr_if_then_else of expression * expression * expression
  | Expr_match_with of expression * (pattern * expression) list
  | Expr_construct_in of let_binding * expression
  | Expr_anonym_fun of pattern * expression
  | Expr_function_fun of (pattern * expression) list
  | Expr_application of application
  | Typed_expression of ttype * expression

and application =
  | Apply_ident of ident * expression list
  | Apply_function of expression * expression list

and rec_flag =
  | Recursive
  | Non_recursive

and let_declaration =
  | Let_pattern of rec_flag * pattern
  | Let_fun of rec_flag * ident * pattern list

and let_binding = Let_binding of let_declaration * expression

type structure = Structure_items of let_binding list
[@@deriving show { with_path = false }]
