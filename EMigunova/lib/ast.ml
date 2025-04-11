(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** identidicator *)
type ident = string [@@deriving show { with_path = false }]

(** constants *)
type constant =
  | Const_int of int
  | Const_bool of bool
  | Const_char of char
  | Const_string of string
  | Const_unit (** Represents a single value [ () ]*)
[@@deriving show { with_path = false }]

(** types *)
type ttype =
  | Type_int
  | Type_bool
  | Type_char
  | Type_string
  | Type_unit
  | Type_var of ident (** Represents type variable, e.g. [ 'a ]*)
  | Type_option of ttype option (**e.g. [ int option ]*)
  | Type_list of ttype (**e.g. [ bool list ]*)
  | Type_tuple of ttype list (**e.g. [ int*int*bool ]*)
  | Type_arrow of ttype * ttype (**e.g. [ int -> bool ]*)
[@@deriving show { with_path = false }]

(** patterns *)
type pattern =
  | Pattern_any (** The pattern [ _ ]. *)
  | Pattern_const of constant (** e.g. [ () ], [ true ], [ 2 ] *)
  | Pattern_var of string (** A variable pattern such as [ x ] *)
  | Pattern_option of pattern option (** e.g. [ Some x ],  [ None ] *)
  | Pattern_tuple of pattern list (** Represnts n-tuples (x1, x2, ... ,xn) *)
  | Pattern_list_sugar_case of pattern list (** e.g. [ [x1; x2; x3] ] *)
  | Pattern_list_constructor_case of pattern list (** e.g. [ x1::x2::[x3;x4] ] *)
[@@deriving show { with_path = false }]

(** binary operators*)
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

(** expressions*)
type expression =
  | Expr_var of ident (** e.g. [x] *)
  | Expr_const of constant (** e.g. [3], [true], ["string"] *)
  | Expr_option of expression option (** e.g. [Some (x-3)], [None] *)
  | Expr_list_sugar of expression list (** e.g. [ [4;5;6] ] *)
  | Expr_list_construct of expression list (** e.g. [ 4::5::6::[] ] *)
  | Expr_tuple of expression list (** e.g. [ (x,5+y)) ] *)
  | Expr_binary_op of binary_op * expression * expression
  (** e.g. [ 2+3 ], [ 23<x ], [ a||b ] *)
  | Expr_if_then_else of expression * expression * expression
  (** e.g. [ if x then 1 else 0] *)
  | Expr_match_with of expression * (pattern * expression) list
  (** e.g. [ match x with | a::[] -> true | _ -> false] *)
  | Expr_construct_in of let_binding * expression (** e.g. [ let x = 4 in x] *)
  | Expr_anonym_fun of pattern list * expression (** e.g. [ fun a b -> a + b ] *)
  | Expr_function_fun of (pattern * expression) list
  (** e.g. [ function | (a,b) -> a+b | _ -> 0] *)
  | Expr_application of expression * expression list
  (** e.g. [ f a b ] [ (fun a -> a*3) 5 ] [ (function | true -> 1 | false -> 0) x ] *)
  | Typed_expression of ttype * expression (** e.g. [ (x : int) ] *)

(** recursive flag *)
and rec_flag =
  | Recursive
  | Non_recursive

(** LHS of let binding comes in two forms: (1) let [ pattern ] = ... ;
    (2) let [ fun_id arg1 arg2 ] = ... *)
and let_declaration =
  | Let_pattern of pattern
  | Let_fun of ident * pattern list

(** let bindind *)
and let_binding =
  | Let_binding of rec_flag * let_declaration * expression
  | Let_rec_and_binding of let_binding list
[@@deriving show { with_path = false }]

(** entire program*)
type structure = let_binding list [@@deriving show { with_path = false }]
