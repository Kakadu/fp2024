(** SPDX_License-Identifier: LGPL-3.0 -or-later *)

type ident = string [@@deriving eq, show { with_path = false }]

type const =
  | Const_bool of bool (** Boolean constants *)
  | Const_int of int (** Integer constants *)
  | Const_string of string (** String constants *)
  | Const_nil (** [] *)
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (** Addition: 1 + 2 *)
  | Sub (** Subtraction: 2 - 1 *)
  | Mul (** Multiplication: 4 * 2 *)
  | Div (** Division: 8 / 2 *)
  | Mod (** Modulus: 10 % 5 *)
  | Eq (** Equality: 5 = 5 *)
  | Con (** hd::tl *)
  | Neq (** Inequality: 4 <> 3 *)
  | Lt (** Less than: 7 < 10 *)
  | Gt (** Greater than: 7 > 5 *)
  | Leq (** Less than or equal to: 10 <= 10 *)
  | Geq (** Greater than or equal to: 7 >= 7 *)
  | Or (** Logical OR: true || false *)
  | And (** Logical AND: true && false *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | Pattern_id of ident
  | Pattern_const of const
  | Pattern_list of pattern * pattern
  | Pattern_tuple of pattern list
  | Pattern_wild (** _ *)
[@@deriving eq, show { with_path = false }]

type expr =
  | Expr_const of const (** Represents constant type (string, bool, or int) *)
  | Expr_var of ident (** Variable *)
  | Expr_fun of pattern * expr (** Function with pattern and type *)
  | Expr_app of expr * expr (** Function application *)
  | Expr_if of expr * expr * expr (** Conditional operator *)
  | Expr_let_in of bool * ident * expr * expr (** Let binding with in clause *)
  | Expr_bin_op of bin_op * expr * expr (** Binary operation, e.g., 2 + 2 - 3 *)
  | Expr_match of expr * (pattern * expr) list (** match x_1 with | x_2   -> ... *)
  | Expr_list of expr * expr (** [1;2;3]*)
  | Expr_tuple of expr list (** (1,2,3)*)
[@@deriving eq, show { with_path = false }]

type struct_prog =
  | Let of bool * ident * expr
  | Expression of expr
[@@deriving show { with_path = false }]
