(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string (* identifier *) [@@deriving eq, ord, show { with_path = false }]

and expr =
  | Expr_int of int (** integer *)
  | Expr_bool of bool (** booleans *)
  | Expr_unit (** type with single value *)
  | Expr_cons of expr * expr (** pair *)
  | Expr_id of id (** identifiers *)
  | Expr_define of id * expr (** definitions *)
  | Expr_if of expr * expr * expr (** condition then else *)
  | Expr_lambda of id list * expr list
  (** anonymous functions, made of list of parameters and its body *)
  | Expr_apply of expr * expr list
  (** represents application of a function. identifier or lambda is called with the list of arguments *)
[@@deriving eq, ord, show { with_path = false }]

type program = expr list [@@deriving eq, ord, show { with_path = false }]
