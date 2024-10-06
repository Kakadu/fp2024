(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string (* identifier *) [@@deriving eq, ord, show { with_path = false }]

type expr =
  | Expr_unit (* type for expr without meaningful return value *)
  | Expr_int of int (* int numbers *)
  | Expr_id of id (* identifier *)
  | Expr_bool of bool (* booleans *)
  | Expr_define of id * expr (* defines id with expr *)
  | Expr_if of expr * expr * expr (* condition then else *)
  | Expr_lambda of
      id list * expr (* anonymous functions, made of list of parameters and its body *)
  | Expr_apply of expr * expr list
    (* represents application of a function. identifier or lambda is called with the list of arguments *)
[@@deriving eq, ord, show { with_path = false }]

type program = expr list [@@deriving eq, ord, show { with_path = false }]
