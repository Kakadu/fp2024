(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int
  | String of string
  | Bool of bool
  | Unit
(* language constants of type int, string, bool, and unit respectively *)
[@@deriving show { with_path = false }]

type bin_op =
  | Add (* addition of two ints *)
  | Mult (* multiplication of two ints *)
  | Sub (* subtraction of two ints *)
  | Div (* division of two ints *)
  | Gt (* greater than *)
  | Lt (* less than *)
  | Eq (* equal *)
  | Neq (* not equal *)
  | Gte (* greater than or equal *)
  | Lte (* less than or equal *)
  | And (* logical AND *)
  | Or (* logical OR *)
[@@deriving show { with_path = false }]

type un_op =
  | Negative
  | Not
(* unary minus, logical NOT *)
[@@deriving show { with_path = false }]

type id = string
(* maybe will add a position later, like string * pos, now used for clarity *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Recursive
  | Non_recursive
(* flag for let expressions *)
[@@deriving show { with_path = false }]

type pattern =
  | PVar of id
  | PConst of const
[@@deriving show { with_path = false }]

type expr =
  | Econst of const (* constants, e.g. 10, "meow", true *)
  | Evar of id (* identifiers, e.g. "x", "f"*)
  (* maybe later switch to id, or even now *)
  | Eif_then_else of expr * expr * expr option
  (* if E0 then E1 else E2; else expression is optional *)
  | Etuple of expr list (* expressions (E0, .., En), n >= 2 *)
  (* or expr * expr * expr list, cause invariant n >= 2 *)
  | Elist of expr list (* expressions [E0; ..; En], n >= 0 *)
  | Ebin_op of bin_op * expr * expr (* E0 bin_op E1, e.g. 1 + 3 *)
  | Eun_op of un_op * expr (* E0 un_op E1, e.g. Negative 2, Not true *)
  | Elet of rec_flag * id * expr * expr
  (* let (rec) P1 = E1 and P2 = E2 and ... and Pn = En in E, e.g. let x = 5 *)
  | Efun_application of expr * expr (* E0 E1, e.g. f x *)
  | Efun of id list * expr
(* anonymous functions, e.g. fun x y -> x + 1 - y, arguments num >= 1  *)
(* should probably change id to pattern later *)
[@@deriving show { with_path = false }]
