(** Copyright 2024-2025, Azamat Ishbaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

(** Constants *)
type const =
  | ConstInt of int (** integers *)
  | ConstBool of bool (** true or false *)
  | ConstUnit (** Unit () *)
  | ConstNil (** [] *)
[@@deriving show { with_path = false }]

(** Binary operations *)
type binop =
  | Add (** "+" *)
  | Sub (** "-" *)
  | Div (** "/" *)
  | Mul (** "*" *)
  | Lesq (** "<=" *)
  | Greq (** ">=" *)
  | Gre (** ">" *)
  | Les (** "<" *)
  | Eql (** "=" *)
  | Neq (** "!=" *)
  | And (** "&&" *)
  | Or (** "||" *)
[@@deriving show { with_path = false }]

(** Recursive flag *)
type recursive =
  | Rec (** recursive function *)
  | NotRec (** Non-recursive functions *)
[@@deriving show { with_path = false }]

type pattern =
  | PatConst of const (** const -> ... *)
  | PatVar of name (** var -> ... *)
  | PatWild (** _ -> ... *)
  | PatEmpty (** [] -> ... *)
  | PatTuple of pattern * pattern * pattern list (** x_1 :: x_2, x_3 :: x_4 *)
  | PatConc of pattern * pattern * pattern list (** x_1 :: x_2 -> ... *)
  | PatOr of pattern * pattern * pattern list (** x_1 | x_2  -> ... *)
[@@deriving show { with_path = false }]

type expr =
  | ExprConst of const (** consts *)
  | ExprVar of name (** variebles with names *)
  | ExprBinop of binop * expr * expr (** x_1 binop x_2 *)
  | ExprTuple of expr * expr * expr list (** list with x_i separeted via "," *)
  | ExprFunc of name * expr (** anonymous functions *)
  | ExprCond of expr * expr * expr (** if then else *)
  | ExprLet of recursive * (pattern * expr) list * expr (** let rec fun *)
  | ExprApp of expr * expr (** application *)
  | ExprMatch of expr * (pattern * expr) list (** match x_1 with | x_2 -> x_3 | ... *)
[@@deriving show { with_path = false }]

type structure = expr list [@@deriving show { with_path = false }]
