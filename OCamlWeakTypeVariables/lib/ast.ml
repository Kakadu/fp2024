[@@@ocaml.text "/*"]

(** Copyright 2024, Damir Yunosov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** literals *)
type literal =
  | Int of int (** literal of int. Ex: 5 *)
  | String of string (** literal of string. Ex: "homka" *)
  | Boolean of bool (** literal of boolean. Ex: true *)
[@@deriving show { with_path = false }]

(** indetificator *)
type id = string [@@deriving show { with_path = false }]

(** binary operators *)
type op_bin =
  | Plus (** [ + ] *)
  | Minus (** [ - ] *)
  | Mul (** [ * ] *)
  | Div (** [ / ] *)
  | Equal (** [ = ] *)
  | Greater (** [ > ] *)
  | GreaterEqual (** [ >= ] *)
  | Less (** [ < ] *)
  | LessEqual (** [ <= ] *)
  | And (** [ && ] *)
  | Or (** [ || ] *)
[@@deriving show { with_path = false }]

(** unary operators *)
type op_un =
  | Not (** [ not ] *)
  | UnMinus (** unary minus. Ex: '-' in -4 *)
[@@deriving show { with_path = false }]

(** ternary operators *)
type op_tern = IfThenElse [@@deriving show { with_path = false }]

(** recursive flag *)
type rec_flag =
  | Recursive
  | NotRecursive
[@@deriving show { with_path = false }]

type expr =
  | Const of literal (** const of literal. Ex: 5, "Homka", true *)
  | Variable of id (** variable with name. Ex: "homka" *)
  | Op_un of op_un * expr (** Unary operation. Ex: not true *)
  | Op_bin of op_bin * expr * expr (** Binary operation. Ex: 5 + 5 *)
  | Op_tern of op_tern * expr * expr * expr (** Ternary operation. Ex: If then else*)
  | Function of id * expr list (** Function: Ex: print a (5 + 5) b true *)
  | Binding of id * rec_flag * id list * expr (** Binding. Ex: let homka a = a + a *)
[@@deriving show { with_path = false }]
