(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type const =
  | CInt of int
  | CBool of bool
  | CUnit
(* () *)
(* | CString of string *)
[@@deriving show { with_path = false }]

type op_un =
  | Neg (* - *)
  | Pos (* + *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving show { with_path = false }]

type op_bin =
  | Add
  | Sub
  | Mul
  | Div
  | And (* && *)
  | Or (* || *)
  | Gt (* > *)
  | Lt (* < *)
  | Gte (* >= *)
  | Lte (* <= *)
  | Eq (* = *)
  | NEq (* <> *)
[@@deriving show { with_path = false }]

type binder = int [@@deriving show { with_path = false }]

type core_type =
  | TPrim of string
  | TVar of binder
  | TArrow of core_type * core_type
  | TTuple of core_type * core_type * core_type list
  | TList of core_type
  | TOption of core_type
[@@deriving show { with_path = false }]

type pattern =
  | PAny (* _ *)
  | PVar of id
  | PUnit (* () *)
  | PTuple of pattern * pattern * pattern list
  (* | PCons of pattern * pattern (* h :: t *) *)
  | PList of pattern list (* [23; 34] *)
  | PConst of const (* 23 *)
  (* | POption of pattern option *)
  | PType of pattern * core_type
[@@deriving show { with_path = false }]

type expression =
  | EConst of const
  | EVar of id
  | EUnary of op_un * expression
  | EBinary of op_bin * expression * expression
  | ETuple of expression * expression * expression list
  | EList of expression list
  | EOption of expression option
  (* | EMatch of expression * case * case list (* match *) *)
  | EIf of expression * expression * expression option (* if x then false else true *)
  | EFun of pattern * expression (* fun x -> x *)
  (* | EFunction of case * case list (* function *) *)
  | EApply of expression * expression (* f x *)
  | ELet of rec_flag * value_binding * value_binding list * expression
  | EType of expression * core_type
(* let x = 23 in x *)
(* | ECons of expression * expression *)
[@@deriving show { with_path = false }]

(* and case = pattern * expression [@@deriving show { with_path = false }] *)
and value_binding = pattern * expression [@@deriving show { with_path = false }]

type structure_item =
  | SValue of rec_flag * value_binding * value_binding list (* let f x = x *)
(* | SEval of expression *)
[@@deriving show { with_path = false }]

type program = structure_item list [@@deriving show { with_path = false }]
