(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int [@@deriving show { with_path = false }]

module VarSet = Stdlib.Set.Make (Int)

(* actual types *)
type ty =
  | TPrim of string
  | TVar of type_var
  | TArrow of ty * ty
  | TTuple of ty * ty * ty list
  | TList of ty
[@@deriving show { with_path = false }]

type scheme = S of VarSet.t * ty

(* utility functions *)
let tprim_int = TPrim "int"
let tprim_string = TPrim "string"
let tprim_bool = TPrim "bool"
let tprim_unit = TPrim "unit"
let tvar x = TVar x
let tarrow l r = TArrow (l, r)
let ( @-> ) = tarrow
let ttuple fst snd rest = TTuple (fst, snd, rest)
let tlist ty = TList ty

(* errors *)
type error =
  [ `Occurs_check
  | `Undefined_variable of string
  | `Unification_failed of ty * ty
  ]
