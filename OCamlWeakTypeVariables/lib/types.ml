[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type type_var = int [@@deriving show { with_path = false }]

type base_type =
  | BInt
  | BBool
  | BUnit
  | BString
[@@deriving show { with_path = false }]

type typ =
  | TBase of base_type
  | TVar of type_var
  | TArrow of typ * typ
  | TTuple of typ * typ * typ list
  | TList of typ
  | TOption of typ
[@@deriving show { with_path = false }]

let ( @-> ) a b = TArrow (a, b)

module TVarSet = Stdlib.Set.Make (Int)
module VarSet = Stdlib.Set.Make (String)

type error =
  | OccursCheckFailed of type_var * typ
  | UnificationFailed of typ * typ
  | Unbound of string
  | PatternNameTwice of string
  | UnknownType of string
  | SomeError of string
[@@deriving show { with_path = false }]

type scheme = Scheme of TVarSet.t * typ
