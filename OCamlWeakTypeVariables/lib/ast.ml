[@@@ocaml.text "/*"]

(** Copyright 2024, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** constants *)
type constant =
  | Pconst_int of int (** constant of int. Ex: 5 *)
  | Pconst_string of string (** constant of string. Ex: "homka" *)
  | Pconst_boolean of bool (** constant of boolean. Ex: true *)
  | Pconst_float of float (** constant of float. Ex: 1.22 *)
[@@deriving show { with_path = false }]

(** identificator *)
type id = Id of string [@@deriving show { with_path = false }]

type pattern =
  | Ppat_any (** The pattern _. *)
  | Ppat_var of string (** A variable pattern such as x *)
  | Ppat_constant of constant (** Patterns such as 1, 'a', "true", 1.0 *)
  | Ppat_interval of constant * constant (** Patterns such as 'a'..'z'. *)
  | Ppat_tuple of pattern list (** Patterns (P1, ..., Pn). Invariant: n >= 2 *)
[@@deriving show { with_path = false }]


(** recursive flag *)
type rec_flag =
  | Recursive
  | NotRecursive
[@@deriving show { with_path = false }]

type expr =
  | Pexpr_ident of id (** variable with name. Ex: "homka" *)
  | Pexpr_const of constant (** const of literal. Ex: 5, "Homka", true *)
  (** Pexp_let(flag, P1, E1, E) represents: *)
  (** let P1 = E1 in E when flag is NotRecursive *)
  (** let rec P1 = E1 in E when flag is Recursive. *)
  | Pexpr_let of rec_flag * pattern * expr * expr 
  | Pexpr_ifThenElse of expr * expr * expr option
  (** If then else. Ex: If homka then hype else no_hype *)
  | Pexpr_apply of expr * expr list (** Function: Ex: print a (5 + 5) b true *)
  | Pexpr_fun of pattern * expr (** fun P -> E1 *)
[@@deriving show { with_path = false }]

type structure_item =
  | Pstr_value of rec_flag * pattern * expr (** let homka = 5 *)
  | Pstr_expr of expr 
[@@deriving show { with_path = false }]
