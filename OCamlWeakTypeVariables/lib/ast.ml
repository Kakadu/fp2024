[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** constants *)
type constant =
  | Pconst_int of int (** constant of int. Ex: 5 *)
  | Pconst_string of string (** constant of string. Ex: "homka" *)
  | Pconst_boolean of bool (** constant of boolean. Ex: true *)
[@@deriving show { with_path = false }]

(** identificator *)
type id = string [@@deriving show { with_path = false }]

type pattern =
  | Ppat_any (** The pattern _. *)
  | Ppat_var of string (** A variable pattern such as x *)
  | Ppat_constant of constant (** Patterns such as 1, 'a', "true", 1.0 *)
  | Ppat_tuple of pattern list (** Patterns (P1, ..., Pn). Invariant: n >= 2 *)
  | Ppat_construct of id * pattern option
[@@deriving show { with_path = false }]

(** recursive flag *)
type rec_flag =
  | Recursive
  | NonRecursive
[@@deriving show { with_path = false }]

type expression =
  | Pexp_ident of id (** Identifiers. Ex: "homka" *)
  | Pexp_constant of constant (** Expressions constant. Ex: 5, "Homka", true *)
  | Pexp_let of rec_flag * value_binding list * expression
  (** Pexp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E) represents:
      let P1 = E1 and ... and Pn = EN in E when flag is Nonrecursive
      let rec P1 = E1 and ... and Pn = EN in E when flag is Recursive *)
  | Pexp_fun of pattern * expression (** fun P -> E *)
  | Pexp_apply of expression * expression list (** Function: Ex: print a (5 + 5) b true *)
  | Pexp_tuple of expression list (** Expressions (E1, ..., En). Invariant: n >= 2 *)
  | Pexp_ifthenelse of expression * expression * expression option
  (** if E1 then E2 else E3. Ex: If homka then hype else no_hype *)
  | Pexp_constraint of expression * Types.typ (** (E : T) *)
  | Pexp_construct of id * expression option
  | Pexp_match of expression * case list (** match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_function of case list (** function P1 -> E1 | ... | Pn -> En *)
[@@deriving show { with_path = false }]

(* let pat : type_constraint = exp *)
and value_binding =
  { pvb_pat : pattern
  ; pvb_expr : expression
  }
[@@deriving show { with_path = false }]

and case =
  { pc_lhs : pattern
  ; pc_rhs : expression
  }
[@@deriving show { with_path = false }]

type structure_item =
  | Pstr_eval of expression
  | Pstr_value of rec_flag * value_binding list
  (** Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))]) represents:
      et P1 = E1 and ... and Pn = EN when rec is Nonrecursive,
      let rec P1 = E1 and ... and Pn = EN  when rec is Recursive.*)
[@@deriving show { with_path = false }]

type program = structure_item list [@@deriving show { with_path = false }]
