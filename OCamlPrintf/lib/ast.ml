(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifier *)
type ident = string [@@deriving show { with_path = false }]

type rec_flag =
  | Recursive (** Recursive value binding *)
  | Nonrecursive (** Nonrecursive value binding *)
[@@deriving show { with_path = false }]

type constant =
  | Const_integer of int (** Integer constant such as [1] *)
  | Const_char of char (** Character such as ['a'] *)
  | Const_string of string (** Constant string such as ["constant"] *)
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any (** The pattern [_] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [1], ['a'], ["true"] *)
  | Pat_tuple of pattern list (** Patterns [(P1, ..., Pn)] *)
  | Pat_construct of ident * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C] when [args] is [None],
      - [C P] when [args] is [Some P] *)
[@@deriving show { with_path = false }]

(** [let pat = exp] *)
type value_binding =
  { pat : pattern
  ; exp : expression
  }
[@@deriving show { with_path = false }]

(** Values of type represents [(P -> E)] *)
and case =
  { left : pattern
  ; right : expression
  }
[@@deriving show { with_path = false }]

and expression =
  | Exp_ident of ident (** Identifier such as [x] *)
  | Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"] *)
  | Exp_let of rec_flag * value_binding list * expression
  (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive]. *)
  | Exp_fun of pattern list * expression
  (** [Exp_fun([P1; ...; Pn], E)] represents [fun P1 ... Pn -> E] *)
  | Exp_apply of expression * expression list
  (** [Exp_apply(E0, [E1; ...; En])] represents [E0 E1 ... En] *)
  | Exp_match of expression * case list (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Exp_tuple of expression list (** Expressions [(E1, ..., En)] *)
  | Exp_construct of ident * expression option
  (** [Exp_construct(C, exp)] represents:
      - [C]               when [exp] is [None],
      - [C E]             when [exp] is [Some E],
      - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
  | Exp_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Exp_sequence of expression * expression (** [E1; E2] *)
[@@deriving show { with_path = false }]

type structure_item =
  | Str_eval of expression (** [E] *)
  | Str_value of rec_flag * value_binding list
  (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
      - [let P1 = E1 and ... and Pn = En] when [rec] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = En ] when [rec] is [Recursive]. *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
