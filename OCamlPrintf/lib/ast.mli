(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifier *)
type ident = string

type rec_flag =
  | Recursive (** Recursive value binding *)
  | Nonrecursive (** Nonrecursive value binding *)

type fmt_item =
  | Simple_string of string (** Just a string *)
  | Fmt_bool (** "%B" *)
  | Fmt_char (** "%c" *)
  | Fmt_int (** "%d" *)
  | Fmt_string (** "%s" *)

(** Full string for printf *)
type fstring = fmt_item list

type constant =
  | Const_integer of int (** Integer constant such as [1] *)
  | Const_char of char (** Character such as ['a'] *)
  | Const_string of string (** Constant string such as ["constant"] *)

type pattern =
  | Pat_any (** The pattern [_] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [1], ['a'], ["true"] *)
  | Pat_tuple of pattern list (** Patterns [(P1, ..., Pn)] *)
  | Pat_construct of ident * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C] when [args] is [None],
      - [C P] when [args] is [Some ([], P)] *)

(** [let pat = exp] *)
type value_binding =
  { pat : pattern
  ; exp : expression
  }

(** Values of type represents [(P -> E)] *)
and case =
  { left : pattern
  ; right : expression
  }

and expression =
  | Exp_ident of ident (** Identifier such as [x] *)
  | Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"] *)
  | Exp_let of rec_flag * value_binding list * expression
  (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive]. *)
  | Exp_fun of pattern * expression (** [Exp_fun (P, E)] represents [fun P -> E] *)
  | Exp_apply of expression * expression (** [Exp_apply(E0, E1)] represents [E0 E1] *)
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
  | Exp_printf of fstring (** printf "abc %d" 52 *)
  | Exp_get (** Expressions such as [get str i] or [str.[i]] *)

type structure_item =
  | Str_eval of expression (** [E] *)
  | Str_value of rec_flag * value_binding list
  (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
      - [let P1 = E1 and ... and Pn = EN] when [rec] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = EN ] when [rec] is [Recursive]. *)

type structure = structure_item list
