(** Copyright 2024-2025, Sultanov Muhammet and Kudrya Alexander *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifier *)
type ident = string [@@deriving show { with_path = false }]

type visibility =
  | Public (** Represents a public entity *)
  | Private (** Represents a private entity *)
[@@deriving show { with_path = false }]

type recursion_flag =
  | Rec (** Recursive binding *)
  | Nonrec (** Non-recursive binding *)
[@@deriving show { with_path = false }]

type binary_operator =
  | Add (** [+]*)
  | Subt (** [-]*)
  | Mult (** [*]*)
  | Div (** [/]*)
  | Mod (** [%]*)
  | And (** [&&]*)
  | Or (** [||]*)
  | Equal (** [=]*)
  | NotEqual (** [!>]*)
  | LessThan (** [<]*)
  | LessEqual (** [<=]*)
  | GreaterThan (** [>]*)
  | GreaterEqual (** [>=]*)
[@@deriving show { with_path = false }]

type unary_operator =
  | Neg (** Unary [-]*)
  | Not (** Unary [not]*)
[@@deriving show { with_path = false }]

type constant =
  | Int of int (** Integer constant such as [24] *)
  | Float of float (** Float constant such as [3.14] *)
  | Bool of bool (** Boolean constant such as [true] or [false] *)
  | String of string (** String constant such as ["Hello, World!"] *)
  | Char of char (** Char constant such as ['a'] *)
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any (** The pattern [_] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [24], ['3.14'], ["true"], ... *)
  | Pat_tuple of pattern list (** Patterns [(P1, ..., Pn)] *)
  | Pat_constructor of ident * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C] when [args] is [None],
      - [C P] when [args] is [Some P] *)
[@@deriving show { with_path = false }]

type expression =
  | Exp_ident of ident (** Identifier such as [x] *)
  | Exp_constant of constant (** Constants such as [24], ['3.14'], ["true"], ... *)
  | Exp_let of recursion_flag * value_binding list * expression
  (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive]. *)
  | Exp_unary of unary_operator * expression (** Unary operators such as [-E], [not E] *)
  | Exp_binary of expression * binary_operator * expression
  (** Binary operators such as [E1 + E2], [E1 && E2] *)
  | Exp_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Exp_tuple of expression list (** Tuples such as [(E1, ..., En)] *)
  | Exp_function of pattern list * expression (** Function such as [fun P -> E] *)
  | Exp_apply of expression * expression list
  (** [Exp_apply(E0, [E1; ...; En])] represents [E0 E1 ... En] *)
  | Exp_object of obj (** [object ... end] *)
  | Exp_override of (ident * expression) list (** [{< x1 = E1; ...; xn = En >}] *)
  | Exp_match of expression * case list
  (** [Exp_match(E, [C1; ...; Cn])] represents [match E with C1 | ... | Cn] *)
  | Exp_construct of ident * expression option
  (** [Exp_construct(C, exp)] represents:
      - [C]               when [exp] is [None],
      - [C E]             when [exp] is [Some E],
      - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
[@@deriving show { with_path = false }]

and value_binding =
  { pat : pattern
  ; exp : expression
  }

and case =
  { left : pattern
  ; right : expression
  }

and obj =
  { self : pattern
  ; fields : object_field list
  }

and object_field =
  | Obj_val of ident * expression (** [val x = E] *)
  | Obj_method of visibility * ident * expression (** [method x = E] *)
  | Obj_inherit of expression (** [inherit E] *)

type structure_item =
  | Str_eval of expression (** [E] *)
  | Str_value of recursion_flag * value_binding list
  (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
      - [let P1 = E1 and ... and Pn = En] when [rec] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = En ] when [rec] is [Recursive]. *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
