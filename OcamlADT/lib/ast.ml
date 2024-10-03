(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type constant =
  | Const_integer of int (** integer as [52] *)
  | Const_char of char (** char as ['w'] *)
  | Const_string of string (** string as ["Kakadu"] *)
[@@deriving eq, show { with_path = false }]
(* Generates fun's for equality check, string type output and removing the full path to module*)

type ident = string [@@deriving eq, show { with_path = false }]

type pattern =
  | Pat_any (** The pattern [_]. *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [52], ['w'], ["uwu"] *)
  | Pat_tuple of pattern * pattern * pattern list (** Patterns [(P1, ..., Pn)]. *)
  | Pat_construct of ident * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C]               when [args] is [None],
      - [C P]             when [args] is [Some (P)]
      - [C (P1, ..., Pn)] when [args] is
        [Some (Pat_tuple [P1; ...; Pn])] *)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving eq, show { with_path = false }]

type expression =
  | Exp_ident of ident (** Identifiers such as [x] *)
  | Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"]**)
  | Exp_var of ident (** A variable such as [x] *)
  | Exp_tuple of expression * expression * expression list
  (** Expressions [(E1, E2, ..., En)] *)
  | Exp_function of case * case list
  (** [Exp_function (P1, [P2; ...; Pn])] represents
      [function P1 | ... | Pn] *)
  | Exp_fun of pattern * pattern list * expression
  (**[Exp_fun (P1, [P2; ...; Pn], E)] represents:
     [fun P1 ... Pn -> E] *)
  | Exp_apply of expression * expression (** [Pexp_apply(E0, E1)]
                                             represents [E0 E1] *)
  | Exp_match of expression * case * case list
  (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Exp_if of expression * expression * expression option (** [if E1 then E2 else E3] *)
  | Exp_let of rec_flag * value_binding list * expression
  (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E]
        when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = EN in E]
        when [flag] is [Recursive]. *)
  | Exp_construct of ident * expression option
  (** [Exp_construct(C, exp)] represents:
      - [C]               when [exp] is [None],
      - [C E]             when [exp] is [Some E],
      - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
[@@deriving eq, show { with_path = false }]

and value_binding =
  { pat : pattern
  ; expr : expression
  }

and case =
  { left : pattern
  ; right : expression
  }

type type_expr =
  | Type_arrow of type_expr * type_expr (** [Type_arrow(T1, T2)] represents:
                                            [T1 -> T2] *)
  | Type_var of ident
  | Type_tuple of type_expr * type_expr * type_expr list
  (** [Type_tuple([T1, T2, ... Tn])] *)
  | Type_construct of ident * type_expr list
  (** [Type_constr(ident, l)] represents:
      - [tconstr]               when [l=[]],
      - [T tconstr]             when [l=[T]],
      - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]]. *)
[@@deriving eq, show { with_path = false }]

type structure_item =
  | Str_eval of expression
  | Str_value of rec_flag * value_binding list
  (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
      - [let P1 = E1 and ... and Pn = EN]
        when [rec] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = EN ]
        when [rec] is [Recursive]. *)
  | Str_adt of ident * (ident * type_expr list) * (ident * type_expr list) list
  (** [Str_type(C0, [(C1, [(T11; T12; ... ; T1n_1)]); (C2, [(T21;T22; ... ; T2n_2)]); ... ;
  (Cm, [(Tm1;Tm2; ... ; Tmn_n)]) ])] represents:

      [type C0 =
    | C1 of T11 * ... * T1n_1
    | ...
    | Cm of Tm1 * ... * Tmn_n
    ]

      n_i: [n_i >= 0]
      Invariant: [m > 0] *)
[@@deriving eq, show { with_path = false }]

type program = structure_item list [@@deriving eq, show { with_path = false }]
