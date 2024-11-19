(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a list_ = 'a list

val pp_list_ : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list_ -> unit
val show_list_ : (Format.formatter -> 'a -> unit) -> 'a list_ -> string
val gen_list_ : 'a QCheck.Gen.t -> 'a list QCheck.Gen.t
val arb_list_ : 'a QCheck.Gen.t -> 'a list QCheck.arbitrary

(** Identifier *)
type ident = string

val gen_ident : string QCheck.Gen.t
val show_ident : ident -> string
val gen_ident : string QCheck.Gen.t
val arb_ident : string QCheck.arbitrary

type rec_flag =
  | Recursive (** Recursive value binding *)
  | Nonrecursive (** Nonrecursive value binding *)

val show_rec_flag : rec_flag -> string
val gen_rec_flag : rec_flag QCheck.Gen.t
val arb_rec_flag : rec_flag QCheck.arbitrary

type constant =
  | Const_integer of int (** Integer constant such as [1] *)
  | Const_char of char (** Character such as ['a'] *)
  | Const_string of string (** Constant string such as ["constant"] *)

val show_constant : constant -> string
val gen_constant : constant QCheck.Gen.t
val arb_constant : constant QCheck.arbitrary

type core_type =
  | Type_any (** [_] *)
  | Type_char (** [char] *)
  | Type_int (** [int] *)
  | Type_string (** [string] *)
  | Type_bool (** [bool] *)
  | Type_list of core_type (** [T list] *)
  | Type_tuple of core_type * core_type * core_type list_ (** [T1 * ... * Tn] *)

val show_core_type : core_type -> string
val gen_core_type_sized : int -> core_type QCheck.Gen.t
val gen_core_type : core_type QCheck.Gen.t
val arb_core_type_sized : int -> core_type QCheck.arbitrary
val arb_core_type : core_type QCheck.arbitrary

type pattern =
  | Pat_any (** The pattern [_] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [1], ['a'], ["true"] *)
  | Pat_tuple of pattern * pattern * pattern list_ (** Patterns [(P1, ... , Pn)] *)
  | Pat_construct of ident * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C]   when [args] is [None],
      - [C P] when [args] is [Some P] *)
  | Pat_constraint of pattern * core_type (** Pattern [(P : T)] *)

val show_pattern : pattern -> string
val gen_pattern_sized : int -> pattern QCheck.Gen.t
val gen_pattern : pattern QCheck.Gen.t
val arb_pattern_sized : int -> pattern QCheck.arbitrary
val arb_pattern : pattern QCheck.arbitrary

(** [let pat = exp] *)
type 'exp value_binding =
  { pat : pattern
  ; exp : 'exp
  }

val show_value_binding
  :  (Format.formatter -> 'exp -> unit)
  -> 'exp value_binding
  -> string

val gen_value_binding : 'a QCheck.Gen.t -> 'a value_binding QCheck.Gen.t
val arb_value_binding : 'a QCheck.Gen.t -> 'a value_binding QCheck.arbitrary

(** Values of type represents [(P -> E)] *)
type 'exp case =
  { left : pattern
  ; right : 'exp
  }

val show_case : (Format.formatter -> 'exp -> unit) -> 'exp case -> string
val gen_case : 'a QCheck.Gen.t -> 'a case QCheck.Gen.t
val arb_case : 'a QCheck.Gen.t -> 'a case QCheck.arbitrary

module Expression : sig
  type expression =
    | Exp_ident of ident (** Identifier such as [x] *)
    | Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"] *)
    | Exp_let of
        rec_flag * expression value_binding * expression value_binding list_ * expression
    (** [Exp_let(flag, [(P1, E1); ... ; (Pn, En)], E)] represents:
        - [let     P1 = E1 and ... and Pn = En in E] when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = En in E] when [flag] is [Recursive]. *)
    | Exp_fun of pattern list_ * expression
    (** [Exp_fun([P1; ... ; Pn], E)] represents [fun P1 ... Pn -> E] *)
    | Exp_apply of expression * expression * expression list_
    (** [Exp_apply(E0, [E1; ... ; En])] represents [E0 E1 ... En] *)
    | Exp_match of expression * expression case * expression case list_
    (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
    | Exp_tuple of expression * expression * expression list_
    (** Expressions [(E1, ... , En)] *)
    | Exp_construct of ident * expression option
    (** [Exp_construct(C, exp)] represents:
        - [C]                when [exp] is [None],
        - [C E]              when [exp] is [Some E],
        - [C (E1, ... , En)] when [exp] is [Some (Exp_tuple[E1; ... ; En])] *)
    | Exp_ifthenelse of expression * expression * expression option
    (** [if E1 then E2 else E3] *)
    | Exp_sequence of expression * expression (** [E1; E2] *)
    | Exp_constraint of expression * core_type (** [(E : T)] *)

  val show_expression : expression -> ident
  val gen_expression_sized : int -> expression QCheck.Gen.t
  val gen_expression : expression QCheck.Gen.t
  val arb_expression_sized : int -> expression QCheck.arbitrary
  val arb_expression : expression QCheck.arbitrary
end

type structure_item =
  | Struct_eval of Expression.expression (** [E] *)
  | Struct_value of
      rec_flag
      * Expression.expression value_binding
      * Expression.expression value_binding list_
  (** [Struct_value(flag, [(P1, E1); ... ; (Pn, En))])] represents:
      - [let     P1 = E1 and ... and Pn = En] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = En] when [flag] is [Recursive]. *)

val show_structure_item : structure_item -> string
val gen_structure_item : structure_item QCheck.Gen.t
val arb_structure_item : structure_item QCheck.arbitrary

type structure = structure_item list_

val show_structure : structure -> string
val gen_structure : structure_item list QCheck.Gen.t
val arb_structure : structure_item list QCheck.arbitrary
