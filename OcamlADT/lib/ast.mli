(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string

val equal_ident : ident -> ident -> bool
val pp_ident : Format.formatter -> ident -> unit
val show_ident : ident -> string
val gen_charc : char QCheck.Gen.t
val is_not_keyword : string -> bool
val gen_filtered_ident : string QCheck.Gen.t -> string QCheck.Gen.t
val gen_ident : string QCheck.Gen.t
val gen_ident_uc : string QCheck.Gen.t
val gen_ident_lc : bool -> string QCheck.Gen.t

module List1 : sig
  type 'a t = 'a * 'a list

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> ident
  val gen : 'a QCheck.Gen.t -> ('a * 'a list) QCheck.Gen.t
  val arb : 'a QCheck.Gen.t -> ('a * 'a list) QCheck.arbitrary
end

module List2 : sig
  type 'a t = 'a * 'a * 'a list

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> ident
  val gen : 'a QCheck.Gen.t -> ('a * 'a * 'a list) QCheck.Gen.t
  val arb : 'a QCheck.Gen.t -> ('a * 'a * 'a list) QCheck.arbitrary
end

module Constant : sig
  type t =
    | Const_integer of int (** Integer constant. *)
    | Const_char of char (** Character constant. *)
    | Const_string of ident (** String constant. *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> ident
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
end

module TypeExpr : sig
  type t =
    | Type_arrow of t * t (** Represents a function type: [T1 -> T2]. *)
    | Type_var of ident (** Represents a type variable: ['a]. *)
    | Type_tuple of t List2.t (** Represents a tuple type: [(T1, T2, ..., Tn)]. *)
    | Type_construct of ident * t list
    (** Represents a type constructor with arguments: [C T1 ... Tn]. *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> ident
  val gen_sized : int -> t QCheck.Gen.t
  val gen : t QCheck.Gen.t
  val arb_sized : int -> t QCheck.arbitrary
  val arb : t QCheck.arbitrary
end

module Pattern : sig
  type t =
    | Pat_constraint of t * TypeExpr.t (** A pattern with a type constraint: [(P : T)]. *)
    | Pat_any (** The wildcard pattern [_]. *)
    | Pat_var of ident (** A variable pattern, such as [x]. *)
    | Pat_constant of Constant.t
    (** A constant pattern, such as [1], ["text"], or ['t']. *)
    | Pat_tuple of t List2.t (** A tuple pattern, such as [(P1, P2, ..., Pn)]. *)
    | Pat_construct of ident * t option
    (** A constructor pattern, such as [C] or [C P]. *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> ident
  val gen_sized : int -> t QCheck.Gen.t
  val gen : t QCheck.Gen.t
  val arb_sized : int -> t QCheck.arbitrary
  val arb : t QCheck.arbitrary
end

module Expression : sig
  type rec_flag =
    | Nonrecursive (** zanuda is zanuda *)
    | Recursive (** zanuda is zanuda *)

  val equal_rec_flag : rec_flag -> rec_flag -> bool
  val pp_rec_flag : Format.formatter -> rec_flag -> unit
  val show_rec_flag : rec_flag -> ident
  val gen_rec_flag : rec_flag QCheck.Gen.t
  val arb_rec_flag : rec_flag QCheck.arbitrary

  type 'expr value_binding =
    { pat : Pattern.t
    ; expr : 'expr
    }

  val equal_value_binding
    :  ('expr -> 'expr -> bool)
    -> 'expr value_binding
    -> 'expr value_binding
    -> bool

  val pp_value_binding
    :  (Format.formatter -> 'expr -> unit)
    -> Format.formatter
    -> 'expr value_binding
    -> unit

  val show_value_binding
    :  (Format.formatter -> 'expr -> unit)
    -> 'expr value_binding
    -> ident

  val gen_value_binding : (int -> 'a QCheck.Gen.t) -> int -> 'a value_binding QCheck.Gen.t

  type 'expr case =
    { first : Pattern.t
    ; second : 'expr
    }

  val equal_case : ('expr -> 'expr -> bool) -> 'expr case -> 'expr case -> bool

  val pp_case
    :  (Format.formatter -> 'expr -> unit)
    -> Format.formatter
    -> 'expr case
    -> unit

  val show_case : (Format.formatter -> 'expr -> unit) -> 'expr case -> ident
  val gen_case : (int -> 'a QCheck.Gen.t) -> int -> 'a case QCheck.Gen.t

  type t =
    | Exp_ident of ident (** Identifiers such as [x] and [M.x]. *)
    | Exp_constant of Constant.t
    (** Expressions with constants such as [1], ['a'], ["true"]. *)
    | Exp_tuple of t List2.t (** A tuple expression, such as [(E1, E2, ..., En)]. *)
    | Exp_function of t case List1.t
    (** A function with pattern matching, such as [function P1 -> E1 | ... | Pn -> En]. *)
    | Exp_fun of Pattern.t List1.t * t
    (** A function expression, such as [fun P1 ... Pn -> E]. *)
    | Exp_apply of t * t (** Function application, such as [E0 E1]. *)
    | Exp_match of t * t case List1.t
    (** A match expression, such as [match E0 with P1 -> E1 | ... | Pn -> En]. *)
    | Exp_constraint of t * TypeExpr.t (** A type constraint, such as [(E : T)]. *)
    | Exp_if of t * t * t option
    (** An if-then-else expression, such as [if E1 then E2 else E3]. *)
    | Exp_let of rec_flag * t value_binding List1.t * t
    (** A let-binding, such as [let P1 = E1 and ... and Pn = En in E]. *)
    | Exp_construct of ident * t option
    (** A constructor expression, such as [C] or [C E]. *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> ident
  val gen_sized : int -> t QCheck.Gen.t
  val gen : t QCheck.Gen.t
  val arb_sized : int -> t QCheck.arbitrary
  val arb : t QCheck.arbitrary
end

module Structure : sig
  type structure_item =
    | Str_eval of Expression.t (** An evaluated expression, such as [E]. *)
    | Str_value of Expression.rec_flag * Expression.t Expression.value_binding List1.t
    (** A let-binding, such as:
        - [let P1 = E1 and ... and Pn = En]
          when [rec] is [rec_flag.Nonrecursive].
        - [let rec P1 = E1 and ... and Pn = En]
          when [rec] is [rec_flag.Recursive]. *)
    | Str_adt of ident list * ident * (ident * TypeExpr.t option) List1.t
    (** A type declaration for an algebraic data type (ADT),
        such as [type t1 = ... | ... | tn = ...]. *)

  val equal_structure_item : structure_item -> structure_item -> bool
  val pp_structure_item : Format.formatter -> structure_item -> unit
  val show_structure_item : structure_item -> ident
  val gen_structure_item : int -> structure_item QCheck.Gen.t
end

type program = Structure.structure_item list

val equal_program : program -> program -> bool
val pp_program : Format.formatter -> program -> unit
val show_program : program -> string

module Program : sig
  val gen_program : int -> Structure.structure_item list QCheck.Gen.t
end
