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
    | Const_integer of int
    | Const_char of char
    | Const_string of ident

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> ident
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
end

module TypeExpr : sig
  type t =
    | Type_arrow of t * t
    | Type_var of ident
    | Type_tuple of t List2.t
    | Type_construct of ident * t list

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
    | Pat_constraint of t * TypeExpr.t
    | Pat_any
    | Pat_var of ident
    | Pat_constant of Constant.t
    | Pat_tuple of t List2.t
    | Pat_construct of ident * t option

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
    | Nonrecursive
    | Recursive

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
    | Exp_ident of ident
    | Exp_constant of Constant.t
    | Exp_tuple of t List2.t
    | Exp_function of t case List1.t
    | Exp_fun of Pattern.t List1.t * t
    | Exp_apply of t * t
    | Exp_match of t * t case List1.t
    | Exp_constraint of t * TypeExpr.t
    | Exp_if of t * t * t option
    | Exp_let of rec_flag * t value_binding List1.t * t
    | Exp_construct of ident * t option

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
    | Str_eval of Expression.t
    | Str_value of Expression.rec_flag * Expression.t Expression.value_binding List1.t
    | Str_adt of ident list * ident * (ident * TypeExpr.t option) List1.t

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
