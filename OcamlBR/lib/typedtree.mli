(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
type type_var = int

val pp_type_var : Format.formatter -> type_var -> unit
val show_type_var : type_var -> string

module VarSet : sig
  type elt = type_var
  type t = Set.Make(Int).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val diff : t -> t -> t
  val inter : t -> t -> t
end

type ty =
  | TPrim of string
  | TVar of type_var
  | TArrow of ty * ty
  | TTuple of ty * ty * ty list
  | TList of ty
  | TOption of ty

val pp_ty : Format.formatter -> ty -> unit
val show_ty : ty -> string

type scheme = S of VarSet.t * ty

val pp_scheme : Format.formatter -> scheme -> unit
val show_scheme : scheme -> string
val tprim_int : ty
val tprim_string : ty
val tprim_bool : ty
val tprim_unit : ty
val tvar : type_var -> ty
val tarrow : ty -> ty -> ty
val ( @-> ) : ty -> ty -> ty
val ttuple : ty -> ty -> ty list -> ty
val tlist : ty -> ty

type error =
  [ `Occurs_check
  | `Undefined_variable of string
  | `Unification_failed of ty * ty
  | `Ill_left_hand_side of string
  | `Ill_right_hand_side of string
  ]

val pp_error
  :  Format.formatter
  -> [< `Occurs_check | `Undefined_variable of string | `Unification_failed of ty * ty | `Ill_left_hand_side of string | `Ill_right_hand_side of string]
  -> unit
