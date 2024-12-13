(** Copyright 2024-2027, Ilia Suponev, Dmitri Chirkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

module VarSet : sig
  include Stdlib.Set.S with type elt = int

  val pp : Format.formatter -> t -> unit
end

type binder_set = VarSet.t

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TTuple of ty * ty * ty list
  | TList of ty

type scheme = S of binder_set * ty

val ty_int : ty
val ty_bool : ty
val ty_string : ty
val ty_unit : ty
val ty_arrow : ty -> ty -> ty
val ty_var : binder -> ty
val ty_tuple : ty * ty * ty list -> ty
val ty_list : ty -> ty
val pp_ty : Format.formatter -> ty -> unit
