(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

type typ =
  | Primitive of string
  | Type_var of binder
  | Arrow of typ * typ
  | Type_list of typ
  | Type_tuple of typ * typ * typ list
  | TOption of typ

val gen_typ_primitive : typ QCheck.Gen.t
val arrow_of_types : typ list -> typ -> typ

module VarSet : sig
  include module type of Stdlib.Set.Make (Int)

  val pp : Format.formatter -> t -> unit
end

type binder_set = VarSet.t
type scheme = Scheme of binder_set * typ

val int_typ : typ
val bool_typ : typ
val string_typ : typ
val unit_typ : typ
