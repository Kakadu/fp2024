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
  | TActPat of string * typ (** [Even(int)] *)
  | Choice of (string, typ, Base.String.comparator_witness) Base.Map.t
  (** [Choice<Even(int * int), Odd(string)>] *)
(* Map of Name/typ is Choice of <Name1(typ1), Name2(typ2), ...>, Name/typ is equiavalent to TActPat *)

val gen_typ_primitive : typ QCheck.Gen.t
val arrow_of_types : typ list -> typ -> typ
val choice_to_list : (string, typ, 'a) Base.Map.t -> typ list

val choice_set_many
  :  (string, typ, 'a) Base.Map.t
  -> (string * typ) list
  -> (string, typ, 'a) Base.Map.t

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
