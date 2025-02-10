[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type type_var = int

(** `base_type` representing base type. *)
type base_type =
  | BInt (** Int base type *)
  | BBool (** Bool base type *)
  | BUnit (** Unit base type *)
  | BString (** String base type *)

(** `typ` representing type of expression or pattern *)
type typ =
  | TBase of base_type
  | TVar of type_var
  | TArrow of typ * typ
  | TTuple of typ * typ * typ list
  | TList of typ
  | TOption of typ

(** Infix operator for creation `TArrow` type *)
val ( @-> ) : typ -> typ -> typ

(** Set for keeping variable types *)
module TVarSet : sig
  include module type of Set.Make (Int)
end

(** Set for keeping variable names in environment *)
module VarSet : sig
  include module type of Set.Make (String)
end

(** Errors due inference of expression or pattern *)
type error =
  | OccursCheckFailed of type_var * typ
  (** Type variable contains in type. It's error because infinity types appear *)
  | UnificationFailed of typ * typ (** Unification process end with error. *)
  | Unbound of string (** Some variable x don't containing in environment *)
  | PatternNameTwice of string (** Some variable x appears twice in one pattern *)
  | UnknownType of string (** Unknown type given in expression constraint *)
  | SomeError of string (** Error with custom message *)

val pp_type_var : Format.formatter -> type_var -> unit
val show_type_var : type_var -> string
val pp_base_type : Format.formatter -> base_type -> unit
val show_base_type : base_type -> string
val pp_typ : Format.formatter -> typ -> unit
val show_typ : typ -> string
val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

(** Scheme it's type with set of free variables in that type *)
type scheme = Scheme of TVarSet.t * typ
