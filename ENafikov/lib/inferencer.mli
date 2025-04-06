(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error_infer =
  | OccursCheck
  | NoVariable of string
  | UnificationFailed of Ty.typ * Ty.typ
  | EmptyPattern
  | EmptyProgram
  | NotImplemented

val pp_error_infer : Format.formatter -> error_infer -> unit

module R : sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error_infer -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error_infer) result
end

type fresh = int

module Type : sig
  type t = Ty.typ

  val occurs_in : fresh -> Ty.typ -> bool
end

val fold_left
  :  ('a, 'b) Base.Map.Poly.t
  -> init:'c R.t
  -> f:('a -> 'b -> 'c -> 'c R.t)
  -> 'c R.t

module Subst : sig
  type t = (fresh, Ty.typ) Base.Map.Poly.t

  val pp : Format.formatter -> (fresh, Ty.typ) Base.Map.Poly.t -> unit
  val empty : ('a, 'b) Base.Map.Poly.t
  val mapping : fresh -> Ty.typ -> (fresh * Ty.typ) R.t
  val singleton : fresh -> Ty.typ -> (fresh, Ty.typ) Base.Map.Poly.t R.t
  val remove : ('a, 'b) Base.Map.Poly.t -> 'a -> ('a, 'b) Base.Map.Poly.t
  val apply : (fresh, Ty.typ) Base.Map.Poly.t -> Ty.typ -> Ty.typ
  val unify : Ty.typ -> Ty.typ -> (fresh, Ty.typ) Base.Map.Poly.t R.t

  val extend
    :  fresh
    -> Ty.typ
    -> (fresh, Ty.typ) Base.Map.Poly.t
    -> (fresh, Ty.typ) Base.Map.Poly.t R.t

  val compose
    :  (fresh, Ty.typ) Base.Map.Poly.t
    -> (fresh, Ty.typ) Base.Map.Poly.t
    -> (fresh, Ty.typ) Base.Map.Poly.t R.t

  val compose_all
    :  (fresh, Ty.typ) Base.Map.Poly.t list
    -> (fresh, Ty.typ) Base.Map.Poly.t R.t
end

module VarSet : sig
  type elt = fresh
  type t = Ty.VarSetInit.t

  val empty : t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val pp : Format.formatter -> t -> unit
  val fold_left_m : ('a -> elt -> 'a R.t) -> t -> 'a R.t -> 'a R.t
end

type binder_set = VarSet.t
type scheme = S of binder_set * Ty.typ

module Scheme : sig
  type t = scheme

  val apply : (fresh, Ty.typ) Base.Map.Poly.t -> scheme -> scheme
end

type environment = (string * scheme) list

module TypeEnv : sig
  type t = environment

  val extend : 'a list -> 'a -> 'a list
  val empty : 'a list

  val apply
    :  (fresh, Ty.typ) Base.Map.Poly.t
    -> ('a, scheme) Base.List.Assoc.t
    -> ('a, scheme) Base.List.Assoc.t
end

val unify : Ty.typ -> Ty.typ -> (int, Ty.typ) Base.Map.Poly.t R.t
val fresh_var : Ty.typ R.t
val instantiate : scheme -> Ty.typ R.t

val lookup_env
  :  string
  -> (string, scheme) Base.List.Assoc.t
  -> (('a, 'b) Base.Map.Poly.t * Ty.typ) R.t

val infer : TypeEnv.t -> Ast.struct_prog -> (Subst.t * Ty.typ) R.t
val empty : environment
val check_type : TypeEnv.t -> Ast.struct_prog -> (TypeEnv.t * Ty.typ) R.t
val check_types : TypeEnv.t -> Ast.struct_prog list -> (TypeEnv.t * Ty.typ) R.t

val check_types
  :  ?env:environment
  -> Ast.struct_prog list
  -> (TypeEnv.t * Ty.typ, error_infer) result

val run_infer : ('a * Ty.typ, error_infer) result -> unit
