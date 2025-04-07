(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Forest.TypesTree

module FreshResult : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Subst : sig
  type t

  val empty : t
  val singleton : var -> typ -> t FreshResult.t
  val find : t -> var -> typ option
  val remove : t -> var -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t FreshResult.t
  val compose_many_sub : t list -> t FreshResult.t
  val compose : t -> t -> t FreshResult.t
end

module VarSet : sig
  type elt = int
  type t = Set.Make(Base.Int).t

  val empty : t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module Type : sig
  val has_type_var : int -> typ -> bool
  val type_vars : typ -> VarSet.t
end

type scheme = Scheme of VarSet.t * typ

module TypeEnv : sig
  type t = (id, scheme, Base.String.comparator_witness) Base.Map.t

  val empty : t
  val free : t -> VarSet.t
  val apply : Subst.t -> t -> t
  val extend : t -> id -> scheme -> t
  val find : t -> id -> scheme option
  val remove : t -> id -> t
end
