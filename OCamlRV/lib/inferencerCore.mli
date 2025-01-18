(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val fun_type : Ast.type_annot -> Ast.type_annot -> Ast.type_annot
val int_type : Ast.type_annot
val bool_type : Ast.type_annot
val string_type : Ast.type_annot
val unit_type : Ast.type_annot
val tuple_type : Ast.type_annot list -> Ast.type_annot
val list_type : Ast.type_annot -> Ast.type_annot

type error =
  [ `Occurs_check
  | `Unbound of string
  | `Unification_failed of Ast.type_annot * Ast.type_annot
  | `LeftHS
  ]

val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  type elt = int
  type t = Set.Make(Base.Int).t

  val empty : t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val diff : t -> t -> t
end

module Result : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) result

  module RMap : sig
    val fold : ('a, 'b, 'c) Base.Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end

module Type : sig
  val occurs_in : int -> Ast.type_annot -> bool
  val free_vars : Ast.type_annot -> VarSet.t
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> Ast.type_annot -> t Result.t
  val find : t -> int -> Ast.type_annot option
  val remove : t -> int -> t
  val apply : t -> Ast.type_annot -> Ast.type_annot
  val unify : Ast.type_annot -> Ast.type_annot -> t Result.t
  val compose : t -> t -> t Result.t
  val compose_all : t list -> t Result.t
end

module Scheme : sig
  type t = S of VarSet.t * Ast.type_annot

  val occurs_in : int -> t -> bool
  val free_vars : t -> VarSet.t
  val apply : Subst.t -> t -> t
end

module TypeEnv : sig
  type t = (string, Scheme.t, Base.String.comparator_witness) Base.Map.t

  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val remove : ('a, 'b, 'c) Base.Map.t -> 'a -> ('a, 'b, 'c) Base.Map.t
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val free_vars : t -> VarSet.t
  val apply : Subst.t -> ('a, Scheme.t, 'b) Base.Map.t -> ('a, Scheme.t, 'b) Base.Map.t
  val find : 'a -> ('a, 'b, 'c) Base.Map.t -> 'b option
end
