(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val use_logging : bool
val log : ('a, Format.formatter, unit, unit) format4 -> 'a

type error =
  [ `No_variable of string
  | `Occurs_check
  | `Unapplicable_type of Ast.type_expr option * Typedtree.ty
  | `Unexpected_error
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  ]

val pp_error : Format.formatter -> error -> unit

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) result
end

type fresh = int

module Type : sig
  type t = Typedtree.ty

  val occurs_in : fresh -> Typedtree.ty -> bool
  val free_vars : Typedtree.ty -> Typedtree.binder_set
end

module Subst : sig
  type t

  val pp : Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> Typedtree.ty -> t R.t
  val find_exn : fresh -> t -> Typedtree.ty
  val find : fresh -> t -> Typedtree.ty option
  val apply : t -> Typedtree.ty -> Typedtree.ty
  val unify : Typedtree.ty -> Typedtree.ty -> t R.t
  val unify_with_tyexpr : Ast.type_expr option -> Typedtree.ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end

module VarSet : sig
  type elt = fresh
  type t = Typedtree.binder_set

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> fresh
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> fresh
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
  val pp : Format.formatter -> t -> unit
  val fold_left_m : ('a -> elt -> 'a R.t) -> t -> 'a R.t -> 'a R.t
end

module Scheme : sig
  type t = Typedtree.scheme

  val occurs_in : fresh -> Typedtree.scheme -> bool
  val free_vars : Typedtree.scheme -> VarSet.t
  val apply : Subst.t -> Typedtree.scheme -> Typedtree.scheme
  val pp : Format.formatter -> Typedtree.scheme -> unit
end

module TypeEnv : sig
  type t = (string * Typedtree.scheme) list

  val extend : 'a list -> 'a -> 'a list
  val empty : 'a list
  val free_vars : t -> VarSet.t

  val apply
    :  Subst.t
    -> ('a, Typedtree.scheme) Base.List.Assoc.t
    -> ('a, Typedtree.scheme) Base.List.Assoc.t

  val pp : Format.formatter -> (string * Typedtree.scheme) list -> unit
  val find_exn : string -> (string, 'a) Base.List.Assoc.t -> 'a
end

val unify : Typedtree.ty -> Typedtree.ty -> Subst.t R.t
val unify_with_tyexpr : Ast.type_expr option -> Typedtree.ty -> Subst.t R.t
val fresh_var : Typedtree.ty R.t
val instantiate : Typedtree.scheme -> Typedtree.ty R.t
val generalize : TypeEnv.t -> Type.t -> Scheme.t

val lookup_env
  :  string
  -> (string, Typedtree.scheme) Base.List.Assoc.t
  -> (Subst.t * Typedtree.ty) R.t

val pp_env : Subst.t -> Format.formatter -> (string * Typedtree.scheme) list -> unit
val uncover_item : 'a list -> 'a R.t
val infer : TypeEnv.t -> Ast.expr -> (Subst.t * Typedtree.ty list) R.t
val w : Ast.expr -> (Typedtree.ty list, error) result
val show_ty_list : Typedtree.ty list -> string
val test_infer : Ast.expr -> unit
val test_var : string -> (string, Typedtree.scheme) Base.List.Assoc.t -> unit
