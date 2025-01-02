(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Impossible_error
  | `No_variable_rec
  | `No_arg_rec
  | `Bound_several_times
  | `Occurs_check of string * Ast.core_type
  | `No_variable of string
  | `Unification_failed of Ast.core_type * Ast.core_type
  ]

val pp_error : Format.formatter -> error -> unit

module State : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold : ('a, 'b, 'c) Base.Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) result
end

module VarSet : sig
  type elt = string
  type t = Set.Make(String).t

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
  val compare : t -> t -> int
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
  val cardinal : t -> int
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
  val fold_left_m : ('a -> elt -> 'a State.t) -> t -> 'a State.t -> 'a State.t
  val pp : Format.formatter -> t -> unit
end

type scheme = Scheme of VarSet.t * Ast.core_type

val pp_scheme : Format.formatter -> scheme -> unit
val show_scheme : scheme -> string

module Type : sig
  type t = Ast.core_type

  val occurs_in : string -> Ast.core_type -> bool
  val free_vars : Ast.core_type -> VarSet.t
end

module Subst : sig
  type t

  val empty : t
  val singleton : string -> Ast.core_type -> t State.t
  val apply : t -> Ast.core_type -> Ast.core_type
  val unify : Ast.core_type -> Ast.core_type -> t State.t
  val compose : t -> t -> t State.t
  val compose_all : t list -> t State.t
  val remove : t -> string -> t
end

module Scheme : sig
  type t = scheme

  val occurs_in : string -> scheme -> bool
  val free_vars : scheme -> VarSet.t
  val apply : Subst.t -> scheme -> scheme
  val pp : Format.formatter -> scheme -> unit
end

module TypeEnv : sig
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val free_vars : ('a, scheme, 'b) Base.Map.t -> VarSet.t
  val apply : Subst.t -> ('a, scheme, 'b) Base.Map.t -> ('a, scheme, 'b) Base.Map.t
  val pp : Format.formatter -> ('a, string * scheme, 'b) Base.Map.t -> unit
  val find : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b option
end

module Infer : sig
  val unify : Ast.core_type -> Ast.core_type -> Subst.t State.t
  val fresh_var : Ast.core_type State.t
  val instantiate : scheme -> Ast.core_type State.t

  val generalize
    :  TypeEnv.t
    -> Ast.core_type
    -> remove_from_env:bool
    -> Ast.ident option
    -> scheme

  val lookup_env
    :  string
    -> (string, scheme, 'a) Base.Map.t
    -> (Subst.t * Ast.core_type) State.t

  val infer_pattern : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * Ast.core_type) State.t

  val infer_expression
    :  TypeEnv.t
    -> Ast.Expression.t
    -> (Subst.t * Ast.core_type) State.t

  val infer_srtucture_item : TypeEnv.t -> Ast.structure_item list -> TypeEnv.t State.t
end

val env_with_print_int : TypeEnv.t
val run_inferencer : Ast.structure_item list -> TypeEnv.t -> (TypeEnv.t, error) result
