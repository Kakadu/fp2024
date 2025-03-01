open InferTypes

module Type : sig
  type t = Ast.TypeExpr.t

  val occurs_check : string -> t -> bool
  val free_vars : t -> VarSet.t
end

module Scheme : sig
  type t = scheme

  val occurs_check : string -> t -> bool
  val free_vars : t -> VarSet.t
  val apply : (string, Type.t, Base.String.comparator_witness) Base.Map.t -> t -> t
  val pp_scheme : Format.formatter -> t -> unit
end

module TypeEnv : sig
  type t = (string, Scheme.t, Base.String.comparator_witness) Base.Map.t

  val extend : t -> string -> Scheme.t -> t
  val empty : t
  val fold : (string -> Scheme.t -> 'a -> 'a) -> 'a -> t -> 'a
  val free_vars : t -> VarSet.t
  val apply : (string, Type.t, Base.String.comparator_witness) Base.Map.t -> t -> t
  val find : string -> t -> Scheme.t option
  val find_exn : string -> t -> Scheme.t
  val remove : (string, 'a, 'b) Base.Map.t -> string -> (string, 'a, 'b) Base.Map.t
  val pp_env : Format.formatter -> t -> unit
end

val env_with_things : TypeEnv.t

val run_infer_program
  :  ?debug:bool
  -> Ast.program
  -> TypeEnv.t
  -> (TypeEnv.t, InferTypes.error) Result.t

