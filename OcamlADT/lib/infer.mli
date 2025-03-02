open InferTypes

module Type : sig
  type t = Ast.TypeExpr.t

  val occurs_check : string -> t -> bool
  val free_vars : t -> VarSet.t
end

module Substitution : sig
  type t = (string, Type.t, Base.String.comparator_witness) Base.Map.t
end

module TypeEnv : sig
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  val pp_env : Format.formatter -> t -> unit
end

val env_with_things : TypeEnv.t

val run_infer_program
  :  ?debug:bool
  -> Ast.program
  -> TypeEnv.t
  -> (TypeEnv.t * string list, InferTypes.error) Result.t
