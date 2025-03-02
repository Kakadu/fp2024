[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Composition of state and error monads *)
module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : Types.error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t

    (* Map functions by Homka122 😼😼😼 *)
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
    val fmap : 'a list -> f:('a -> 'b) -> 'b list t
    val map2 : 'a list -> 'b list -> f:('a -> 'b -> 'c t) -> 'c list t
    val fmap2 : 'a list -> 'b list -> f:('a -> 'b -> 'c) -> 'c list t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> int -> ('a, Types.error) Result.t
end

(** Type Environment representing variables and their schemes *)
module TypeEnv : sig
  type t

  val operators : (string list * Types.typ) list
  val pp : Format.formatter -> t -> unit
  val pp_names : string list -> Format.formatter -> t -> unit
  val print : ?name:string -> t -> unit
end

(** Type Environment with built-in variables (functions, constructors, etc...) *)
val defaultEnv : TypeEnv.t

(** Run inferencer to expression *)
val run_expr_inferencer : Ast.expression -> (Types.typ, Types.error) result

(** Run inferencer to structure *)
val run_structure_inferencer
  :  ?env:TypeEnv.t
  -> Ast.structure_item
  -> (TypeEnv.t * string list, Types.error) result

val run_structure_inferencer_exn
  :  ?env:TypeEnv.t
  -> Ast.structure_item
  -> (TypeEnv.t * string list, string) result

(** Run inferencer to program *)
val run_program_inferencer
  :  ?env:TypeEnv.t
  -> Ast.structure_item list
  -> (TypeEnv.t * string list, Types.error) result

(** Run inferencer to program, but error is formatted *)
val run_program_inferencer_exn
  :  ?env:TypeEnv.t
  -> Ast.structure_item list
  -> (TypeEnv.t * string list, string) result
