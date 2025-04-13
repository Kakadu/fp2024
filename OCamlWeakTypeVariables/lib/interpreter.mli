(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | Type_error
  | Pattern_error of Ast.pattern
  | Eval_expr_error of Ast.expression
  | No_variable of string
  | Match_error

module Res : sig
  type 'a t

  val fail : error -> 'a t
  val return : 'a -> 'a t
  val run : 'a t -> ('a, error) result
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module rec Value : sig
  type value =
    | Val_integer of int
    | Val_string of string
    | Val_boolean of bool
    | Val_fun of Ast.pattern * Ast.expression * EvalEnv.t
    | Val_rec_fun of string * value
    | Val_function of Ast.case list * EvalEnv.t
    | Val_tuple of value list
    | Val_construct of string * value option
    | Val_builtin of string

  val pp : Format.formatter -> value -> unit
end

and EvalEnv : sig
  type t

  val empty : t
  val extend : t -> string -> Value.value -> t
  val compose : t -> t -> t
  val find_exn : t -> string -> Value.value Res.t
  val find_exn1 : t -> string -> Value.value
end

val pp_error : Format.formatter -> error -> unit
val run_interpret : Ast.structure_item list -> EvalEnv.t Res.t
val run_interpret_exn : Ast.structure_item list -> (EvalEnv.t, string) result
