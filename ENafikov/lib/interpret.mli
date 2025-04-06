(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX_License-Identifier: LGPL-3.0-or-later *)

type error_inter =
  | DivisionByZero
  | UnboundValue of string
  | UnboundConstructor of string
  | MatchFailure
  | TypeError
  | Unreachable
  | StringOfLengthZero of string
  | EmptyProgram
  | NotImplemented

val pp_error_inter : Format.formatter -> error_inter -> unit

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VFun of Ast.pattern * Ast.expr * (string * value) list
  | VLetWAPat of string * value
  | VNil

val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

module type MonadFail = sig
  type ('a, 'e) t

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'b) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'b) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end

  module Monad_infix : sig
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

val is_constr : char -> bool

type environment = (string, value, Base.String.comparator_witness) Base.Map.t

module Environment : functor (M : MonadFail) -> sig
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val find_val : (string, 'a, 'b) Base.Map.t -> string -> ('a, error_inter) M.t
  val add_bind : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val add_binds : ('a, 'b, 'c) Base.Map.t -> ('a * 'b) list -> ('a, 'b, 'c) Base.Map.t
end

module Interpret : functor (M : MonadFail) -> sig
  val bind_fun_params
    :  ?env:(string, value, Base.String.comparator_witness) Base.Map.t
    -> Ast.pattern * value
    -> ((string * value) list, error_inter) M.t

  val eval_binding
    :  Ast.struct_prog
    -> (string, value, Base.String.comparator_witness) Base.Map.t
    -> (value, error_inter) M.t

  val eval
    :  Ast.expr
    -> (string, value, Base.String.comparator_witness) Base.Map.t
    -> (value, error_inter) M.t

  val eval_program : Ast.struct_prog list -> (value, error_inter) M.t
end

module InterpretResult : sig
  val bind_fun_params
    :  ?env:(string, value, Base.String.comparator_witness) Base.Map.t
    -> Ast.pattern * value
    -> ((string * value) list, error_inter) result

  val eval_binding
    :  Ast.struct_prog
    -> (string, value, Base.String.comparator_witness) Base.Map.t
    -> (value, error_inter) result

  val eval
    :  Ast.expr
    -> (string, value, Base.String.comparator_witness) Base.Map.t
    -> (value, error_inter) result

  val eval_program : Ast.struct_prog list -> (value, error_inter) result
end
