(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Interpreter error type *)
type error =
  | DivisionByZero
  | TypeMismatch
  | UnboundVariable of string
  | PatternMismatch
  | RecursionError
  | EmptyProgram
  | ParserError
  | NotAnADT of string
  | NotAnADTVariant of string
  | UndefinedConstructor of string
  | UndefinedArgs

(** Runtime value type *)
type value =
  | VInt of int
  | VString of string
  | VChar of char
  | VBool of bool
  | VTuple of value List2.t
  | VFun of Pattern.t List1.t * Expression.t * environment * Expression.rec_flag
  | VFunction of Expression.t Expression.case List1.t * environment
  | VConstruct of ident * value option
  | VAdt of (value * ident list * ident * (ident * TypeExpr.t option) List1.t)
  | VUnit
  | VType of TypeExpr.t * ident option
  | VBuiltin_binop of (value -> value -> (value, error) Result.t)
  | VBuiltin_print of (value -> (value, error) Result.t)

(** Environment: a map from variable names to values *)
and environment = (string, value, Base.String.comparator_witness) Base.Map.t

(** Utility functions *)
val compare_values : value -> value -> bool

val list1_to_list2 : 'a List1.t -> ('a * 'a * 'a list) option

(** Create a List1.t from a standard list, if the list is nonempty. *)
val make_list1 : 'a list -> ('a * 'a list) option

(** Convert a value to a boolean; raises [Invalid_argument "TypeMismatch"] if the value is not convertible. *)
val to_bool : value -> bool

(** Module type for an error monad. *)
module type Error_monad = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

(** [Interpreter(M)] is the interpreter functor using monad [M]. *)
module Interpreter (M : Error_monad) : sig
  val interpret_program : program -> ((ident option * value) list, error) M.t
end

(** [RESULT_MONAD_ERROR] is an implementation of [Error_monad] using ocaml's [Result]. *)
module RESULT_MONAD_ERROR : Error_monad

(** [InterpreterWResult] is the interpreter instantiated with [RESULT_MONAD_ERROR]. *)
module InterpreterWResult : sig
  include module type of Interpreter (RESULT_MONAD_ERROR)
end

(** [run_interpreter] runs the interpreter on a [program]. *)
val run_interpreter : program -> ((ident option * value) list, error) result

(** Pretty printer functions for interpreter values and errors. *)
module PPrinter : sig
  val pp_value : Stdlib.Format.formatter -> value -> unit
  val pp_error : Stdlib.Format.formatter -> error -> unit
  val print_error : error -> unit
end
