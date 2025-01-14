(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident : sig
  type t = ident

  val compare : t -> t -> int
end

module MapIdent : Map.S with type key = Ident.t

(** Value for [nil] identifier and unitialized functions and channels *)
type nil

(** Value for unbuffered channels *)
type chan_value =
  | Chan_initialized of bool (** Initialized channel, may either be opened or closed *)
  | Chan_uninitialized of nil

(** Values that can be stored in a variables *)
type value =
  | Value_int of int (** [3], [-100] *)
  | Value_string of string (** ["my_string"] *)
  | Value_bool of bool (** [true], [false] *)
  | Value_array of int * value list
  (** Array of values, invariant: number of values matches the size *)
  | Value_func of func_value
  | Value_chan of chan_value
  | Value_nil of nil
  (** Untyped [<nil>] value that is stored in [nil] predeclared identifier *)

and func_value =
  | Func_initialized of value MapIdent.t * anon_func
  (** varMap stores variables, to which the function is bounded if it is a clojure *)
  | Func_uninitialized of nil

type stack_frame =
  { local_envs : value MapIdent.t list
  (** Storage for local variables, new [{}] block creates new environment *)
  ; expr_eval : value list (** Stack for expression evaluation *)
  ; deferred_funcs : stack_frame list
  }

type goroutine_state =
  | Running
  | Ready
  (** State of the goroutine that doesn't try to receive from or send to a chanel, but another goroutine is running *)
  | Sending of ident (** State of goroutine that is trying to send to a chanel *)
  | Recieving of ident (** State of goroutine that is trying to receive from a chanel *)

type goroutine =
  { stack : stack_frame list * stack_frame
  (** Stack of separate goroutine's local func calls. Is a tuple because there is always a root func *)
  ; state : goroutine_state
  ; id : int
  }

type eval_state =
  { global_env : value MapIdent.t
  (** Stores values for predeclared identifiers and global variables and functions *)
  ; running : goroutine
  (** Goroutine that is currently running, stored separately for time efficiency *)
  ; sleeping : goroutine list (** All sleeping and ready to run goroutines *)
  }

(** Monad for evaluating the program state *)
module EvalMonad : sig
  (** ['a t] is an interpreter that stores current state
      and the result of evaluation - ['a] (['a] or runtime error) *)
  type 'a t

  val return : 'a -> 'a t
  val fail : Errors.error -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter2 : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val run : 'a t -> eval_state -> eval_state * ('a, Errors.error) Result.t

  (* Global environment *)
  val read_global : value MapIdent.t t
  val save_global_id : ident -> value -> unit t

  (* Goroutines *)
  val read_sleeping : goroutine list t
  val add_sleeping : goroutine -> unit t
  val read_running : goroutine t
  val write_running : goroutine -> unit t

  (* Call stack *)
  val add_stack_frame : stack_frame -> unit t
  val delete_stack_frame : unit t

  (* Local environments *)
  val save_local_id : ident -> value -> unit t
  val add_env : unit t
  val delete_env : unit t

  (* Deferred functions *)
  val add_deferred : stack_frame -> unit t
  val delete_deferred : unit t

  (* Expr eval stack *)
  val push_value : value -> unit t
  val pop_value : value t
end
