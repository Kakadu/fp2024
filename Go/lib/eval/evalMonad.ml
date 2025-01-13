(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

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

and var_map = value MapIdent.t

and func_value =
  | Func_initialized of var_map * anon_func
  (** varMap stores variables, to which the function is bounded if it is a clojure *)
  | Func_uninitialized of nil

(** Stores values for 1) predeclared identifiers, 2) global variables and functions *)
type global_env =
  { builtins : var_map
  ; toplevel : var_map
  }

type stack_frame =
  { caller : stack_frame option
  (** Function, that called current function and where it will return to.
      [None] for [main()] and function called via [go] *)
  ; local_envs : var_map list
  ; deferred_funcs : stack_frame list
  }

type goroutine_state =
  | Running
  | Ready
  (** State of the goroutine that doesn't try to receive from or send to a chanel, but another goroutine is running *)
  | Sending of ident (** State of goroutine that is trying to send to a chanel *)
  | Recieving of ident (** State of goroutine that is trying to receive from a chanel *)

type goroutine =
  { call_stack : stack_frame (** Stack of separate goroutine's local calls *)
  ; state : goroutine_state
  }

type eval_state =
  { global_env : global_env
  ; running_goroutine : goroutine
  ; sleeping_goroutines : goroutine list
  }
