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

type func_value =
  | Func_initialized of anon_func
  | Func_uninitialized of nil

type chan_value =
  | Chan_initialized of bool
  | Chan_uninitialized of nil

(** Values that can be stored in a variables *)
type value =
  | Value_int of int (** [3], [-100] *)
  | Value_string of string (** ["my_string"] *)
  | Value_bool of bool (** [true], [false] *)
  | Value_array of int * value list
  (** Array of values, number of values matches the size *)
  | Value_func of func_value
  | Value_chan of chan_value
  (** Value for unbuffered channel, may either be opened or closed *)
  | Value_nil of nil
  (** Untyped [<nil>] value that is stored in [nil] predeclared identifier *)

type varMap = value MapIdent.t

(** Stores values for 1) predeclared identifiers, 2) global variables and functions *)
type global_env = varMap * varMap

(** Stack of separate goroutine's local environments *)
type loval_env = varMap list
