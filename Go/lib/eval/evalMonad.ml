(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

(** Values that can be stored in a variables *)
type value =
  | Value_int of int (** [3], [-100] *)
  | Value_string of string (** ["my_string"] *)
  | Value_bool of bool (** [true], [false] *)
  | Value_array of int * value list
  (** Array of values, number of values matches the size *)
  | Value_func (* тут в каком-то виде надо хранить код функции *)
  | Value_chan of bool (** Value for unbuffered channel, may either be opened or closed *)
  | Value_nil (** Value for [nil] identifier and undeclared functions and channels *)

(** Stores values for predeclared identifiers, global variables and functions *)
type global_env = value MapIdent.t * value MapIdent.t

(** Stack of separate goroutine's local environments *)
type loval_env = value MapIdent.t list
