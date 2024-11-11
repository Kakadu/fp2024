(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open TopLevel
open Ast

val parse : 'a t -> string -> 'a
val pp_repl : string -> unit
