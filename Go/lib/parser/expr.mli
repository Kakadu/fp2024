(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val parse_expr : block t -> expr t
val parse_func_args_returns_and_body : block t -> anon_func t
val parse_index : 'a t -> 'b -> ('b * 'a) t
