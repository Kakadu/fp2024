(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val parse_stmt : block t -> stmt t
val parse_block : block t
val parse_long_var_decl : block t -> long_var_decl t
