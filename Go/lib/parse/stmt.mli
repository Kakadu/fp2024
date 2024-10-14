(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val parse_stmt : stmt t

val parse_block: block t

val parse_var_decl_top_level: var_decl t