(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom

(** [parse_stmt pblock] returns parser for any statement such as
    [call()], [i++], [c := 9], [if true {} else {}], etc. *)
val parse_stmt : block t -> stmt t

(** [parse_block] parses block of statements such as: [{}]
    [{
        a := 0
        call(a)
     }] *)
val parse_block : block t

(** [parse_long_var_decl pblock] returns parser for long variable declaration such as:
    [var a int], [var a = 5], [var a, b, c = 1, "str", call()] *)
val parse_long_var_decl : block t -> long_var_decl t
