(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom

(** [parse_expr pblock] returns parser for any expressions
    such as [2], [var], [-(2 + var)], [<-call[0]()]*)
val parse_expr : block t -> expr t

(** [parse_func_args_returns_and_body pblock] returns
    parser for arguments, return values and body of a function such as:
    [() {}], [(a int) string { return "" }],
    [(a, b int, c string) (d, e bool) { 
        d, e := true, false;
        return 
    }] *)
val parse_func_args_returns_and_body : block t -> anon_func t

(** [parse_index pexpr array] creates a parser that runs [square_brackets pexpr],
    gets its result as [index] and returns [array, index]*)
val parse_index : 'a t -> 'b -> ('b * 'a) t
