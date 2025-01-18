(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Common
open Expr
open Stmt
open TopLevel

(** [parse_ident] parses identifiers that can be used as veriables and function names.
    Identifiers has to start with latin letter or ['_']
    and consist only of latin letters, digits and ['_'] *)
let parse_ident = parse_ident

(** [parse_type] parses integer, boolean, string, array, function and channel types sucn as:
    [int], [bool], [string], [[3]int], [func()], [func(int) (string, bool)],
    [chan int], [<-chan int], [chan<- int], [chan chan bool] *)
let parse_type = parse_type

(** [parse_expr ] parses any expressions such as [2], [var], [-(2 + var)], [<-call[0]()]*)
let parse_expr = parse_expr parse_block

(** [parse_stmt] parses any statements such as
    [call()], [i++], [c := 9], [if true {} else {}], etc. *)
let parse_stmt = parse_stmt parse_block

(** [parse_file] parses the whole program *)
let parse_file = parse_file

(** [parse parser str] runs [parser] on [str] and returns [Ok result] if it succeds,
    where [result] is an AST, or [Error msg] if not *)
let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
