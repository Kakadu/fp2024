(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

(** [fail] creates a parser that will always fail without message *)
val fail : 'a t

(** [fail_if cond] creates a parser that will fail without message if [cond] is [true] *)
val fail_if : bool -> unit t

(** [ws] accepts whitespace ([' '], ['\t'], ['\n'], ['\r']) or comments such as
    [/* block comment */] and [// line comment \n]
    {i zero} or more times, discarding the results. *)
val ws : unit t

(** [ws_line] accepts spaces or tabs ([' '], ['\t']) or block comments
    [/* block comment */] {i zero} or more times, discarding the results. *)
val ws_line : unit t

(** [token string] creates parser that skips [ws_line], parses [string]
    skips [ws] and returns string *)
val token : string -> string t

(** [parens p] creates parser that parses ['('], runs [p],
    parses [')'] and returns result of a [p] *)
val parens : 'a t -> 'a t

(** [square_brackets p] creates parser that parses ['['], runs [p],
    parses [']'] and returns result of a [p] *)
val square_brackets : 'a t -> 'a t

(** [curly_braces p] creates parser that parses one ['{'], runs [p],
    parses ['}'] and returns result of a [p] *)
val curly_braces : 'a t -> 'a t

(** [sep_by_comma p] runs [p] {i zero} or more times,
    interspersing runs of [char ','] in between. *)
val sep_by_comma : 'a t -> 'a list t

(** [sep_by_comma p] runs [p] {i one} or more times,
    interspersing runs of [char ','] in between. *)
val sep_by_comma1 : 'a t -> 'a list t

(** [parse_int] parses {i one} or more digits and returns non-negative integer number *)
val parse_int : int t

(** [parse_stmt_sep ] parses separator for the statements,
    it runs [ws_line], then parses [;] or [\n], the runs [ws], and discards the results *)
val parse_stmt_sep : unit t

(** [parse_ident] parses identifiers that can be used as veriables and function names.
    Identifiers has to start with latin letter or ['_']
    and consist only of latin letters, digits and ['_'] *)
val parse_ident : ident t

(** [parse_type] parses integer, boolean, string, array, function and channel types sucn as:
    [int], [bool], [string], [[3]int], [func()], [func(int) (string, bool)],
    [chan int], [<-chan int], [chan<- int], [chan chan bool] *)
val parse_type : type' t
