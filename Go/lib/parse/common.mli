(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val ws : unit t
val ws_line : unit t
val token : string -> string t
val parens : 'a t -> 'a t
val square_brackets : 'a t -> 'a t
val curly_braces : 'a t -> 'a t
val sep_by_comma : 'a t -> 'a list t
val sep_by_comma1 : 'a t -> 'a list t
val parse_int : int t

(** Parses separator for the statements, [;] or [\n], returns nothing *)
val parse_stmt_sep : unit t

(** Parses identifiers that can be used as lvalues in variable declarations
    and as function name (includes blank identifier [_]) *)
val parse_ident : ident t

val parse_type : type' t
