(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast

(** [' '], ['\n'] or ['\r'] *)
val is_whitespace : char -> bool

(** [skip_ws] skips 0 or more whitespaces *)
val skip_ws : unit t

(** [skip_ws1] skips 1 or more whitespaces *)
val skip_ws1 : unit t

(** [skip_token str] skips [str] exactly, separated
    with 0 or more whitespaces before and after it *)
val skip_token : string -> unit t

(** [is_keyword] returns true if given string is a keyword of F# *)
val is_keyword : string -> bool

(** [is_ident_char] returns true if char can be used in an identificator *)
val is_ident_char : char -> bool

(** [is_ident_char] returns true if char can be used
    as a first char of an identificator *)
val is_ident_start_char : char -> bool

(** [parse_ident] parses identificator and returns it *)
val parse_ident : string t

(** [parse_const] parses constant and returns it. Cannot parse whitespaces. *)
val parse_const : constant t
