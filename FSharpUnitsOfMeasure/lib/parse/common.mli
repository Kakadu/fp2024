(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast

(** [' '], ['\n'] or ['\r']. *)
val is_whitespace : char -> bool

(** [skip_ws] skips 0 or more whitespaces. *)
val skip_ws : unit t

(** [skip_ws1] skips 1 or more whitespaces. *)
val skip_ws1 : unit t

(** [skip_token str] skips [str] exactly, separated
    with 0 or more whitespaces before and after it. *)
val skip_token : string -> unit t

(** [is_keyword] returns true if given string is a keyword of F#. *)
val is_keyword : string -> bool

(** [is_ident_char] returns true if char can be used in an identificator. *)
val is_ident_char : char -> bool

(** [is_ident_char] returns true if char can be used
    as a first char of an identificator. *)
val is_ident_start_char : char -> bool

(** [parse_ident] parses identificator and returns it. *)
val parse_ident : string t

(** [parse_ident_or_op] parses identificator or an operator inside parentheses and returns it. *)
val parse_ident_or_op : string t

(** [parse_char] parses char with single quotes and returns it. Cannot parse surrouning whitespaces. *)
val parse_char : char t

(** [parse_string] parses string with double quotes and returns it. Cannot parse surrouning whitespaces. *)
val parse_string : string t

(** [parse_bool] parses bool and returns it. Cannot parse surrouning whitespaces. *)
val parse_bool : bool t

(** [parse_int] parses int and returns it. Cannot parse surrouning whitespaces. *)
val parse_int : int t

(** [parse_float] parses float and returns it. Cannot parse surrouning whitespaces. *)
val parse_float : float t

(** [chainl parse_alpha parse_sep] runs [parse_alpha]'s with [parse_sep]'s in betweens,
    applying [parse_sep]'s returned functions left-associatively. *)
val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a t

(** [chainr parse_alpha parse_sep] runs [parse_alpha]'s with [parse_sep]'s in betweens,
    applying [parse_sep]'s returned functions right-associatively. *)
val chainr : 'a t -> ('a -> 'a -> 'a) t -> 'a t
