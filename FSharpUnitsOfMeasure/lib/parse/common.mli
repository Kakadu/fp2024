(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom

(** [' '], ['\n'] or ['\r']. *)
val is_whitespace : char -> bool

(** returns true if operator is builtin, false otherwise *)
val is_builtin_op : string -> bool

(** [skip_ws] skips 0 or more whitespaces, newlines included *)
val skip_ws : unit t

(** [skip_ws_nl] skips 0 or more whitespaces, fails if ['\n'] wasn't met *)
val skip_ws_nl : unit t

(** [skip_ws_no_nl] skips 0 or more whitespaces, stops when ['\n'] is met *)
val skip_ws_no_nl : unit t

(** [skip_ws1] skips 1 or more whitespaces. *)
val skip_ws1 : unit t

(** [skip_token str] skips [str] exactly, separated
    with 0 or more whitespaces before and after it. *)
val skip_token : string -> unit t

(** [is_id_char] returns true if char can be used in an identificator. *)
val is_id_char : char -> bool

(** [is_id_stchar] returns true if char can be used
    as a first char of an identificator. *)
val is_id_stchar : char -> bool

(** [pid] parses identificator and returns it. *)
val pid : string t

(** [pid_or_op] parses identificator or an operator inside parentheses and returns it. *)
val pid_or_op : string t

(** [pchar] parses char with single quotes and returns it. Cannot parse surrouning whitespaces. *)
val pchar : char t

(** [pstring] parses string with double quotes and returns it. Cannot parse surrouning whitespaces. *)
val pstring : string t

(** [pbool] parses bool and returns it. Cannot parse surrouning whitespaces. *)
val pbool : bool t

(** [pint] parses unsigned int and returns it. Should be used for expression parsing. Cannot parse surrouning whitespaces. *)
val pint : int t

(** [psint] parses signed int and returns it. Should be used for pattern parsing. Cannot parse surrouning whitespaces. *)
val psint : int t

(** [pfloat] parses unsigned float and returns it. Should be used for expression parsing. Cannot parse surrouning whitespaces. *)
val pfloat : float t

(** [psfloat] parses signed float and returns it. Should be used for pattern parsing. Cannot parse surrouning whitespaces. *)
val psfloat : float t

(** [chainl pa psep] runs [pa]'s with [psep]'s in betweens,
    applying [psep]'s returned functions left-associatively. *)
val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a t

(** [chainr pa psep] runs [pa]'s with [psep]'s in betweens,
    applying [psep]'s returned functions right-associatively. *)
val chainr : 'a t -> ('a -> 'a -> 'a) t -> 'a t
