(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast

(** [parse_pat] accepts pattern and returns it *)
val parse_pat : pattern t

(** [parse_pat_wild] accepts [ _ ] pattern and returns it *)
val parse_pat_wild : pattern t

(** [parse_pat_ident_or_op] accepts identificator or operator pattern and returns it *)
val parse_pat_ident_or_op : pattern t

(** [parse_pat_const] accepts constant pattern and returns it *)
val parse_pat_const : pattern t
