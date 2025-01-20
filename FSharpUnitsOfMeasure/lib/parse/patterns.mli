(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast

(** [ppat] accepts pattern and returns it *)
val ppat : pattern t

(** [ppat_wild] accepts [ _ ] pattern and returns it *)
val ppat_wild : pattern t

(** [ppat_id_or_op] accepts identificator or operator pattern and returns it *)
val ppat_id_or_op : pattern t

(** [ppat_const] accepts constant pattern and returns it *)
val ppat_const : pattern t
