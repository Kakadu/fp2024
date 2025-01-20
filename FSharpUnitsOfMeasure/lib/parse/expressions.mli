(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast

(** [pexpr] parses expression and returns it *)
val pexpr : expression t

(** [pbind] parses one function or value binding and returns it *)
val pbind : expression t -> expression val_binding t
