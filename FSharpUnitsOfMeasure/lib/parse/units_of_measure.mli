(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 309 *)

open Base
open Angstrom
open Ast

(** [pm_num] parses number before unit of measure and returns it. *)
val pm_num : measure_num t

(** [pm] parses identifiers, products, quotients and powers as measures. *)
val pm : measure t

(** [puom] parses int or float number, then parses a measure in
    angular brackets and returns them as a unit of measure. *)
val puom : unit_of_measure t
