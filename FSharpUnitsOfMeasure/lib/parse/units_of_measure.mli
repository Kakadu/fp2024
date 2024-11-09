(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 309 *)

open Base
open Angstrom
open Ast

(** [parse_measure_num] parses number before unit of measure and returns it. *)
val parse_measure_num : measure_num t

(** [parse_measure] parses identifiers, products, quotients and powers as measures. *)
val parse_measure : measure t

(** [parse_unit_of_measure] parses int or float number, then parses a measure in
    angular brackets and returns them as a unit of measure. *)
val parse_unit_of_measure : unit_of_measure t
