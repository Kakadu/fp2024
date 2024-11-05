(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 309 *)

open Base
open Angstrom
open Ast
open Common

let parse_measure_num_int = parse_int >>| fun i -> Mnum_int i

let parse_measure_num_float = parse_float >>| fun f -> Mnum_float f

let parse_measure_num = parse_measure_num_int <|> parse_measure_num_float
