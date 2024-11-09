(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Units_of_measure

(** [parse_const] parses any constant and returns it as a constant type. Cannot parse surrouning whitespaces. *)
let parse_const =
  choice
    [ (parse_char >>| fun c -> Const_char c)
    ; (parse_string >>| fun s -> Const_string s)
    ; (parse_bool >>| fun b -> Const_bool b)
    ; (parse_unit_of_measure >>| fun u -> Const_unit_of_measure u)
    ; (parse_int >>| fun i -> Const_int i)
    ; (parse_float >>| fun f -> Const_float f)
    ]
;;
