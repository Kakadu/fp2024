(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Units_of_measure

(** [parse_const_u] parses any (unsigned) constant and returns it as a constant type.
    Should be used for expression parsing. Cannot parse surrouning whitespaces. *)
let parse_const_u =
  choice
    [ (parse_char >>| fun c -> Const_char c)
    ; (parse_string >>| fun s -> Const_string s)
    ; (parse_bool >>| fun b -> Const_bool b)
    ; (puom >>| fun m -> Const_unit_of_measure m)
    ; (parse_int >>| fun i -> Const_int i)
    ; (parse_float >>| fun f -> Const_float f)
    ]
;;

(** [parse_const_s] parses any (probably signed) constant and returns it as a constant type.
    Should be used for pattern parsing. Cannot parse surrouning whitespaces. *)
let parse_const_s =
  choice
    [ (parse_char >>| fun c -> Const_char c)
    ; (parse_string >>| fun s -> Const_string s)
    ; (parse_bool >>| fun b -> Const_bool b)
    ; (puom >>| fun m -> Const_unit_of_measure m)
    ; (parse_sint >>| fun i -> Const_int i)
    ; (parse_sfloat >>| fun f -> Const_float f)
    ]
;;
