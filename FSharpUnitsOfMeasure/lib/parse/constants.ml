(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast
open Common
open Units_of_measure

let puconst =
  choice
    [ (pchar >>| fun c -> Const_char c)
    ; (pstring >>| fun s -> Const_string s)
    ; (pbool >>| fun b -> Const_bool b)
    ; (puom >>| fun m -> Const_unit_of_measure m)
    ; (pint >>| fun i -> Const_int i)
    ; (pfloat >>| fun f -> Const_float f)
    ]
;;

let psconst =
  choice
    [ (pchar >>| fun c -> Const_char c)
    ; (pstring >>| fun s -> Const_string s)
    ; (pbool >>| fun b -> Const_bool b)
    ; (puom >>| fun m -> Const_unit_of_measure m)
    ; (psint >>| fun i -> Const_int i)
    ; (psfloat >>| fun f -> Const_float f)
    ]
;;
