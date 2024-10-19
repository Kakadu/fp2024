(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Ast
open Angstrom
open Common

let parse_wild = char '_' <* (peek_char_fail >>| Char.is_whitespace)

let parse_pat =
  choice
    [ (parse_ident >>| fun i -> Pattern_ident i)
    ; (parse_wild >>| fun _ -> Pattern_wild)
    ; (parse_const >>| fun c -> Pattern_const c)
    ]
;;
