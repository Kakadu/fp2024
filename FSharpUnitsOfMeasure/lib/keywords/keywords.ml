(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains a subset of keywords of F# 4.1, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 25 *)

let is_keyword = function
  | "and"
  | "else"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "or"
  | "rec"
  | "then"
  | "true"
  | "type"
  | "with"
  | "Some"
  | "None" -> true
  | _ -> false
;;

let is_builtin_type = function
  | "int" | "bool" | "float" | "char" | "string" -> true
  | _ -> false
;;
