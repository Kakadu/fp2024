(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(* A subset of keywords of F# 4.1 is taken from https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 25 *)

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

(** [is_builtin_op op] returns true if [s] is a builtin operation of F#. *)
let is_builtin_op = function
  | "+"
  | "-"
  | "*"
  | "/"
  | "<="
  | "<"
  | ">="
  | ">"
  | "="
  | "<>"
  | "||"
  | "&&"
  | "+."
  | "-."
  | "*."
  | "/."
  | "::" -> true
  | _ -> false
;;

(** [is_builtin_fun s] returns true if [s] is a builtin function of F#. *)
let is_builtin_fun = function
  | "print_int"
  | "print_bool"
  | "print_float"
  | "print_string"
  | "print_char"
  | "print_endline" -> true
  | _ -> false
;;
