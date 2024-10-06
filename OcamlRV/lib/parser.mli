[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error = [ `Parsing_error of string ]

val pp_error : Format.formatter -> [< `Parsing_error of string ] -> unit

(** Main entry of parser *)
val parse : string -> (Ast.name Ast.t, [> error ]) result

type dispatch =
  { apps : dispatch -> Ast.name Ast.t Angstrom.t
  ; single : dispatch -> Ast.name Ast.t Angstrom.t
  }

(* A collection of miniparsers *)
val parse_lam : dispatch
