(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Parsing_Error of string
  | `Some_Error
  ]

val pp_error : Format.formatter -> error -> unit
val parse : string -> (Ast.expr, error) result
