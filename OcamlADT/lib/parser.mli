(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast
open Angstrom
(** 
  Parses a given input string into a list of structure items.

  @param str The input string to parse.
  @return An [Ok] containing the parsed structure or an [Error] message.
*)
val parse : string -> (program, string) result
val parse_str : string -> program