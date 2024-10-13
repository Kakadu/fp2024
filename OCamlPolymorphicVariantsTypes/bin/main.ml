(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

let () =
  print_endline
    (string_of_program_parse_result
       (parse
          program_parser
          {|
        let rec factorial n = if (n > 1) then n * factorial(n-1) else 1;;
        factorial 5;;
        |}))
;;
