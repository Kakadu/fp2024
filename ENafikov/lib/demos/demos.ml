(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Interpret
open Parser

let run input =
  match main_parse input with
  | Ok ast ->
    (match InterpretResult.eval_program ast with
     | Ok res -> Stdlib.Format.printf "%s" (show_value res)
     | Error e -> Stdlib.Format.printf "(Error while interpreting): %a" pp_error_inter e)
  | Error e -> Stdlib.Format.printf "(Error while parsing): %s" e
;;

let () = run (Stdio.In_channel.input_all Caml.stdin)
