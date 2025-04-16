(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Interpret
open Parser

let run input =
  match main_parse input with
  | Ok ast ->
    (match InterpretResult.eval_program ast with
     | Ok res -> print_value res
     | Error e -> Stdlib.Format.printf "(Error while interpreting): %a" pp_error_inter e)
  | Error e -> Stdlib.Format.printf "(Error while parsing): %s" e
;;

let () =
  let filename =
    try Sys.argv.(1) with
    | _ ->
      prerr_endline "Usage: ./demos.exe <file.ml>";
      exit 1
  in
  let input = Stdio.In_channel.read_all filename in
  run input
;;
