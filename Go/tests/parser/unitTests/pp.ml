(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Ast

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> print_endline (printer res)
  | Error _ -> print_endline ": syntax error"
;;

let print_result str =
  match parse parse_file str with
  | Ok res -> pp_file Format.std_formatter res
  | Error err -> print_endline err
;;
