(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> print_endline (printer res)
  | Error _ -> print_endline ": syntax error"
;;
