(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base

let parse parser str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All parser str |> Result.ok_or_failwith
;;
