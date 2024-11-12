(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error res -> print_endline res
;;

