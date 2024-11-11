(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open TopLevel
open Ast

let parse parser str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All parser str |> Result.ok_or_failwith
;;

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error res -> print_endline res
;;

let pp_repl str = pp pp_file parse_file str
