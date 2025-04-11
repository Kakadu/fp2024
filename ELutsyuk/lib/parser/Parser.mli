(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast

val prs_program : program t

(* val parse_program : program t *)
val parse_program : id -> (program, string) result
