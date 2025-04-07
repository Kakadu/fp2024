(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.ValuesTree
open Forest.Ast

val interpret_program : program -> (env, error) result
