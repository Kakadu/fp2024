(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Forest.TypesTree
open InfAuxilary

val run_expr_inf : expr -> (typ, error) result
val run_program_inf : structure_item list -> (TypeEnv.t, error) result
