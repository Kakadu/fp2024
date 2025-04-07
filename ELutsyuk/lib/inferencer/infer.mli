(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Forest.TypesTree
open InfAuxilary

val inference_program : structure_item list -> (TypeEnv.t, error) result
