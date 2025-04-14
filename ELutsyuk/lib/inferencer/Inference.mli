(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Forest.TypesTree
open InfAuxilary

val inference : structure_item list -> (Subst.t * TypeEnv.t, error) result
