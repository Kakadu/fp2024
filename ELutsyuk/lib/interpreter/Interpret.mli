(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Forest.ValuesTree

val interpret : program -> env IntAuxilary.Res.t
