(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.ValuesTree
open IntAuxilary
open Forest.Ast

val eval_expr : expr -> (env, error) Res.t
