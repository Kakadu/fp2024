(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)
open Ast

val eval : top_decl list -> (unit, Errors.error) result
