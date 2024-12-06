(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val run_inference : Ast.expr -> (Typing.ty, Typing.error) result
