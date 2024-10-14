(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

val pp : (Format.formatter -> 'a -> unit) -> 'a t -> ident -> unit

val ws : unit t

val ws_line : unit t

val token : ident -> ident t

val parens : 'a t -> 'a t

val many_sep : sep:'a t -> parser:'b t -> 'b list t

(** Separator for the statements, [;] or [\n] *)
val parse_stmt_sep : unit t

val parse_const: const t

val parse_ident: string t

val parse_type: type' t
