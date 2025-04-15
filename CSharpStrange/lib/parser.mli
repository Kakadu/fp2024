(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

val parens : 'a t -> 'a t
val braces : 'a t -> 'a t
val brackets : 'a t -> 'a t
val parse_int : val_type t
val parse_char : val_type t
val parse_bool : val_type t
val parse_val_string : val_type t
val parse_modifiers : modifier list t
val parse_ops : expr t
val parse_decl : stmt t
val parse_return : stmt t
val parse_break : stmt t
val parse_continue : stmt t
val parse_block : stmt t
val parse_method_member : field t
val parse_field_member : field t
val parse_class : c_sharp_class t
val parse_prog : program t
val apply_parser : 'a t -> string -> ('a, string) Result.t
val parse_option : 'a t -> string -> 'a option
