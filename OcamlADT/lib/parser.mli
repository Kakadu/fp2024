(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** Parses a given input string into a list of structure items.

    @param str The input string to parse.
    @return An [Ok] containing the parsed structure or an [Error] message. *)
open Angstrom

val pident_cap : string t
val pident_lc : string t
val ppatvar : Pattern.t t
val pletbinding : Expression.t t -> Expression.t Expression.value_binding t
val pifexpr : Expression.t t -> Expression.t t
val papplyexpr : (Expression.t -> Expression.t -> Expression.t) t
val pfunexpr : Expression.t t -> Expression.t t
val parsebinop : string -> (Expression.t -> Expression.t -> Expression.t) t
val padd : (Expression.t -> Expression.t -> Expression.t) t
val psub : (Expression.t -> Expression.t -> Expression.t) t
val pdiv : (Expression.t -> Expression.t -> Expression.t) t
val pmul : (Expression.t -> Expression.t -> Expression.t) t
val pcompops : (Expression.t -> Expression.t -> Expression.t) t
val plogops : (Expression.t -> Expression.t -> Expression.t) t
val pexpr : Expression.t t
val pseval : Structure.structure_item t

(* val psvalue : structure_item t *)
val pstr_item : Structure.structure_item t
val pstructure : Structure.structure_item list t
val parse : string -> (Structure.structure_item list, string) result
val parse_str : string -> Structure.structure_item list
