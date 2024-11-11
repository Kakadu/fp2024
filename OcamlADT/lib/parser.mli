(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

(** Parses a given input string into a list of structure items.

    @param str The input string to parse.
    @return An [Ok] containing the parsed structure or an [Error] message. *)
open Angstrom

val pconst : expression t
val pident : string t
val pconstintexpr : expression t
val pconstcharexpr : expression t
val pconststringexpr : expression t
val pconst : expression t
val pany : pattern t
val pvar : pattern t
val ppattern : pattern t
val pletbinding : expression t -> value_binding t
val plet : expression t -> expression t
val ptupleexpr : expression t -> expression t
val pifexpr : expression t -> expression t
val papplyexpr : expression t -> expression t
val pfunexpr : expression t -> expression t
val parsebinop : string -> (expression -> expression -> expression) t
val padd : (expression -> expression -> expression) t
val psub : (expression -> expression -> expression) t
val pdiv : (expression -> expression -> expression) t
val pmul : (expression -> expression -> expression) t
val pcompops : (expression -> expression -> expression) t
val plogops : (expression -> expression -> expression) t
val pexpr : expression t
val pseval : structure_item t
val psvalue : structure_item t
val pstr_item : structure_item t
val pstructure : structure_item list t
val parse : string -> (structure_item list, string) result
val parse_str : string -> structure_item list
