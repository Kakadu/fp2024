[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Enable debug output to stdout.
    If true, prints debug messages to stdout. *)
val debug : bool

(** Minimize type variables.
    If true, type variables will minimize as much as possible.
    Example: '4 -> '2 -> ('5 option) -> '4
    becomes: '1 -> '2 -> '3 option -> '1 *)
val vars_min : bool

(** Representing type variables with letters instead of numbers.
    If true, type variables use alphabetical characters (e.g., 'a 'b).
    Example: '4 -> '2 -> >('5 option) -> '4
    becomes: 'd -> 'b -> ('e option) -> 'd *)
val vars_char : bool

(** Display quantified type variables explicitly.
    If true, binding variables are printed before the type.
    For example 'a -> 'b -> int
    will be 'b . 'a -> 'b -> int *)
val show_scheme_vars : bool

(** Cuneiform is a logo-syllabic writing system that was used to write several languages of the Ancient Near East.
    The script was in active use from the early Bronze Age until the beginning of the Common Era.
    Cuneiform scripts are marked by and named for the characteristic wedge-shaped impressions (Latin: cuneus) which form their signs.
    Cuneiform is the earliest known writing system and was originally developed to write
    the Sumerian language of southern Mesopotamia.

    For example '4 -> '2 -> ('5 option) -> '4
    becomes: '𒀀 -> '𒀐 -> '𒀲 option -> '𒀀

    Inspired by Kirill Smirnov *)
val use_cuneiform : bool
