(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser_utility

val string_of_parse_result : ('a -> string) -> 'a parse_result -> string
