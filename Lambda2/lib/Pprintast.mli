[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Verbose printing. Usable for paring *)
val pp : Format.formatter -> string Ast.t -> unit

(** Print in fancy human-readable form *)
val pp_hum : Format.formatter -> string Ast.t -> unit
