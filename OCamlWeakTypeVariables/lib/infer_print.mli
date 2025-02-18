[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Pretty printer for base type *)
val pp_base_type_my : Format.formatter -> Types.base_type -> unit

(** Pretty printer for type *)
val pp_typ_my : Format.formatter -> Types.typ -> unit

(** Pretty printf for inference errors *)
val pp_error_my : Format.formatter -> Types.error -> unit

(** Print type *)
val print_typ : ?name:string -> Types.typ -> unit
