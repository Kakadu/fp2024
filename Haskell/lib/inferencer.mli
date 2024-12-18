(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Typedtree

type typeenv

val typeenv_print_int : typeenv
val typeenv_empty : typeenv
val pp_typeenv : Format.formatter -> typeenv -> unit
val w : binding_list -> typeenv -> binder -> binder * (typeenv, error) Result.t
