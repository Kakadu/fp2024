(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Typedtree

type typeenv

val typeenv_print_int : typeenv
val typeenv_empty : typeenv
val pp_some_typeenv : Format.formatter -> string list * typeenv -> unit

val w
  :  binding_list
  -> typeenv
  -> binder
  -> binder * (typeenv * string list, error) Result.t