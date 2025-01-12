(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap : Map.S with type key = string

type state =
  { registers : int StringMap.t
  ; pc : int
  }

val interpret : state -> ast -> state
val show_state : state -> string
