(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

(** [shprog] shrinks the whole program *)
val shprog : structure_item list QCheck.Shrink.t
