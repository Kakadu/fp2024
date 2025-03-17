(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

(** [puconst] parses any (unsigned) constant and returns it as a constant type.
    Should be used for expression parsing. Cannot parse surrouning whitespaces. *)
val puconst : constant t

(** [psconst] parses any (probably signed) constant and returns it as a constant type.
    Should be used for pattern parsing. Cannot parse surrouning whitespaces. *)
val psconst : constant t
