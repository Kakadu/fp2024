(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** [is_keyword s] returns true if [s] is a keyword of F#. *)
val is_keyword : string -> bool

(** [is_builtin_type t] returns true if [t] is a builtin type of F#. *)
val is_builtin_type : string -> bool

(** [is_builtin_op s] returns true if [s] is a builtin operator of F#. *)
val is_builtin_op : string -> bool
