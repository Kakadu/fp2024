(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

type error =
  | Division_by_zero
  | Type_mismatch
  | Unbound_identificator of string
  | Invalid_syntax
  | Unsupported_operation
  | Match_failure
  | Not_implemented
[@@deriving show { with_path = false }]

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VChar of char
  | VString of string
  | VFun of rec_flag * pattern * expression * environment
  | VList of value list
  | VTuple of value * value * value list
  | VOption of value option
  | VFunction of rule * rule list
  | VUnit
[@@deriving show { with_path = false }]

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

