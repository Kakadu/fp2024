(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type type_check_error =
  | Incorrect_main of string
  | Multiple_declaration of string
  | Undefined_ident of string
  | Mismatched_types of string
  | Cannot_assign of string
  | Missing_return of string
  | Invalid_operation of string
[@@deriving show { with_path = false }]

type error = Type_check_error of type_check_error
(* | Eval_error *)
[@@deriving show { with_path = false }]
