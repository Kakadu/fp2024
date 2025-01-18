(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type type_check_error =
  | Incorrect_main of string (** No main/main with returns or args *)
  | Multiple_declaration of string (** Multiple declaration of ident *)
  | Undefined_ident of string (** No declaration of ident in current sapce *)
  | Mismatched_types of string (** Mismatched types in binoper/assign/return... *)
  | Cannot_assign of string (** Error with assigning a multiple-return value *)
  | Missing_return of string (** Error with missing return of values in func *)
  | Invalid_operation of string (**Error with doing some invalid operation *)
[@@deriving show { with_path = false }]

type error = Type_check_error of type_check_error
(* | Eval_error *)
[@@deriving show { with_path = false }]
