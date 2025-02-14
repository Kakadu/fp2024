(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type type_check_error =
  | Incorrect_main of string (** No main/main with returns or args *)
  | Multiple_declaration of string (** Multiple declaration of ident *)
  | Undefined_ident of string (** No declaration of ident in current sapce *)
  | Mismatched_types of string (** Mismatched types in binoper/assign/return... *)
  | Cannot_assign of string (** Error with assigning a multiple-return value *)
  | Missing_return of string (** Error with missing return of values in func *)
  | Go_make (** trying to run [make] builtin func as a goroutine ([go make(chan int)]) *)
  | Invalid_operation of string (**Error with doing some invalid operation *)
  | Unexpected_operation of string (**Return/continue not inside for body *)

type runtime_error =
  | Division_by_0
  | Array_index_out_of_bound
  | Negative_array_index
  | Uninited_func
  | Deadlock of string
  | Close_of_closed_chan
  | Close_of_nil_chan
  | Panic of string
  | TypeCheckFailed of string
  | Dev of string

type error =
  | Type_check_error of type_check_error
  | Runtime_error of runtime_error

val pp_typecheck_error : type_check_error -> string
val pp_runtime_error : runtime_error -> string
