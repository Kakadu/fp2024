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

type devonly_runtime_error =
  | Not_enough_stack_frames
  | Not_enough_local_envs
  | Not_enough_operands
  | No_goroutine_running
  | Two_goroutine_running
  | Undefined_ident of string
  | TypeCheckFailed
[@@deriving show { with_path = false }]

type runtime_error =
  | Stack_overflow
  | Division_by_zero
  | Array_index_out_of_bound
  | Negative_array_index
  | Uninited_func
  | Deadlock
  | Close_of_closed_chan
  | Close_of_nil_chan
  | Panic of string
  | DevOnly of devonly_runtime_error
[@@deriving show { with_path = false }]

type error =
  | Type_check_error of type_check_error
  | Runtime_error of runtime_error
[@@deriving show { with_path = false }]
