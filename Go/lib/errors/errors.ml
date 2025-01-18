(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type type_check_error =
  | Incorrect_main of string
  | Multiple_declaration of string
  | Undefined_ident of string
  | Mismatched_types of string
  | Cannot_assign of string
  | Missing_return of string
  | Go_make
  | Invalid_operation of string
  | Unexpected_operation of string

type devonly_runtime_error =
  | Not_enough_stack_frames
  | Not_enough_local_envs
  | Not_enough_operands
  | No_goroutine_running
  | Two_goroutine_running
  | Undefined_ident of string
  | TypeCheckFailed of string

type runtime_error =
  | Stack_overflow
  | Division_by_zero
  | Array_index_out_of_bound
  | Negative_array_index
  | Uninited_func
  | Deadlock of string
  | Close_of_closed_chan
  | Close_of_nil_chan
  | Panic of string
  | DevOnly of devonly_runtime_error

type error =
  | Type_check_error of type_check_error
  | Runtime_error of runtime_error

let pp_typecheck_error = function
  | Multiple_declaration msg -> "Multiple declaration error: " ^ msg
  | Incorrect_main msg -> "Incorrect main error: " ^ msg
  | Undefined_ident msg -> "Undefined ident error: " ^ msg
  | Mismatched_types msg -> "Mismatched types: " ^ msg
  | Cannot_assign msg -> "Cannot assign: " ^ msg
  | Missing_return msg -> "Missing return: " ^ msg
  | Invalid_operation msg -> "Invalid operation: " ^ msg
  | Go_make -> "Go discards result of make builtin function"
  | Unexpected_operation msg -> "Unexpected operation: " ^ msg
;;

let pp_runtime_error = function
  | DevOnly Not_enough_operands -> "Not enough operands"
  | DevOnly No_goroutine_running -> "No goroutine running"
  | DevOnly Two_goroutine_running -> "Two goroutine running"
  | Stack_overflow -> "Stack overflow"
  | Division_by_zero -> "Try to divide by zero"
  | Array_index_out_of_bound -> "Array index out of bounds"
  | Deadlock msg -> "Deadlock: " ^ msg
  | Panic msg -> "Paniced with message:" ^ msg
  | DevOnly (TypeCheckFailed msg) ->
    "Internal Typecheck error occured while evaluating" ^ msg
  | DevOnly (Undefined_ident msg) -> "Undefined ident " ^ msg
  | DevOnly _ -> "Some kind of devonly error"
  | _ -> "Some kind of runtime error"
;;
