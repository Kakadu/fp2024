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
  | Division_by_0 -> "division by zero"
  | Array_index_out_of_bound -> "array index out of bounds"
  | Negative_array_index -> "negative array index call"
  | Uninited_func -> "uninitialized function call"
  | Close_of_closed_chan -> "close of closed chanel"
  | Close_of_nil_chan -> "close of uninitialized chanel"
  | Deadlock msg -> "Deadlock: " ^ msg
  | Panic msg -> "Panic: " ^ msg
  | TypeCheckFailed msg -> "Type error in runtime: " ^ msg
  | Dev msg -> "Dev: " ^ msg
;;
