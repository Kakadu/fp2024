(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Eval
open Typecheck

let pp str =
  match parse parse_file str with
  | Ok ast ->
    (match TypeChecker.type_check ast with
     | Result.Ok _ ->
       (match Eval.eval ast with
        | Result.Ok _ -> prerr_endline "Correct evaluating"
        | Result.Error err ->
          (match err with
           | Runtime_error (DevOnly Not_enough_operands) ->
             prerr_endline "Not enough operands"
           | Runtime_error (DevOnly No_goroutine_running) ->
             prerr_endline "No goroutine running"
           | Runtime_error (DevOnly Two_goroutine_running) ->
             prerr_endline "Two goroutine running"
           | Runtime_error Stack_overflow -> prerr_endline "Stack overflow"
           | Runtime_error Division_by_zero -> prerr_endline "Try to divide by zero"
           | Runtime_error Array_index_out_of_bound ->
             prerr_endline "Array index out of bounds"
           | Runtime_error Deadlock -> prerr_endline "Go routine deadlock"
           | Runtime_error (Panic msg) -> prerr_endline ("Paniced with message:" ^ msg)
           | Runtime_error (DevOnly TypeCheckFailed) ->
             prerr_endline "Internal Typecheck error occured while evaluating"
           | Runtime_error _ -> prerr_endline "Some kind of runtime error"
           | Type_check_error _ -> prerr_endline "Some kind of typecheck error"))
     | Result.Error err ->
       prerr_string "ERROR WHILE TYPECHECK WITH ";
       (match err with
        | Type_check_error Check_failed -> prerr_endline "Check failed"
        | Type_check_error (Multiple_declaration msg) ->
          prerr_string ("Multiple declaration error: " ^ msg)
        | Type_check_error (Incorrect_main msg) ->
          prerr_endline ("Incorrect main error: " ^ msg)
        | Type_check_error (Undefined_ident msg) ->
          prerr_endline ("Undefined ident error: " ^ msg)
        | Type_check_error (Mismatched_types msg) ->
          prerr_endline ("Mismatched types: " ^ msg)
        | Type_check_error (Cannot_assign msg) -> prerr_endline ("Cannot assign: " ^ msg)
        | Type_check_error (Missing_return msg) -> prerr_endline ("Missing return: " ^ msg)
        | Type_check_error (Invalid_operation msg) ->
          prerr_endline ("Missing return: " ^ msg)
        | _ -> ()))
  | Error _ -> print_endline ": syntax error"
;;

let%expect_test "ok: single main" =
  pp {|
    func main() {print("kill OCaml")}
    |};
  [%expect {|
    CORRECT |}]
;;
