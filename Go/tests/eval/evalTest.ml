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
           | Runtime_error (DevOnly (Undefined_ident msg)) ->
             prerr_endline ("Undefined ident " ^ msg)
           | Runtime_error (DevOnly _) -> prerr_endline "Some kind of devonly error"
           | Runtime_error _ -> prerr_endline "Some kind of runtime error"
           | Type_check_error _ -> prerr_endline "Some kind of typecheck error"))
     | Result.Error err ->
       prerr_string "ERROR WHILE TYPECHECK WITH ";
       (match err with
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
  pp
    {|
    func main() {
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")      
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml |}]
;;

let%expect_test "ok: single long_var_init" =
  pp {|
    var x = "kill OCaml"
    func main() {print(x)}
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: multiple long_var_init" =
  pp {|
    var x, y = "kill ", "OCaml"
    func main() {print(x, y)}
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: simple func_call with args" =
  pp
    {|
    var x, y = "kill ", "OCaml"
    func foo(x string, y string) {
      print(x, y)
    }
  
    func main() {foo(x, y)}
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: simple value func call" =
  pp
    {|
    var x = "kill "
    func foo(x string, y string) {
      print(x, y)
    }
  
    func main() {
      foo(x, "OCaml")
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: local var decl func call" =
  pp
    {|
    var x = "kill "
  
    func main() {
      var y = "OCaml"
      print(x, y)
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: func args init" =
  pp
    {|
    var x = "kill "
    func foo(x string, k string) {
      var z = " OCaml"
      print(x, k, z)
    }
  
    func main() {
      var y = "OCaml"
      foo(x, y)
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml OCaml |}]
;;

let%expect_test "ok: assignment check" =
  pp
    {|
    var x = "kill "
    func foo(x string, k string) {
      var z = " OCaml"
      print(x, k, z)
    }
  
    func main() {
      x = "OCaml "
      var y = "OCaml"
      foo(x, y)
    }
    |};
  [%expect {|
    Correct evaluating
    OCaml OCaml OCaml |}]
;;

let%expect_test "ok: simple arithmetic check" =
  pp
    {|
    var x int = 1
    func foo(k int) {
      var z = k 
      print(k + 1 + z)
    }
  
    func main() {
      x = 100
      x++       
      foo(x + 1)
    }
    |};
  [%expect {|
    Correct evaluating
    205 |}]
;;
