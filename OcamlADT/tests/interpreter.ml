(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Interpreter
open Ocamladt_lib.Interpreter.PPrinter
open Ocamladt_lib.Parser

(* Full verison with TC
   let pp_interpret ast =
   match run_inference ast with
   | Ok _ ->
   (match run_interpreter ast with
   | Ok value -> print_value
   | Error e -> print_error e)
   | Error e -> print_type_error e (* to be implemented in TC*)
   ;;
*)

let pp_interpret_demo ast =
  match run_interpreter ast with
  | Ok value -> print_value value
  | Error e -> print_error e
;;

let pp_parse_demo str =
  let ast = parse_str str in
  pp_interpret_demo ast
;;

let%expect_test "negative int constant" =
  pp_parse_demo {|-1;;|};
  [%expect {| Intepreter error: Pattern mismatch |}]
;;

let%expect_test "zero" =
  pp_parse_demo {|0;;|};
  [%expect {| 0 |}]
;;

let%expect_test "x" =
  pp_parse_demo {|x;;|};
  [%expect {| Intepreter error: Unbound value x |}]
;;

(*should be nothing*)
let%expect_test "substraction" =
  pp_parse_demo {|5-11;;|};
  [%expect {|
    -6 |}]
;;

(*should be nothing*)
let%expect_test "strange move" =
  pp_parse_demo {|5=5;;|};
  [%expect {|
    true |}]
;;

let%expect_test "assignment (fail: UnboundValue - x)" =
  pp_parse_demo {|x = 51;;|};
  [%expect {|
    Intepreter error: Unbound value x |}]
;;

(*should be nothing*)
let%expect_test "operators with different priorities" =
  pp_parse_demo {|5-5*1;;|};
  [%expect {| 0 |}]
;;

(*should be nothing *)
let%expect_test "just let (int)" =
  pp_parse_demo {|let x = 51;;|};
  [%expect {| 51 |}]
;;

(*should be nothing *)
let%expect_test "just let (string)" =
  pp_parse_demo {|let x = "51";;|};
  [%expect {| 51 |}]
;;

(*should be nothing *)
let%expect_test "just let (char)" =
  pp_parse_demo {|let x = '5';;|};
  [%expect {| 5 |}]
;;

let%expect_test "int print_endline (fail: TypeMismatch)" =
  pp_parse_demo {|let x = 51 in 
print_endline x;;|};
  [%expect {|
    Intepreter error: TypeMismatch |}]
;;

let%expect_test "string print_endline" =
  pp_parse_demo {|let x = "51" in 
print_endline x;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_endline" =
  pp_parse_demo {|print_endline "51";;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_endline as an arg" =
  pp_parse_demo {|let f = print_endline in 
f "Hello";;|};
  [%expect {|
    Hello |}]
;;

let%expect_test "print_endline as an arg (fail: TypeMismatch)" =
  pp_parse_demo {|let f = print_endline in 
f 5;;|};
  [%expect {|
    Intepreter error: TypeMismatch |}]
;;

let%expect_test "print_int as an arg" =
  pp_parse_demo {|let f = print_int in 
f 51;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_int let assignment" =
  pp_parse_demo {|let reca = 51 in 
print_int reca;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_char as an arg" =
  pp_parse_demo {|let f = print_char in 
f '5';;|};
  [%expect {|
    5 |}]
;;

let%expect_test "print_char let assignment" =
  pp_parse_demo {|let reca = '5' in 
print_char reca;;|};
  [%expect {|
    5 |}]
;;

let%expect_test "let assignment none (fail: PatternMismatch)" =
  pp_parse_demo {|let Some Some Some Some Some None = 1 in 
print_int None;;|};
  [%expect {|
    Intepreter error: Pattern mismatch |}]
;;

let%expect_test "multiple let assignments" =
  pp_parse_demo {| let x = 3 in let y = 4 in print_int (x + y) ;; |};
  [%expect {| 7 |}]
;;

let%expect_test "too damn simple function assignment (TC should fail?)" =
  pp_parse_demo {| let id = fun x -> y in print_int (id 7) ;; |};
  [%expect {| Intepreter error: Unbound value y |}]
;;

(*4 am vibes, im sorry*)
let%expect_test "not too damn simple function assignment" =
  pp_parse_demo {| let id = fun x -> x * x in print_int (id 7) ;; |};
  [%expect {| 49 |}]
;;

let%expect_test "match case (_ case)" =
  pp_parse_demo
    {|
let classify n = 
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "other"
in
print_endline (classify 2);; |};
  [%expect {|
    other |}]
;;

let%expect_test "match case (specific pattern case)" =
  pp_parse_demo
    {|
let classify n = 
  match n with
  | "0" -> 51
  | "1" -> 811
  | _ -> 0
in
print_int (classify "1");; |};
  [%expect {|
    811 |}]
;;

(*idk*)
let%expect_test "if then case" =
  pp_parse_demo
    {| let x = 10 in
if x > 5 then print_endline "> 5"
else print_endline "<= 5";;
;; |};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Interpreter.pp_parse_demo in file "tests/interpreter.ml", line 27, characters 12-25
  Called from Ocamladt_tests__Interpreter.(fun) in file "tests/interpreter.ml", line 205, characters 2-104
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "if then case (else)" =
  pp_parse_demo
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_endline "One"
  else
    print_endline "Other"
in 
check_number 5
;; |};
  [%expect {|
    Other |}]
;;

let%expect_test "if then case (then)" =
  pp_parse_demo
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_endline "One"
  else
    print_endline "Other"
in 
check_number 0
;; |};
  [%expect {|
    Zero |}]
;;

let%expect_test "if then case (else if)" =
  pp_parse_demo
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_int 555555555555
  else
    print_endline "Other"
in 
check_number 1
;; |};
  [%expect {|
    555555555555 |}]
;;

let%expect_test "nested assignments" =
  pp_parse_demo
    {| 
let x = 
      let y = 
        let z = 
          let w = 1
          in w
        in z
      in y
;; |};
  [%expect {|
    1 |}]
;;

let%expect_test "factorial" =
  pp_parse_demo
    {| 
let rec fact n = if n = 0 then 1 else n * fact(n-1) in 
print_int (fact 5)
;; |};
  [%expect {|
    120 |}]
;;

(*to fix structure item env i guess*)
let%expect_test "wtf" =
  pp_parse_demo
    {| let arith x y = (x * y, x / y, x + y, x - y);;
  let prod x y = 
    let fst (a, _, _, _) = a in
    fst (arith x y)
  ;;
  let p = prod 3 0;;
|};
  [%expect {|
    Intepreter error: Unbound value prod |}]
;;
