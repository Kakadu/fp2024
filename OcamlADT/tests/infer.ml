open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes
open Ocamladt_lib.Ast.TypeExpr
(*
   let infer_result exp  =
    (match run_infer_expr exp TypeEnv.empty with
     | Ok (_, env) -> printf "%a" pprint_type env
     | Error err -> printf "%a" pp_inf_err err)
;;


let%expect_test _ =
  let _ = infer_result 
  (Exp_let (Nonrecursive,
     ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) },
      []),
     (Exp_constant (Const_integer 1))))
 in
  [%expect{| int |}]
;;

let%expect_test _ =
  let _ = infer_result 
  (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_string "a")) }, []),
           (Exp_ident "x")))
 in
  [%expect{| string |}]
;;


let%expect_test _ =
  let _ = infer_result 
  (Exp_constant (Const_integer 1))
 in
  [%expect{| int |}]
;;

let%expect_test _ =
  let _ = infer_result 
  (Exp_constant (Const_char 'a'))
 in
  [%expect{| char |}]
;;

let%expect_test _ =
  let _ = infer_result 
  (Exp_constant (Const_string "str"))
 in
  [%expect{| string |}]
;;
*)

(*str item tetst*)
let parse_and_infer_result program =
  match parse_str program with
  | str ->
    (match run_infer_program str TypeEnv.empty with
     | Ok env -> printf "\nres:\n%a" TypeEnv.pp_env env
     (* | Ok (_,[]) -> failwith "abibi" *)
     | Error err -> printf "%a" pp_inf_err err)
  | _ -> failwith "aboba"
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> x;;|};
  [%expect
    {|
    res:
    "-": '0 -> '0 |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = x+x;;|};
  [%expect {|
    Unbound_variable: "x" |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result {|let f x = x+x;;|};
  [%expect
    {|
    res:
    "f": [ int; ]. 'int -> 'int |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result {|5+5;;|};
  [%expect
    {|
    res:
    "-": 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5/5;;|};
  [%expect
    {|
    res:
    "-": 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5-5;;|};
  [%expect
    {|
    res:
    "-": 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5*5;;|};
  [%expect
    {|
    res:
    "-": 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5>=5;;|};
  [%expect
    {|
    res:
    "-": 'bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5<=5;;|};
  [%expect
    {|
    res:
    "-": 'bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5>5;;|};
  [%expect
    {|
    res:
    "-": 'bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5<5;;|};
  [%expect
    {|
    res:
    "-": 'bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 5 in x;;|};
  [%expect
    {|
    res:
    "-": '1 |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result {|let x = 1;;
let y = 2;;
let z = 3;;
(x,y,z) = (5,6,7);;|};
  [%expect
    {|
    res:
    "-": 'bool
    "x": [ int; ]. 'int
    "y": [ int; ]. 'int
    "z": [ int; ]. 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5=5 then 1 else 5;;|};
  [%expect
    {|
    res:
    "-": 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5=5 then "aboba";;|};
  [%expect
    {|
    res:
    "-": 'string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|(5,6,7);;|};
  [%expect
    {|
    res:
    "-": 'int * 'int * 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|function 
5 -> 'c' 
| 67 -> 'b' 
| 68 -> 'h' 
| 69 -> 's' 
| 89 -> 'a';;|};
  [%expect
    {|
    res:
    "-": 'char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|match 9 with
|5 -> 5 
|6 -> 5
|7 -> 7
|7 -> 1
|7 -> 1
|7 -> 1
| _ -> 3
;;|};
  [%expect {|
    Occurs_check |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> fun y -> y+x;;|};
  [%expect
    {|
    res:
    "-": 'int -> 'int -> 'int |}]
;;

(*ALL "LET" ITEMS*)

let%expect_test "zero" =
  parse_and_infer_result {|let y = 5;;|};
  [%expect{|
    res:
    "y": [ int; ]. 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let _ = (2,5) and y = ("a","b");;|};
  [%expect
    {|
    res:
    "y": [ string; ]. 'string * 'string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = (2,5) and y = ("a","b");;|};
  [%expect
    {|
    res:
    "x": [ int; ]. 'int * 'int
    "y": [ string; ]. 'string * 'string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let (x,y) = (2,5) and z = ("a","b");; let f = x;;|};
  [%expect
    {|
    res:
    "f": [ 3; ]. '3
    "x": [ int; ]. 'int
    "y": [ int; ]. 'int
    "z": [ string; ]. 'string * 'string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let (x,y) = (2,'c');;|};
  [%expect
    {|
    res:
    "x": [ char; int; ]. 'int
    "y": [ char; int; ]. 'char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 5=5;;|};
  [%expect
    {|
    res:
    "x": [ bool; ]. 'bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 6 and y = 6 and z = 6;;|};
  [%expect
    {|
    res:
    "x": [ int; ]. 'int
    "y": [ 0; ]. '0
    "z": [ 1; ]. '1 |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let rec f x = f x;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 304, characters 2-48
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = 5;;
let 5 = x;;|};
  [%expect
    {|
    res:
    "x": [ int; ]. 'int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = (6: char);;|};
  [%expect
    {|
    res:
    "x": [ char; ]. 'char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
() = print_int 5;;|};
  [%expect
    {|
    Unbound_variable: "print_int" |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result
    {|
let rec meven n = if n = 0 then 1 else modd (n - 1)
   and modd n = if n = 0 then 1 else meven (n - 1)
   and homka n = damir 4
   and damir n = homka 5
;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 351, characters 2-189
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*KAKADU TESTS*)
(*PASSED*)
let%expect_test "001fact without builtin" =
  parse_and_infer_result {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 378, characters 2-81
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*FAILED*)
let%expect_test "001fact with builtin" =
  parse_and_infer_result
    {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);; let main =
  let () = print_int (fac 4) in
  0;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 399, characters 2-134
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*PASSED*)
let%expect_test "002fact without builtin" =
  parse_and_infer_result
    {|
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n));;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 424, characters 2-115
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*FAILED*)
let%expect_test "002fact builtin" =
  parse_and_infer_result
    {|
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n));; let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 448, characters 2-197
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*PASSED*)
let%expect_test "003fib without builtin" =
  parse_and_infer_result
    {|
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1;;
    let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 475, characters 2-215
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*FAILED*)
let%expect_test "003fib builtin" =
  parse_and_infer_result
    {|
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1;;
    let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2);;
  let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Unsupported pattern in let binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 49, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 589, characters 68-107
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 507, characters 2-306
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*PASSED*)
let%expect_test "004" =
  parse_and_infer_result
    {|
let wrap f = if 1 = 1 then f else f;;

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j;;|};
  [%expect
    {|
    res:
    "test10": [ int; ]. 'int -> 'int -> 'int -> 'int -> 'int -> 'int -> 'int -> 'int -> 'int -> 'int -> 'int
    "wrap": [ 0; ]. '0 -> '0 |}]
;;

(*FAILED*)
let%expect_test "004" =
  parse_and_infer_result
    {|
let wrap f = if 1 = 1 then f else f;;

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j;;
let main =
  let rez =
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0;;|};
  [%expect {|
    Unbound_variable: "print_int" |}]
;;
