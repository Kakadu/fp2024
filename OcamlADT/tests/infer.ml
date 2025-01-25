open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes

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


(*str item tetst*)
let parse_and_infer_result program  =
  match parse_str program with
  | str ->
    (match run_infer_program str TypeEnv.empty with
     | Ok env -> printf "\nres:\n %a" TypeEnv.pp_env env
     (* | Ok (_,[]) -> failwith "abibi" *)
     | Error err -> printf "%a" pp_inf_err err)
  | _ -> failwith "aboba"
;;

let%expect_test "zero" =
parse_and_infer_result {|fun x -> x;;|};
  [%expect{|
    res: |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|let x = "a" and y = 5 and z = 'c';;|};
  [%expect{|
    res:
     "x": string
    "y": int
    "z": char |}]
;;


(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|let x = x+x;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_value_binding in file "lib/infer.ml", line 355, characters 25-47
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 84, characters 0-40
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|let f x = x+x;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_exp in file "lib/infer.ml", line 289, characters 12-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 106, characters 0-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|5+5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 128, characters 0-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5/5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 146, characters 0-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5-5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 164, characters 0-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5*5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 182, characters 0-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5>=5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 200, characters 0-33
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5<=5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 218, characters 0-33
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5>5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 236, characters 0-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5<5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure unlucky)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.infer_structure_item in file "lib/infer.ml", line 377, characters 17-34
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 10, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 41, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program in file "lib/infer.ml", line 397, characters 52-83
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 59, characters 11-46
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 254, characters 0-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;


let%expect_test "zero" =
parse_and_infer_result {|let x = 5 in x;;|};
  [%expect{|
    res: |}]
;;
