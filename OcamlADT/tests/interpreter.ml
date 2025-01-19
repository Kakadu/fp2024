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
  [%expect {| Pattern mismatch |}]
;;

let%expect_test "zero" =
  pp_parse_demo {|0;;|};
  [%expect {| 0 |}]
;;

let%expect_test "x" =
  pp_parse_demo {|x;;|};
  [%expect {| Unbound value x |}]
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
    IDK |}]
;;

let%expect_test "assignment (fail: UnboundValue - x)" =
  pp_parse_demo {|x = 51;;|};
  [%expect {|
    Unbound value x |}]
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
    TypeMismatch |}]
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
    TypeMismatch |}]
;;
