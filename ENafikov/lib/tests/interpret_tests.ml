(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Interpret
open Ast

(* Helper function to evaluate a program AST and print the result or error *)
let run_eval ast =
  match InterpretResult.eval_program ast with
  | Ok res -> Format.printf "%s%!" (show_value res)
  | Error e -> Format.printf "%a%!" pp_error_inter e
;;

let%expect_test _ =
  (* Division by zero error *)
  let ast =
    [ Expression (Expr_bin_op (Div, Expr_const (Const_int 5), Expr_const (Const_int 0))) ]
  in
  run_eval ast;
  [%expect {| Exception: Division_by_zero. |}]
;;

let%expect_test _ =
  (* Unbound variable error *)
  let ast = [ Expression (Expr_var "x") ] in
  run_eval ast;
  [%expect {| Error: Unbound value x |}]
;;

let%expect_test _ =
  (* Non-exhaustive pattern match error *)
  let ast =
    [ Expression
        (Expr_match
           ( Expr_const (Const_int 1)
           , [ Pattern_const (Const_int 2), Expr_const (Const_int 2) ] ))
    ]
  in
  run_eval ast;
  [%expect {| Exception: this pattern-matching is not exhaustive. |}]
;;

let%expect_test _ =
  (* Type mismatch error at runtime *)
  let ast =
    [ Expression
        (Expr_bin_op (Add, Expr_const (Const_bool true), Expr_const (Const_int 1)))
    ]
  in
  run_eval ast;
  [%expect {| Error: type mismatch, a different type was expected |}]
;;

let%expect_test _ =
  (* Empty program error *)
  let ast = [] in
  run_eval ast;
  [%expect {| the program was not provided or was empty |}]
;;

let%expect_test _ =
  (* Boolean logic evaluation *)
  let ast =
    [ Expression
        (Expr_bin_op (And, Expr_const (Const_bool true), Expr_const (Const_bool false)))
    ]
  in
  run_eval ast;
  [%expect {| (VBool false) |}]
;;

let%expect_test _ =
  (* Pattern matching evaluation *)
  let ast =
    [ Expression
        (Expr_match
           ( Expr_const (Const_int 2)
           , [ Pattern_const (Const_int 0), Expr_const (Const_int 0)
             ; Pattern_id "n", Expr_bin_op (Mul, Expr_var "n", Expr_const (Const_int 10))
             ] ))
    ]
  in
  run_eval ast;
  [%expect {| (VInt 20) |}]
;;

let%expect_test _ =
  (* Function definition and application *)
  let ast =
    [ Let
        ( false
        , "double"
        , Expr_fun
            (Pattern_id "x", Expr_bin_op (Mul, Expr_var "x", Expr_const (Const_int 2))) )
    ; Expression (Expr_app (Expr_var "double", Expr_const (Const_int 5)))
    ]
  in
  run_eval ast;
  [%expect {| (VInt 10) |}]
;;

let%expect_test _ =
  (* let-in expression *)
  let ast =
    [ Expression
        (Expr_let_in
           ( false
           , "x"
           , Expr_const (Const_int 5)
           , Expr_bin_op (Add, Expr_var "x", Expr_const (Const_int 3)) ))
    ]
  in
  run_eval ast;
  [%expect {| (VInt 8) |}]
;;

let%expect_test _ =
  (* Tuple expression *)
  let ast =
    [ Expression (Expr_tuple [ Expr_const (Const_int 42); Expr_const (Const_bool true) ])
    ]
  in
  run_eval ast;
  [%expect {| (VTuple [(VInt 42); (VBool true)]) |}]
;;

let%expect_test _ =
  (* Pattern match on list *)
  let ast =
    [ Let
        ( false
        , "head"
        , Expr_fun (Pattern_list (Pattern_id "x", Pattern_wild), Expr_var "x") )
    ; Expression
        (Expr_app
           (Expr_var "head", Expr_list (Expr_const (Const_int 1), Expr_const Const_nil)))
    ]
  in
  run_eval ast;
  [%expect {| (VInt 1) |}]
;;

let%expect_test _ =
  (* :: (Con) list constructor *)
  let ast =
    [ Expression (Expr_bin_op (Con, Expr_const (Const_int 10), Expr_const Const_nil)) ]
  in
  run_eval ast;
  [%expect {| (VList [(VInt 10)]) |}]
;;

let%expect_test _ =
  (* Match on tuple pattern *)
  let ast =
    [ Expression
        (Expr_match
           ( Expr_tuple [ Expr_const (Const_int 1); Expr_const (Const_int 2) ]
           , [ Pattern_tuple [ Pattern_const (Const_int 1); Pattern_id "x" ], Expr_var "x"
             ; Pattern_wild, Expr_const (Const_int 0)
             ] ))
    ]
  in
  run_eval ast;
  [%expect {| (VInt 2) |}]
;;
