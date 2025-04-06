(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Inferencer
open Ast

let%expect_test _ =
  let _ =
    let e = [ Expression (Expr_const (Const_int 4)) ] in
    check_types e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ Expression
          (Expr_tuple
             [ Expr_const (Const_int 1)
             ; Expr_const (Const_int 2)
             ; Expr_const (Const_int 3)
             ; Expr_list
                 ( Expr_const (Const_int 4)
                 , Expr_list (Expr_const (Const_int 5), Expr_const Const_nil) )
             ])
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| (int * int * int * int list) |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ Let
          ( true
          , "fact"
          , Expr_fun
              ( Pattern_id "n"
              , Expr_if
                  ( Expr_bin_op (Eq, Expr_var "n", Expr_const (Const_int 1))
                  , Expr_const (Const_int 1)
                  , Expr_bin_op
                      ( Mul
                      , Expr_var "n"
                      , Expr_app
                          ( Expr_var "fact"
                          , Expr_bin_op (Sub, Expr_var "n", Expr_const (Const_int 1)) ) )
                  ) ) )
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| (int -> int) |}]
;;

(* Additional inferencer tests for type errors and pattern matching *)
let%expect_test _ =
  let _ =
    let e =
      [ Expression
          (Expr_bin_op (Add, Expr_const (Const_int 1), Expr_const (Const_bool true)))
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| Error: Typechecker error: unification failed on bool and int |}]
;;

let%expect_test _ =
  let _ =
    let e = [ Expression (Expr_var "x") ] in
    check_types e |> run_infer
  in
  [%expect {| Error: Typechecker error: undefined variable 'x' |}]
;;

let%expect_test _ =
  let _ =
    let e = [] in
    check_types e |> run_infer
  in
  [%expect {| Error: empty program |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ Expression (Expr_fun (Pattern_id "y", Expr_app (Expr_var "y", Expr_var "y"))) ]
    in
    check_types e |> run_infer
  in
  [%expect {| Error: Typechecker error: occurs check failed |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ Expression
          (Expr_match
             ( Expr_const (Const_int 1)
             , [ Pattern_const (Const_int 0), Expr_const (Const_string "zero")
               ; Pattern_wild, Expr_const (Const_string "nonzero")
               ] ))
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| string |}]
;;

let%expect_test _ =
  let _ =
    let e =
      [ Expression
          (Expr_match
             ( Expr_const (Const_int 1)
             , [ Pattern_const (Const_int 0), Expr_const (Const_int 0)
               ; Pattern_wild, Expr_const (Const_string "nonzero")
               ] ))
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| Error: Typechecker error: unification failed on int and string |}]
;;
