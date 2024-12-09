(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast.Expression
open Ocamladt_lib.Ast.Constant
open Ocamladt_lib.Ast.Pattern
open Ocamladt_lib.Pprinter
open Ocamladt_lib.Ast.Structure
open Format

let test_pprint_expression input_expr =
  let actual_output = asprintf "%a" (fun fmt -> pprint_expression fmt 0) input_expr in
  print_endline actual_output
;;

let%expect_test "simple addition" =
  let expr =
    Exp_apply
      ( Exp_ident "+"
      , Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []) )
  in
  test_pprint_expression expr;
  [%expect {| 1 + 2 |}]
;;

let%expect_test "nested expressions with precedence handling" =
  let expr =
    Exp_apply
      ( Exp_ident "+"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "*"
              , Exp_tuple
                  (Exp_constant (Const_integer 3), Exp_constant (Const_integer 4), []) )
          , Exp_constant (Const_integer 5)
          , [] ) )
  in
  test_pprint_expression expr;
  [%expect {| 3 * 4 + 5 |}]
;;

let%expect_test "parentheses for lower precedence" =
  let expr =
    Exp_apply
      ( Exp_ident "*"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "+"
              , Exp_tuple
                  (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []) )
          , Exp_constant (Const_integer 3)
          , [] ) )
  in
  test_pprint_expression expr;
  [%expect {| (1 + 2) * 3 |}]
;;

let%expect_test "simple nested addition expression" =
  let expr =
    Exp_apply
      ( Exp_ident "+"
      , Exp_tuple
          ( Exp_constant (Const_integer 1)
          , Exp_apply
              ( Exp_ident "+"
              , Exp_tuple
                  (Exp_constant (Const_integer 2), Exp_constant (Const_integer 3), []) )
          , [] ) )
  in
  test_pprint_expression expr;
  [%expect {| 1 + (2 + 3) |}]
;;

let%expect_test "subtraction" =
  let expr =
    Exp_apply
      ( Exp_ident "-"
      , Exp_tuple (Exp_constant (Const_integer 5), Exp_constant (Const_integer 1), []) )
  in
  test_pprint_expression expr;
  [%expect {| 5 - 1 |}]
;;

let%expect_test "division with parentheses" =
  let expr =
    Exp_apply
      ( Exp_ident "/"
      , Exp_tuple
          ( Exp_constant (Const_integer 8)
          , Exp_apply
              ( Exp_ident "+"
              , Exp_tuple
                  (Exp_constant (Const_integer 2), Exp_constant (Const_integer 1), []) )
          , [] ) )
  in
  test_pprint_expression expr;
  [%expect {| 8 / (2 + 1) |}]
;;

let%expect_test "nested expression with division" =
  let expr4 =
    Exp_apply
      ( Exp_ident "/"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "+"
              , Exp_tuple
                  (Exp_constant (Const_integer 10), Exp_constant (Const_integer 5), []) )
          , Exp_apply
              ( Exp_ident "*"
              , Exp_tuple
                  (Exp_constant (Const_integer 2), Exp_constant (Const_integer 3), []) )
          , [] ) )
  in
  pprint_expression std_formatter 0 expr4;
  [%expect {| (10 + 5) / (2 * 3) |}]
;;

let%expect_test "nested function calls with precedence" =
  let expr5 =
    Exp_apply
      ( Exp_ident "*"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "-"
              , Exp_tuple
                  ( Exp_constant (Const_integer 8)
                  , Exp_apply
                      ( Exp_ident "+"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 2)
                          , Exp_constant (Const_integer 3)
                          , [] ) )
                  , [] ) )
          , Exp_constant (Const_integer 4)
          , [] ) )
  in
  pprint_expression std_formatter 0 expr5;
  [%expect {| (8 - (2 + 3)) * 4 |}]
;;

let%expect_test "nested operations with multi-level tuples" =
  let expr6 =
    Exp_apply
      ( Exp_ident "+"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "/"
              , Exp_tuple
                  (Exp_constant (Const_integer 18), Exp_constant (Const_integer 3), []) )
          , Exp_apply
              ( Exp_ident "*"
              , Exp_tuple
                  (Exp_constant (Const_integer 2), Exp_constant (Const_integer 4), []) )
          , [] ) )
  in
  pprint_expression std_formatter 0 expr6;
  [%expect {| 18 / 3 + 2 * 4 |}]
;;

let%expect_test "chained operations requiring multiple parentheses" =
  let expr7 =
    Exp_apply
      ( Exp_ident "+"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "-"
              , Exp_tuple
                  ( Exp_constant (Const_integer 10)
                  , Exp_apply
                      ( Exp_ident "/"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 9)
                          , Exp_apply
                              ( Exp_ident "*"
                              , Exp_tuple
                                  ( Exp_constant (Const_integer 2)
                                  , Exp_constant (Const_integer 3)
                                  , [] ) )
                          , [] ) )
                  , [] ) )
          , Exp_constant (Const_integer 5)
          , [] ) )
  in
  pprint_expression std_formatter 0 expr7;
  [%expect {| 10 - 9 / (2 * 3) + 5 |}]
;;

let%expect_test "complex nested arithmetic expression" =
  let expr8 =
    Exp_apply
      ( Exp_ident "-"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "+"
              , Exp_tuple
                  ( Exp_apply
                      ( Exp_ident "-"
                      , Exp_tuple
                          ( Exp_apply
                              ( Exp_ident "/"
                              , Exp_tuple
                                  ( Exp_apply
                                      ( Exp_ident "*"
                                      , Exp_tuple
                                          ( Exp_constant (Const_integer 105)
                                          , Exp_constant (Const_integer 64)
                                          , [] ) )
                                  , Exp_constant (Const_integer 27)
                                  , [] ) )
                          , Exp_apply
                              ( Exp_ident "*"
                              , Exp_tuple
                                  ( Exp_constant (Const_integer 2)
                                  , Exp_apply
                                      ( Exp_ident "*"
                                      , Exp_tuple
                                          ( Exp_constant (Const_integer 5)
                                          , Exp_apply
                                              ( Exp_ident "-"
                                              , Exp_tuple
                                                  ( Exp_constant (Const_integer 5)
                                                  , Exp_constant (Const_integer 1)
                                                  , [] ) )
                                          , [] ) )
                                  , [] ) )
                          , [] ) )
                  , Exp_apply
                      ( Exp_ident "/"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 47)
                          , Exp_constant (Const_integer 64)
                          , [] ) )
                  , [] ) )
          , Exp_apply
              ( Exp_ident "-"
              , Exp_tuple
                  ( Exp_apply
                      ( Exp_ident "*"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 56)
                          , Exp_apply
                              ( Exp_ident "*"
                              , Exp_tuple
                                  ( Exp_constant (Const_integer 57)
                                  , Exp_constant (Const_integer 4)
                                  , [] ) )
                          , [] ) )
                  , Exp_constant (Const_integer 5)
                  , [] ) )
          , [] ) )
  in
  pprint_expression std_formatter 0 expr8;
  [%expect {| 105 * 64 / 27 - 2 * (5 * (5 - 1)) + 47 / 64 - (56 * (57 * 4) - 5) |}]
;;

(* 105 * 64 / 27 - 2 * (5*(5-1)) + 47 / 64 - (56 * (57 *4) - 5) *)

let%expect_test "deeply nested mixed operations with logical operators" =
  let expr =
    Exp_apply
      ( Exp_ident "||"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "||"
              , Exp_tuple
                  ( Exp_apply
                      ( Exp_ident "&&"
                      , Exp_tuple
                          ( Exp_apply
                              ( Exp_ident "<"
                              , Exp_tuple
                                  ( Exp_apply
                                      ( Exp_ident "*"
                                      , Exp_tuple
                                          ( Exp_constant (Const_integer 3)
                                          , Exp_apply
                                              ( Exp_ident "-"
                                              , Exp_tuple
                                                  ( Exp_constant (Const_integer 9)
                                                  , Exp_apply
                                                      ( Exp_ident "/"
                                                      , Exp_tuple
                                                          ( Exp_constant (Const_integer 12)
                                                          , Exp_constant (Const_integer 4)
                                                          , [] ) )
                                                  , [] ) )
                                          , [] ) )
                                  , Exp_constant (Const_integer 7)
                                  , [] ) )
                          , Exp_constant (Const_integer 1)
                          , [] ) )
                  , Exp_apply
                      ( Exp_ident "&&"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 1)
                          , Exp_apply
                              ( Exp_ident "<"
                              , Exp_tuple
                                  ( Exp_constant (Const_integer 5)
                                  , Exp_constant (Const_integer 6)
                                  , [] ) )
                          , [] ) )
                  , [] ) )
          , Exp_apply
              ( Exp_ident "&&"
              , Exp_tuple
                  ( Exp_apply
                      ( Exp_ident "-"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 20)
                          , Exp_apply
                              ( Exp_ident "/"
                              , Exp_tuple
                                  ( Exp_constant (Const_integer 100)
                                  , Exp_apply
                                      ( Exp_ident "+"
                                      , Exp_tuple
                                          ( Exp_constant (Const_integer 4)
                                          , Exp_constant (Const_integer 16)
                                          , [] ) )
                                  , [] ) )
                          , [] ) )
                  , Exp_apply
                      ( Exp_ident "<"
                      , Exp_tuple
                          ( Exp_constant (Const_integer 10)
                          , Exp_constant (Const_integer 12)
                          , [] ) )
                  , [] ) )
          , [] ) )
    (* At this point, you would typically evaluate or inspect `expr` *)
  in
  pprint_expression std_formatter 0 expr;
  [%expect
    {| (3 * (9 - 12 / 4) < 7 && 1 || 1 && 5 < 6) || 20 - 100 / (4 + 16) && 10 < 12 |}]
;;

let%expect_test "let and construct" =
  let program =
    [ Str_eval
        (Exp_apply
           ( Exp_let
               ( Nonrecursive
               , ( { pat = Pat_any; expr = Exp_ident "s" }
                 , [ { pat = Pat_constant (Const_string "fgo"); expr = Exp_ident "ilm" } ]
                 )
               , Exp_ident "j_9" )
           , Exp_construct ("Tep", Some (Exp_ident "ha9")) ))
    ]
  in
  pprint_program std_formatter program;
  [%expect {|
    ((let _ = s and "fgo" = ilm in j_9) (Tep (ha9))) ;; |}]
;;

(*(let _ s = "fgo" -> __im in j_9) Tep (ha9);;*)

let%expect_test "if with parenthesis" =
  let program =
    [ Str_eval
        (Exp_tuple
           ( Exp_tuple
               (Exp_ident "c6BR_J", Exp_constant (Const_string ""), [ Exp_ident "j_v_" ])
           , Exp_if
               ( Exp_constant (Const_string "hgdpg")
               , Exp_ident "_T"
               , Some (Exp_constant (Const_integer 80)) )
           , [ Exp_let
                 ( Nonrecursive
                 , ( { pat = Pat_constant (Const_integer 69)
                     ; expr = Exp_constant (Const_integer 8)
                     }
                   , [ { pat = Pat_constant (Const_integer 5)
                       ; expr = Exp_constant (Const_char 'd')
                       }
                     ] )
                 , Exp_constant (Const_integer 4) )
             ] ))
    ]
  in
  pprint_program std_formatter program;
  [%expect
    {|
    ((c6BR_J, "", j_v_), (if "hgdpg"
      then _T
      else 80), (let 69 = 8 and 5 = 'd' in 4)) ;; |}]
;;

let%expect_test "let binding with integer" =
  let program =
    [ Str_value
        (Nonrecursive, ({ pat = Pat_var "x"; expr = Exp_constant (Const_integer 42) }, []))
    ]
  in
  pprint_program std_formatter program;
  [%expect {|
    let  x = 42;;
  |}]
;;

let%expect_test "if expression" =
  let program =
    [ Str_eval
        (Exp_if
           ( Exp_apply
               ( Exp_ident ">"
               , Exp_tuple (Exp_ident "x", Exp_constant (Const_integer 10), []) )
           , Exp_ident "large"
           , Some (Exp_ident "small") ))
    ]
  in
  pprint_program std_formatter program;
  [%expect {|
    if x > 10
      then large
      else small ;;
  |}]
;;

let%expect_test "tuple and match expression" =
  let program =
    [ Str_eval
        (Exp_match
           ( Exp_tuple (Exp_ident "a", Exp_ident "b", [ Exp_ident "c" ])
           , ( { first = Pat_tuple (Pat_var "x", Pat_var "y", [ Pat_var "z" ])
               ; second =
                   Exp_apply (Exp_ident "f", Exp_tuple (Exp_ident "x", Exp_ident "y", []))
               }
             , [] ) ))
    ]
  in
  pprint_program std_formatter program;
  [%expect {|
    match (a, b, c) with
      | (x, y, z) -> (f (x, y)) ;;
  |}]
;;

let%expect_test "nested constructs" =
  let program =
    [ Str_eval
        (Exp_apply
           ( Exp_ident "map"
           , Exp_tuple
               ( Exp_function
                   ( { first = Pat_var "x"
                     ; second =
                         Exp_apply
                           ( Exp_ident "*"
                           , Exp_tuple (Exp_ident "x", Exp_constant (Const_integer 2), [])
                           )
                     }
                   , [] )
               , Exp_ident "list"
               , [] ) ))
    ]
  in
  pprint_program std_formatter program;
  [%expect {|
    (map ((function
      | x -> x * 2), list)) ;;
  |}]
;;

let%expect_test "construct with optional arguments" =
  let program =
    [ Str_eval (Exp_construct ("Some", Some (Exp_constant (Const_string "Hello")))) ]
  in
  pprint_program std_formatter program;
  [%expect {|
    (Some ("Hello")) ;;
  |}]
;;

let%expect_test "complex program" =
  let program =
    [ Str_eval
        (Exp_let
           ( Nonrecursive
           , ({ pat = Pat_var "x"; expr = Exp_constant (Const_integer 5) }, [])
           , Exp_if
               ( Exp_apply
                   ( Exp_ident ">"
                   , Exp_tuple (Exp_ident "x", Exp_constant (Const_integer 0), []) )
               , Exp_construct ("Some", Some (Exp_ident "x"))
               , Some (Exp_construct ("None", None)) ) ))
    ]
  in
  pprint_program std_formatter program;
  [%expect {|
    let x = 5 in (if x > 0
      then (Some (x))
      else (None)) ;;
  |}]
;;
