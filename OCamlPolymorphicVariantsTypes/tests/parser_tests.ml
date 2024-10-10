(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

(* Test for literal parsers *)
let%expect_test _ =
  Format.printf "%s" (string_of_literal_parse_result (parse integer "123"));
  [%expect {| (IntLiteral 123) |}];
  Format.printf
    "%s"
    (string_of_literal_parse_result (parse integer (string_of_int Int.max_int)));
  [%expect {| (IntLiteral 4611686018427387903) |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "true"));
  [%expect {| (BoolLiteral true) |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "false"));
  [%expect {| (BoolLiteral false) |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "faalse"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "trua"));
  [%expect {| Parse process failed |}]
;;

(* Test for expressin parsers *)
let%expect_test _ =
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr {|  123|}));
  [%expect {| (Const (IntLiteral 123)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr {|    true|}));
  [%expect {| (Const (BoolLiteral true)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr "trua"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_expression_parse_result (parse unary_expr "+  12"));
  [%expect {| (Const (IntLiteral 12)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse unary_expr "  ~-123"));
  [%expect {| (Unary (Negate, (Const (IntLiteral 123)))) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "-(1234)"));
  [%expect {| (Unary (Negate, (Const (IntLiteral 1234)))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse basic_expr "(true, -12, (789; 45))"));
  [%expect
    {|
    (Tuple
       [(Const (BoolLiteral true)); (Unary (Negate, (Const (IntLiteral 12))));
         (ExpressionBlock [(Const (IntLiteral 789)); (Const (IntLiteral 45))])]) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "(true, )"));
  [%expect {|
    ParseError(line=1 pos=6): Not found elements after separator: ',' |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse basic_expr "(true; 12  ;)"));
  [%expect {|
    ParseError(line=1 pos=12): Not found elements after separator: ';' |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "()"));
  [%expect {|
    (Const UnitLiteral) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "(12,34"));
  [%expect {|
    ParseError(line=1 pos=6): Not found close bracket |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result
       (parse
          basic_expr
          {|     (  
       12, 
       ((((   (   
       (   23  ; 
          45))))   
          ))   )
          |}));
  [%expect
    {|
    (Tuple
       [(Const (IntLiteral 12));
         (ExpressionBlock [(Const (IntLiteral 23)); (Const (IntLiteral 45))])]) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "~-123"));
  [%expect {| (Unary (Negate, (Const (IntLiteral 123)))) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "true"));
  [%expect {| (Const (BoolLiteral true)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse multiply_expr "12 * 45"));
  [%expect {| (Binary ((Const (IntLiteral 12)), Multiply, (Const (IntLiteral 45)))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse multiply_expr "60 * 789 / 12"));
  [%expect
    {|
    (Binary (
       (Binary ((Const (IntLiteral 60)), Multiply, (Const (IntLiteral 789)))),
       Division, (Const (IntLiteral 12)))) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse summary_expr "12 + 45"));
  [%expect {| (Binary ((Const (IntLiteral 12)), Add, (Const (IntLiteral 45)))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse summary_expr "45 - 789 + 12"));
  [%expect
    {|
    (Binary (
       (Binary ((Const (IntLiteral 45)), Subtract, (Const (IntLiteral 789)))),
       Add, (Const (IntLiteral 12)))) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse compare_expr "12 > 45"));
  [%expect {| (Binary ((Const (IntLiteral 12)), Gt, (Const (IntLiteral 45)))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse compare_expr "12 <= 45 - 45"));
  [%expect
    {|
    (Binary ((Const (IntLiteral 12)), Lte,
       (Binary ((Const (IntLiteral 45)), Subtract, (Const (IntLiteral 45)))))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse boolean_expr "true && false"));
  [%expect {| (Binary ((Const (BoolLiteral true)), And, (Const (BoolLiteral false)))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse boolean_expr "45 = 45 || 35 / 2 > 90"));
  [%expect
    {|
    (Binary ((Binary ((Const (IntLiteral 45)), Equals, (Const (IntLiteral 45)))),
       Or,
       (Binary (
          (Binary ((Const (IntLiteral 35)), Division, (Const (IntLiteral 2)))),
          Gt, (Const (IntLiteral 90))))
       )) |}]
;;