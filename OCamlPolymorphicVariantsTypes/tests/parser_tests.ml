(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

(* Tests for identifier parser *)
let%expect_test _ =
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "x"));
  [%expect {| "x" |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "fact5"));
  [%expect {| "fact5" |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "_"));
  [%expect {| "_" |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "0var"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident ""));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "123"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "true"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_identifier_parse_result (parse ident "then"));
  [%expect {| Parse process failed |}]
;;

(* Tests for pattern parser *)
let%expect_test _ =
  Format.printf "%s" (string_of_pattern_parse_result (parse pvariable "x"));
  [%expect {| (PVar "x") |}];
  Format.printf "%s" (string_of_pattern_parse_result (parse ptuple "((), (x, y))"));
  [%expect {| (PTuple [PUnit; (PTuple [(PVar "x"); (PVar "y")])]) |}];
  Format.printf "%s" (string_of_pattern_parse_result (parse pattern_parser "x"));
  [%expect {| (PVar "x") |}];
  Format.printf "%s" (string_of_pattern_parse_result (parse pattern_parser "(x,y)"));
  [%expect {| (PTuple [(PVar "x"); (PVar "y")]) |}]
;;

(* Tests for literal parsers *)
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

(* Tests for expressin parsers *)
let%expect_test _ =
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr {|  123|}));
  [%expect {| (Const (IntLiteral 123)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr {|    true|}));
  [%expect {| (Const (BoolLiteral true)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr "trua"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_expression_parse_result (parse variable "x"));
  [%expect {| (Variable "x") |}];
  Format.printf "%s" (string_of_expression_parse_result (parse variable "if"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_expression_parse_result (parse unary_expr "+  12"));
  [%expect {| (Const (IntLiteral 12)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse unary_expr "  ~-123"));
  [%expect {| (Unary (Negate, (Const (IntLiteral 123)))) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "-(1234)"));
  [%expect {| (Unary (Negate, (Const (IntLiteral 1234)))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result
       (parse basic_expr "(true, -12, (789; 45        ))"));
  [%expect
    {|
    (Tuple
       [(Const (BoolLiteral true)); (Unary (Negate, (Const (IntLiteral 12))));
         (ExpressionBlock [(Const (IntLiteral 789)); (Const (IntLiteral 45))])]) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "(f x; f y)"));
  [%expect
    {|
    (ExpressionBlock
       [(Apply ((Variable "f"), [(Variable "x")]));
         (Apply ((Variable "f"), [(Variable "y")]))]) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "(true, )"));
  [%expect {| ParseError(line=1 pos=6): Not found elements after separator: ',' |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse basic_expr "(true; 12  ;)"));
  [%expect {| ParseError(line=1 pos=12): Not found elements after separator: ';' |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "()"));
  [%expect {| (Const UnitLiteral) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse basic_expr "(12,34"));
  [%expect {| ParseError(line=1 pos=6): Not found close bracket |}];
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
       )) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse if_expr "if true || false then 12 else 34"));
  [%expect
    {|
    (If ((Binary ((Const (BoolLiteral true)), Or, (Const (BoolLiteral false)))),
       (Const (IntLiteral 12)), (Some (Const (IntLiteral 34))))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse if_expr "if true || false then 12"));
  [%expect
    {|
    (If ((Binary ((Const (BoolLiteral true)), Or, (Const (BoolLiteral false)))),
       (Const (IntLiteral 12)), None)) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse if_expr "if true || false then 12 else 34"));
  [%expect
    {|
    (If ((Binary ((Const (BoolLiteral true)), Or, (Const (BoolLiteral false)))),
       (Const (IntLiteral 12)), (Some (Const (IntLiteral 34))))) |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse if_expr "if true || false then 12 else"));
  [%expect
    {| ParseError(line=1 pos=29): Expected expression of 'else' branch for if expression |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse if_expr "if true || false then"));
  [%expect
    {| ParseError(line=1 pos=21): Expected expression of 'then' branch for if expression |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse if_expr "if true || false"));
  [%expect {| ParseError(line=1 pos=16): Not found 'then' branch for if-expression |}];
  Format.printf "%s" (string_of_expression_parse_result (parse if_expr "if"));
  [%expect {| ParseError(line=1 pos=0): Not found if expression after keyword 'if' |}];
  Format.printf
    "%s"
    (string_of_expression_parse_result (parse apply_expr "print_int (12;true; -12)"));
  [%expect
    {|
    (Apply ((Variable "print_int"),
       [(ExpressionBlock
           [(Const (IntLiteral 12)); (Const (BoolLiteral true));
             (Unary (Negate, (Const (IntLiteral 12))))])
         ]
       )) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse apply_expr "(f x) y"));
  [%expect
    {|
    (Apply ((Apply ((Variable "f"), [(Variable "x")])), [(Variable "y")])) |}]
;;

(* applyable : var |  lambda | let .. in e *)
