(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Structure
open Parse.Expressions

(************************** Structure items **************************)

let%expect_test "parse structure item which is single expression" =
  pp pp_structure_item parse_structure_item {| a |};
  [%expect {| (Str_item_eval (Expr_ident_or_op "a")) |}]
;;

let%expect_test "parse structure item which is do expr" =
  pp pp_structure_item parse_structure_item {| do a |};
  [%expect {| (Str_item_eval (Expr_ident_or_op "a")) |}]
;;

let%expect_test "parse structure item which is single let binding" =
  pp pp_structure_item parse_structure_item {| let x = y |};
  [%expect
    {|
    (Str_item_def (Nonrecursive,
       (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "y"))), [])) |}]
;;

let%expect_test "parse structure item which is single rec let binding" =
  pp pp_structure_item parse_structure_item {| let rec x = x |};
  [%expect
    {|
    (Str_item_def (Recursive,
       (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "x"))), [])) |}]
;;

let%expect_test "parse structure item which is multiple let bindings" =
  pp
    pp_structure_item
    parse_structure_item
    {|let a = b
      and c = d
      and e = f
      and i = j|};
  [%expect
    {|
    (Str_item_def (Nonrecursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_ident_or_op "b"))),
       [(Bind ((Pattern_ident_or_op "c"), (Expr_ident_or_op "d")));
         (Bind ((Pattern_ident_or_op "e"), (Expr_ident_or_op "f")));
         (Bind ((Pattern_ident_or_op "i"), (Expr_ident_or_op "j")))]
       ))|}]
;;

(* Doesn't halt on 4+ nested let ... in's, the reason is in parse_expr_let *)
(* let%expect_test "parse structure item which is nested let bindings" =
  pp
    pp_structure_item
    parse_structure_item
    {| let a = f in
         let b = g in
         let c = h in
         let d = j in
         E |};
  [%expect
    {|
    (Str_item_eval
       (Expr_let (Nonrecursive,
          (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 1)))),
          [],
          (Expr_let (Nonrecursive,
             (Bind ((Pattern_ident_or_op "b"), (Expr_const (Const_int 2)))),
             [],
             (Expr_let (Nonrecursive,
                (Bind ((Pattern_ident_or_op "c"), (Expr_const (Const_int 3)))),
                [], (Expr_ident_or_op "e")))
             ))
          )))|}]
;; *)

let%expect_test "parse factorial" =
  pp
    pp_structure_item
    parse_structure_item
    {|let rec factorial n =
        if (n < 2) then 1
        else n * factorial (n - 1) |};
  [%expect
    {|
    (Str_item_def (Recursive,
       (Bind ((Pattern_ident_or_op "factorial"),
          (Expr_lam ((Pattern_ident_or_op "n"),
             (Expr_ifthenelse (
                (Expr_apply (
                   (Expr_apply ((Expr_ident_or_op "<"), (Expr_ident_or_op "n"))),
                   (Expr_const (Const_int 2)))),
                (Expr_const (Const_int 1)),
                (Some (Expr_apply (
                         (Expr_apply ((Expr_ident_or_op "*"),
                            (Expr_ident_or_op "n"))),
                         (Expr_apply ((Expr_ident_or_op "factorial"),
                            (Expr_apply (
                               (Expr_apply ((Expr_ident_or_op "-"),
                                  (Expr_ident_or_op "n"))),
                               (Expr_const (Const_int 1))))
                            ))
                         )))
                ))
             ))
          )),
       []))|}]
;;

(************************** Programs **************************)

let%expect_test "parse example 1 program" =
  pp pp_program parse_program {| '_' [];; |};
  [%expect
    {|
    [(Str_item_eval (Expr_apply ((Expr_const (Const_char '_')), (Expr_list []))))
      ] |}]
;;

let%expect_test "parse example 2 program" =
  pp
    pp_program
    parse_program
    {| let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;; |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_ident_or_op "_Sy"), (Expr_const (Const_string "<X&D(")))),
        [(Bind ((Pattern_ident_or_op "_QQf"), (Expr_ident_or_op "tOWs")));
          (Bind (Pattern_wild,
             (Expr_apply ((Expr_ident_or_op "-"),
                (Expr_const
                   (Const_unit_of_measure
                      (Unit_of_measure ((Mnum_float 3625.090462),
                         (Measure_ident "b9o'5")))))
                ))
             ))
          ]
        ))
      ] |}]
;;

let%expect_test "parse simple binding as a program" =
  pp pp_program parse_program {| let x = y |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "y"))), []))
      ] |}]
;;

let%expect_test "parse program of bindings separated by ;;" =
  pp pp_program parse_program {|
     let x = y;; let z = 5000
     |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "y"))), []));
      (Str_item_def (Nonrecursive,
         (Bind ((Pattern_ident_or_op "z"), (Expr_const (Const_int 5000)))),
         []))
      ] |}]
;;

(* Not done yet *)
let%expect_test "parse program of bindings separated by newline" =
  pp pp_program parse_program {|
    let x = y
    let z = w
    |};
  [%expect {|
    : end_of_input |}]
;;
