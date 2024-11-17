(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Structure

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
    (Str_item_def (Nonrecursive, ((Pattern_ident "x"), (Expr_ident_or_op "y")),
       [])) |}]
;;

let%expect_test "parse structure item which is single rec let binding" =
  pp pp_structure_item parse_structure_item {| let rec x = x |};
  [%expect
    {|
    (Str_item_def (Recursive, ((Pattern_ident "x"), (Expr_ident_or_op "x")), [])) |}]
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
    (Str_item_def (Nonrecursive, ((Pattern_ident "a"), (Expr_ident_or_op "b")),
       [((Pattern_ident "c"), (Expr_ident_or_op "d"));
         ((Pattern_ident "e"), (Expr_ident_or_op "f"));
         ((Pattern_ident "i"), (Expr_ident_or_op "j"))]
       ))|}]
;;

let%expect_test "parse structure item which is nested let bindings" =
  pp
    pp_structure_item
    parse_structure_item
    {|let a = b in
        let c = d in
        let e = f in
        let i = j in
        somefunc
    |};
  [%expect
    {|
    (Str_item_eval
       (Expr_let (Nonrecursive, ((Pattern_ident "a"), (Expr_ident_or_op "b")),
          [],
          (Expr_let (Nonrecursive, ((Pattern_ident "c"), (Expr_ident_or_op "d")),
             [],
             (Expr_let (Nonrecursive,
                ((Pattern_ident "e"), (Expr_ident_or_op "f")), [],
                (Expr_let (Nonrecursive,
                   ((Pattern_ident "i"), (Expr_ident_or_op "j")), [],
                   (Expr_ident_or_op "somefunc")))
                ))
             ))
          )))|}]
;;

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
       ((Pattern_ident "factorial"),
        (Expr_fun ((Pattern_ident "n"),
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
           ))),
       []))|}]
;;

(************************** Programs **************************)

let%expect_test "parse simple binding as a program" =
  pp pp_program parse_program {| let x = y |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive, ((Pattern_ident "x"), (Expr_ident_or_op "y")),
        []))
      ] |}]
;;

let%expect_test "parse program of bindings separated by ;;" =
  pp pp_program parse_program {|
     let x = y;; let z = 5000
     |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive, ((Pattern_ident "x"), (Expr_ident_or_op "y")),
        []));
      (Str_item_def (Nonrecursive,
         ((Pattern_ident "z"), (Expr_const (Const_int 5000))), []))
      ] |}]
;;

(* Not done yet *)
let%expect_test "parse program of bindings separated by newline" =
  pp pp_program parse_program {|
    let x = y
    let z = w
    |};
  [%expect
    {|
    : end_of_input |}]
;;
