(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Base
open Pprinter
open Format

let%expect_test "print identificator" =
  printf "%a\n" pprint_ident "myident";
  [%expect {| myident |}]
;;

(************************** Constants **************************)

let%expect_test "print int constant" =
  printf "%a\n" pprint_const (Const_int 123);
  [%expect {| 123 |}]
;;

let%expect_test "print char constant" =
  printf "%a\n" pprint_const (Const_char 'a');
  [%expect {| 'a' |}]
;;

let%expect_test "print string constant" =
  printf "%a\n" pprint_const (Const_string "gorillaz");
  [%expect {| "gorillaz" |}]
;;

let%expect_test "print float constant" =
  printf "%a\n" pprint_const (Const_float 3.142222222);
  [%expect {| 3.142222 |}]
;;

(************************** Types **************************)

let%expect_test "print type int" =
  printf "%a\n" pprint_type (Type_ident "int");
  [%expect {| int |}]
;;

let%expect_test "print type int -> int" =
  printf "%a\n" pprint_type (Type_func (Type_ident "int", Type_ident "int"));
  [%expect {| int -> int |}]
;;

let%expect_test "print type int -> int -> int" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_ident "int", Type_func (Type_ident "int", Type_ident "int")));
  [%expect {| int -> int -> int |}]
;;

let%expect_test "print type int -> (int -> int) and omit parentheses" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_ident "int", Type_func (Type_ident "int", Type_ident "int")));
  [%expect {| int -> int -> int |}]
;;

let%expect_test "print type (int -> int) -> int" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_func (Type_ident "int", Type_ident "int"), Type_ident "int"));
  [%expect {| (int -> int) -> int |}]
;;

let%expect_test "print type int * int" =
  printf "%a\n" pprint_type (Type_tuple (Type_ident "int", Type_ident "int", []));
  [%expect {| int * int |}]
;;

let%expect_test "print type int * int * int" =
  printf
    "%a\n"
    pprint_type
    (Type_tuple (Type_ident "int", Type_ident "int", [ Type_ident "int" ]));
  [%expect {| int * int * int |}]
;;

let%expect_test "print type int * int -> int" =
  printf
    "%a\n"
    pprint_type
    (Type_func (Type_tuple (Type_ident "int", Type_ident "int", []), Type_ident "int"));
  [%expect {| int * int -> int |}]
;;

(************************** Patterns **************************)

let%expect_test "print identificator pattern" =
  printf "%a\n" pprint_pat (Pattern_ident_or_op "ident");
  [%expect {| ident |}]
;;

let%expect_test "print int constant pattern" =
  printf "%a\n" pprint_pat (Pattern_const (Const_int 123));
  [%expect {| 123 |}]
;;

let%expect_test "print wildcard pattern" =
  printf "%a\n" pprint_pat Pattern_wild;
  [%expect {| _ |}]
;;

let%expect_test "print OR pattern" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_or (Pattern_ident_or_op "P1", Pattern_ident_or_op "P2"));
  [%expect {| P1 | P2 |}]
;;

let%expect_test "print x : int typed pattern" =
  printf "%a\n" pprint_pat (Pattern_typed (Pattern_ident_or_op "x", Type_ident "int"));
  [%expect {| x : int |}]
;;

let%expect_test "print (x, y) tuple pattern" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_tuple (Pattern_ident_or_op "x", Pattern_ident_or_op "y", []));
  [%expect {| (x, y) |}]
;;

let%expect_test "print (x, y, z) tuple pattern" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_tuple
       (Pattern_ident_or_op "x", Pattern_ident_or_op "y", [ Pattern_ident_or_op "z" ]));
  [%expect {| (x, y, z) |}]
;;

let%expect_test "print (x : int, y: int) tuple of typed idents pattern" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_tuple
       ( Pattern_typed (Pattern_ident_or_op "x", Type_ident "int")
       , Pattern_typed (Pattern_ident_or_op "y", Type_ident "int")
       , [] ));
  [%expect {| (x : int, y : int) |}]
;;

let%expect_test "print [] list pattern" =
  printf "%a\n" pprint_pat (Pattern_list []);
  [%expect {| [] |}]
;;

let%expect_test "print [x] list pattern" =
  printf "%a\n" pprint_pat (Pattern_list [ Pattern_ident_or_op "x" ]);
  [%expect {| [x] |}]
;;

let%expect_test "print [x; y] list pattern" =
  printf
    "%a\n"
    pprint_pat
    (Pattern_list [ Pattern_ident_or_op "x"; Pattern_ident_or_op "y" ]);
  [%expect {| [x; y] |}]
;;

(************************** Expressions **************************)

let%expect_test "print int constant expression" =
  printf "%a\n" pprint_expr (Expr_const (Const_int 123));
  [%expect {| 123 |}]
;;

let%expect_test "print identificator expression" =
  printf "%a\n" pprint_expr (Expr_ident_or_op "x");
  [%expect {| x |}]
;;

let%expect_test "print (x, y) tuple expression" =
  printf "%a\n" pprint_expr (Expr_tuple (Expr_ident_or_op "x", Expr_ident_or_op "y", []));
  [%expect {| (x, y) |}]
;;

let%expect_test "print (x, y, z) tuple expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_tuple (Expr_ident_or_op "x", Expr_ident_or_op "y", [ Expr_ident_or_op "z" ]));
  [%expect {| (x, y, z) |}]
;;

let%expect_test "print [] list expression" =
  printf "%a\n" pprint_expr (Expr_list []);
  [%expect {| [] |}]
;;

let%expect_test "print [x] list expression" =
  printf "%a\n" pprint_expr (Expr_list [ Expr_ident_or_op "x" ]);
  [%expect {| [x] |}]
;;

let%expect_test "print [x; y] list expression" =
  printf "%a\n" pprint_expr (Expr_list [ Expr_ident_or_op "x"; Expr_ident_or_op "y" ]);
  [%expect {| [x; y] |}]
;;

let%expect_test "print lambda expression" =
  printf "%a\n" pprint_expr (Expr_lam (Pattern_ident_or_op "x", Expr_ident_or_op "x"));
  [%expect {| fun x -> x |}]
;;

let%expect_test "print let a = 5 in a expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_let
       ( Nonrecursive
       , Bind (Pattern_ident_or_op "a", Expr_const (Const_int 5))
       , []
       , Expr_ident_or_op "a" ));
  [%expect {| let a = 5 in a |}]
;;

let%expect_test "print let rec a = 5 in a expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_let
       ( Recursive
       , Bind (Pattern_ident_or_op "a", Expr_const (Const_int 5))
       , []
       , Expr_ident_or_op "a" ));
  [%expect {| let rec a = 5 in a |}]
;;

let%expect_test "print let a = 5 and b = 4 in e expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_let
       ( Nonrecursive
       , Bind (Pattern_ident_or_op "a", Expr_const (Const_int 5))
       , [ Bind (Pattern_ident_or_op "b", Expr_const (Const_int 4)) ]
       , Expr_ident_or_op "e" ));
  [%expect {| let a = 5 and b = 4 in e |}]
;;

let%expect_test "print nested let .. in expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_let
       ( Nonrecursive
       , Bind (Pattern_ident_or_op "a", Expr_const (Const_int 1))
       , []
       , Expr_let
           ( Nonrecursive
           , Bind (Pattern_ident_or_op "b", Expr_const (Const_int 2))
           , []
           , Expr_let
               ( Nonrecursive
               , Bind (Pattern_ident_or_op "c", Expr_const (Const_int 3))
               , []
               , Expr_ident_or_op "e" ) ) ));
  [%expect {| let a = 1 in let b = 2 in let c = 3 in e |}]
;;

let%expect_test "print let f a b c = x in e" =
  printf
    "%a\n"
    pprint_expr
    (Expr_let
       ( Nonrecursive
       , Bind
           ( Pattern_ident_or_op "f"
           , Expr_lam
               ( Pattern_ident_or_op "a"
               , Expr_lam
                   ( Pattern_ident_or_op "b"
                   , Expr_lam (Pattern_ident_or_op "c", Expr_ident_or_op "x") ) ) )
       , []
       , Expr_ident_or_op "e" ));
  [%expect {| let f = fun a -> fun b -> fun c -> x in e |}]
;;

let%expect_test "print if then expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_ifthenelse (Expr_ident_or_op "cond", Expr_ident_or_op "e", None));
  [%expect {| if cond then e |}]
;;

let%expect_test "print if then else expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_ifthenelse
       (Expr_ident_or_op "cond", Expr_ident_or_op "e1", Some (Expr_ident_or_op "e2")));
  [%expect {| if cond then e1 else e2 |}]
;;

let%expect_test "print match x with P1 -> E1 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_match
       (Expr_ident_or_op "x", Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1"), []));
  [%expect {| match x with P1 -> E1 |}]
;;

let%expect_test "print match x with P1 -> E1 | P2 -> E2 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_match
       ( Expr_ident_or_op "x"
       , Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1")
       , [ Rule (Pattern_ident_or_op "P2", Expr_ident_or_op "E2") ] ));
  [%expect {| match x with P1 -> E1 | P2 -> E2 |}]
;;

let%expect_test "print match x with P1 | P2 | P3 -> E1 | P4 -> E2 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_match
       ( Expr_ident_or_op "x"
       , Rule
           ( Pattern_or
               ( Pattern_or (Pattern_ident_or_op "P1", Pattern_ident_or_op "P2")
               , Pattern_ident_or_op "P3" )
           , Expr_ident_or_op "E1" )
       , [ Rule (Pattern_ident_or_op "P4", Expr_ident_or_op "E2") ] ));
  [%expect {| match x with P1 | P2 | P3 -> E1 | P4 -> E2 |}]
;;

let%expect_test "print match x with P1 | P2 | P3 -> E1 | P4 -> E2 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_match
       ( Expr_ident_or_op "x"
       , Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1")
       , [ Rule
             ( Pattern_or
                 ( Pattern_or (Pattern_ident_or_op "P2", Pattern_ident_or_op "P3")
                 , Pattern_ident_or_op "P4" )
             , Expr_ident_or_op "E2" )
         ] ));
  [%expect {| match x with P1 -> E1 | P2 | P3 | P4 -> E2 |}]
;;

let%expect_test "print function P1 -> E1 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_function (Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1"), []));
  [%expect {| function P1 -> E1 |}]
;;

let%expect_test "print function P1 -> E1 | P2 -> E2 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_function
       ( Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1")
       , [ Rule (Pattern_ident_or_op "P2", Expr_ident_or_op "E2") ] ));
  [%expect {| function P1 -> E1 | P2 -> E2 |}]
;;

let%expect_test "print function P1 | P2 -> E1 | P3 -> E2 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_function
       ( Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1")
       , [ Rule (Pattern_ident_or_op "P2", Expr_ident_or_op "E2") ] ));
  [%expect {| function P1 -> E1 | P2 -> E2 |}]
;;

let%expect_test "print function P1 -> E1 | P2 | P3 -> E2 expression" =
  printf
    "%a\n"
    pprint_expr
    (Expr_function
       ( Rule (Pattern_ident_or_op "P1", Expr_ident_or_op "E1")
       , [ Rule (Pattern_ident_or_op "P2", Expr_ident_or_op "E2") ] ));
  [%expect {| function P1 -> E1 | P2 -> E2 |}]
;;

let%expect_test "print f x expression" =
  printf "%a\n" pprint_expr (Expr_apply (Expr_ident_or_op "f", Expr_ident_or_op "x"));
  [%expect {| f x |}]
;;

let%expect_test "print (if true then f else g) x (with parentheses)" =
  printf
    "%a\n"
    pprint_expr
    (Expr_apply
       ( Expr_ifthenelse
           ( Expr_const (Const_bool true)
           , Expr_ident_or_op "f"
           , Some (Expr_ident_or_op "g") )
       , Expr_ident_or_op "x" ));
  [%expect {| (if true then f else g) x |}]
;;

let%expect_test "print f (if true then x else y) (with parentheses)" =
  printf
    "%a\n"
    pprint_expr
    (Expr_apply
       ( Expr_ident_or_op "f"
       , Expr_ifthenelse
           ( Expr_const (Const_bool true)
           , Expr_ident_or_op "x"
           , Some (Expr_ident_or_op "y") ) ));
  [%expect {| f (if true then x else y) |}]
;;

let%expect_test "print single structure item expression" =
  printf "%a\n" pprint_program [ Str_item_eval (Expr_const (Const_int 1)) ];
  [%expect {| 1;; |}]
;;

let%expect_test "print two structure item expressions sep by ;;" =
  printf
    "%a\n"
    pprint_program
    [ Str_item_eval
        (Expr_let
           ( Nonrecursive
           , Bind (Pattern_ident_or_op "a", Expr_const (Const_int 1))
           , []
           , Expr_ident_or_op "a" ))
    ; Str_item_eval
        (Expr_let
           ( Nonrecursive
           , Bind (Pattern_ident_or_op "b", Expr_const (Const_int 2))
           , []
           , Expr_ident_or_op "b" ))
    ];
  [%expect {|
    let a = 1 in a;;

    let b = 2 in b;; |}]
;;

let%expect_test "print single structure item definition" =
  printf
    "%a\n"
    pprint_program
    [ Str_item_def
        (Nonrecursive, Bind (Pattern_ident_or_op "a", Expr_const (Const_int 1)), [])
    ];
  [%expect {| let a = 1;; |}]
;;

let%expect_test "print two structure item definitions sep by ;;" =
  printf
    "%a\n"
    pprint_program
    [ Str_item_def
        (Nonrecursive, Bind (Pattern_ident_or_op "a", Expr_const (Const_int 1)), [])
    ; Str_item_def
        (Nonrecursive, Bind (Pattern_ident_or_op "b", Expr_const (Const_int 2)), [])
    ];
  [%expect {|
    let a = 1;;

    let b = 2;; |}]
;;
