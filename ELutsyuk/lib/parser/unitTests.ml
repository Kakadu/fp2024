(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open PrsAuxilary
open Const
open Patterns
open Expr

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"
;;

(* ================================= auxiliary ================================= *)

let%expect_test "normal_id" =
  pp Format.pp_print_string prs_id "_id";
  [%expect {| _id |}]
;;

let%expect_test "incorrect_id_num" =
  pp Format.pp_print_string prs_id "1id";
  [%expect {| Syntax error |}]
;;

let%expect_test "keyword_id" =
  pp Format.pp_print_string prs_id "let";
  [%expect {| Syntax error |}]
;;

(* ================================= constants ================================= *)

let%expect_test "num_without_sign_before" =
  pp pp_const prs_int "2024";
  [%expect {| (Int 2024) |}]
;;

let%expect_test "quoted_string_with_double_slash" =
  pp pp_const prs_str {|"str\\meow"|};
  [%expect {| (Str "str\\meow") |}]
;;

let%expect_test "regular_string_with_double_slash" =
  pp pp_const prs_str {|"str\\meow"|};
  [%expect {| (Str "str\\meow") |}]
;;

let%expect_test "regular_string_with_slash" =
  pp pp_const prs_str {|"str\meow"|};
  [%expect {| Syntax error |}]
;;

let%expect_test "regular_unclosed_string" =
  pp pp_const prs_str {|"str|};
  [%expect {| Syntax error |}]
;;

let%expect_test "quoted_unclosed_string" =
  pp pp_const prs_str "{|str";
  [%expect {| Syntax error |}]
;;

let%expect_test "quoted_empty_string" =
  pp pp_const prs_str "\"\"";
  [%expect {| (Str "") |}]
;;

let%expect_test "regular_empty_string" =
  pp pp_const prs_str "{||}";
  [%expect {| (Str "") |}]
;;

let%expect_test "regular_string_one_space" =
  pp pp_const prs_str "\" \"";
  [%expect {| (Str " ") |}]
;;

let%expect_test "quoted_string_one_space" =
  pp pp_const prs_str "{| |}";
  [%expect {| (Str " ") |}]
;;

let%expect_test "quoted_string_two_spaces" =
  pp pp_const prs_str "{|  |}";
  [%expect {| (Str "  ") |}]
;;

let%expect_test "quoted_string_with_text" =
  pp pp_const prs_str "{| hello |}";
  [%expect {| (Str " hello ") |}]
;;

let%expect_test "regular_string_escape_sequence" =
  pp pp_const prs_str "\"Hex\\x41\\x42\\x43\"";
  [%expect {| (Str "HexABC") |}]
;;

let%expect_test "regular_string_incorrect_escape_sequence" =
  pp pp_const prs_str "\"meow\n\"";
  [%expect {| (Str "meow\n") |}]
;;

let%expect_test "true" =
  pp pp_const prs_bool "true";
  [%expect {| (Bool true) |}]
;;

let%expect_test "false" =
  pp pp_const prs_bool "false";
  [%expect {| (Bool false) |}]
;;

let%expect_test "incorrect_bool_with_char_after" =
  pp pp_const prs_bool "truee";
  [%expect {| Syntax error |}]
;;

let%expect_test "unit" =
  pp pp_const prs_unit "()";
  [%expect {| Unit |}]
;;

(* ================================== patterns ================================= *)

let%expect_test "pat_var" =
  pp pp_pat prs_pat_var "meow\n";
  [%expect {| (PatVar "meow") |}]
;;

let%expect_test "pat_constant" =
  pp pp_pat prs_pat_constant "  \r{|meow|}\n";
  [%expect {| (PatConst (Str "meow")) |}]
;;

let%expect_test "pat_constructor" =
  pp pp_pat (prs_pat_cons prs_pat) "a :: []";
  [%expect {| (PatListCons ((PatVar "a"), (PatList []))) |}]
;;

let%expect_test "pat_empty_str" =
  pp pp_pat prs_pat_constant "  \r{||}\n";
  [%expect {| (PatConst (Str "")) |}]
;;

let%expect_test "pat_any" =
  pp pp_pat prs_pat_any "\r_\n";
  [%expect {| PatAny |}]
;;

let%expect_test "pat_simple_tuple" =
  pp pp_pat prs_pat "1, 2, 3";
  [%expect
    {|
      (PatTup ((PatConst (Int 1)), (PatConst (Int 2)), [(PatConst (Int 3))])) |}]
;;

let%expect_test "incorrect_pat_tuple_of_one_element" =
  pp pp_pat (prs_pat_tuple prs_pat) "1";
  [%expect {| Syntax error |}]
;;

let%expect_test "parse_empty_list" =
  pp pp_pat (prs_pat_list prs_pat) "[]";
  [%expect {|
    (PatList []) |}]
;;

(* ================================ expressions ================================ *)

let%expect_test "expr_cons_int" =
  pp pp_expr prs_expr_const "1";
  [%expect {|
    (Const (Int 1)) |}]
;;

let%expect_test "expr_cons_str" =
  pp pp_expr prs_expr_const "  {|meow|}  ";
  [%expect {|
    (Const (Str "meow")) |}]
;;

let%expect_test "expr_cons_unit" =
  pp pp_expr prs_expr_const "  ()  ";
  [%expect {|
    (Const Unit) |}]
;;

let%expect_test "simple_list" =
  pp pp_expr (prs_expr_list prs_expr) "   [ 1 ; 2\n; 3]   ";
  [%expect {| (List [(Const (Int 1)); (Const (Int 2)); (Const (Int 3))]) |}]
;;

let%expect_test "complex_list" =
  pp pp_expr (prs_expr_list prs_expr) "   [ [1;2;3] ; 2\n; 3]   ";
  [%expect
    {|
    (List
       [(List [(Const (Int 1)); (Const (Int 2)); (Const (Int 3))]);
         (Const (Int 2)); (Const (Int 3))]) |}]
;;

let%expect_test "empty_list" =
  pp pp_expr (prs_expr_list prs_expr) "[]";
  [%expect {| (List []) |}]
;;

let%expect_test "incorrect_tuple_one_el" =
  pp pp_expr (prs_expr_tuple prs_expr) "(1)";
  [%expect {| Syntax error |}]
;;

let%expect_test "simple_fun" =
  pp pp_expr (prs_expr_fun prs_pat prs_expr) "fun x -> x";
  [%expect {| (Fun ((PatVar "x"), (Var "x"))) |}]
;;

let%expect_test "fun_two_var" =
  pp pp_expr (prs_expr_fun prs_pat prs_expr) "fun x y -> x + y";
  [%expect
    {|
    (Fun ((PatVar "x"), (Fun ((PatVar "y"), (BinOp (Add, (Var "x"), (Var "y")))))
       )) |}]
;;

let%expect_test "fun_of_fun" =
  pp pp_expr (prs_expr_fun prs_pat prs_expr) "fun x -> fun _ -> x";
  [%expect {| (Fun ((PatVar "x"), (Fun (PatAny, (Var "x"))))) |}]
;;

let%expect_test "simple_let" =
  pp pp_expr (prs_expr_let prs_expr) "let x = 1";
  [%expect
    {|
    (Let (NonRec, { pat = (PatVar "x"); expr = (Const (Int 1)) }, [],
       (Const Unit))) |}]
;;

let%expect_test "let_two_var" =
  pp pp_expr (prs_expr_let prs_expr) "let x y = x + y";
  [%expect
    {|
    (Let (NonRec,
       { pat = (PatVar "x");
         expr = (Fun ((PatVar "y"), (BinOp (Add, (Var "x"), (Var "y"))))) },
       [], (Const Unit))) |}]
;;

let%expect_test "let_rec" =
  pp pp_expr (prs_expr_let prs_expr) "let rec x = 1";
  [%expect
    {|
    (Let (Rec, { pat = (PatVar "x"); expr = (Const (Int 1)) }, [], (Const Unit))) |}]
;;

let%expect_test "let_unit" =
  pp pp_expr (prs_expr_let prs_expr) "let _ = ()";
  [%expect
    {|
    (Let (NonRec, { pat = PatAny; expr = (Const Unit) }, [], (Const Unit))) |}]
;;

let%expect_test "let_with_in" =
  pp pp_expr (prs_expr_let prs_expr) "let meow = 5 in meow + 1";
  [%expect
    {|
    (Let (NonRec, { pat = (PatVar "meow"); expr = (Const (Int 5)) }, [],
       (BinOp (Add, (Var "meow"), (Const (Int 1)))))) |}]
;;

let%expect_test "simple_app" =
  pp pp_expr (prs_expr_app prs_expr) "fact 1";
  [%expect {|
    (App ((Var "fact"), (Const (Int 1)))) |}]
;;

let%expect_test "app_two_par" =
  pp pp_expr (prs_expr_app prs_expr) "foo 1 2";
  [%expect {|
    (App ((App ((Var "foo"), (Const (Int 1)))), (Const (Int 2)))) |}]
;;

let%expect_test "app_in_app" =
  pp pp_expr (prs_expr_app prs_expr) "foo (g 1) 2";
  [%expect
    {|
    (App ((App ((Var "foo"), (App ((Var "g"), (Const (Int 1)))))),
       (Const (Int 2)))) |}]
;;

let%expect_test "app_in" =
  pp pp_expr (prs_expr_app prs_expr) "foo -1";
  [%expect {|
    (App ((Var "foo"), (BinOp (Sub, (Const (Int 0)), (Const (Int 1)))))) |}]
;;

let%expect_test "simple_branch" =
  pp pp_expr (prs_expr_branch prs_expr) "if x = 5 then 7 else 6";
  [%expect
    {|
    (Branch ((BinOp (Eq, (Var "x"), (Const (Int 5)))), (Const (Int 7)),
       (Const (Int 6)))) |}]
;;

let%expect_test "branch_in_branch" =
  pp pp_expr (prs_expr_branch prs_expr) "if x = 5 then (if x = 7 then 7) else 4";
  [%expect
    {|
    (Branch ((BinOp (Eq, (Var "x"), (Const (Int 5)))),
       (Branch ((BinOp (Eq, (Var "x"), (Const (Int 7)))), (Const (Int 7)),
          (Const Unit))),
       (Const (Int 4)))) |}]
;;

(* let%expect_test "simple_match" =
   pp pp_expr (prs_expr_match prs_pat prs_expr) "match x with | _ -> x + 5 | c -> meow";
   [%expect
    {|
      (Match ((Var "x"),
         { match_pat = PatAny;
           match_expr = (BinOp (Add, (Var "x"), (Const (Int 5)))) },
         [{ match_pat = (PatVar "c"); match_expr = (Var "meow") }])) |}]
   ;; *)

let%expect_test "factorial" =
  pp pp_expr prs_expr "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)";
  [%expect
    {|
    (Let (Rec,
       { pat = (PatVar "fact");
         expr =
         (Fun ((PatVar "n"),
            (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
               (BinOp (Mul, (Var "n"),
                  (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                  ))
               ))
            ))
         },
       [], (Const Unit))) |}]
;;

let%expect_test "prs_some" =
  pp pp_expr (prs_option prs_expr) "Some (n - 1) ";
  [%expect {|
    (Option (Some (BinOp (Sub, (Var "n"), (Const (Int 1)))))) |}]
;;

let%expect_test "prs_none" =
  pp pp_expr (prs_option prs_expr) "None ";
  [%expect {|
    (Option None) |}]
;;

let%expect_test "prs_expr_binop_with_some" =
  pp pp_expr prs_expr "Some n - 1 ";
  [%expect {|
    (BinOp (Sub, (Option (Some (Var "n"))), (Const (Int 1)))) |}]
;;

let%expect_test "prs_expr_some" =
  pp pp_expr prs_expr "Some  (n - 1) ";
  [%expect {|
    (Option (Some (BinOp (Sub, (Var "n"), (Const (Int 1)))))) |}]
;;
