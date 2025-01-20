(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Aux
open Pat
open Expr

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"
;;

(* ================================ auxiliaries ================================ *)

let%expect_test "normal_id" =
  pp Format.pp_print_string pid "id";
  [%expect {| id |}]
;;

let%expect_test "keyword_id" =
  pp Format.pp_print_string pid "while";
  [%expect {| Syntax error |}]
;;

(* ================================= constants ================================= *)

let%expect_test "num_with_plus" =
  pp pp_const pint "+2024";
  [%expect {| Syntax error |}]
;;

let%expect_test "num_with_minus" =
  pp pp_const pint "-2024";
  [%expect {| Syntax error |}]
;;

let%expect_test "num_without_sign_before" =
  pp pp_const pint "2024\n";
  [%expect {| (Int 2024) |}]
;;

let%expect_test "two_signs_before_number" =
  pp pp_const pint "+-2024";
  [%expect {| Syntax error |}]
;;

let%expect_test "string_with_//" =
  pp pp_const pstr "{|str//|}";
  [%expect {| (Str "str//") |}]
;;

let%expect_test "incorrect_string" =
  pp pp_const pstr "str//";
  [%expect {| Syntax error |}]
;;

let%expect_test "empty_string" =
  pp pp_const pstr "{||}";
  [%expect {| (Str "") |}]
;;

let%expect_test "one_space_string" =
  pp pp_const pstr "{| |}";
  [%expect {| (Str " ") |}]
;;

let%expect_test "true" =
  pp pp_const pbool "true";
  [%expect {| (Bool true) |}]
;;

let%expect_test "false" =
  pp pp_const pbool "false";
  [%expect {| (Bool false) |}]
;;

let%expect_test "incorrect_bool_with_char_after" =
  pp pp_const pbool "truee";
  [%expect {| Syntax error |}]
;;

let%expect_test "'\n'_after_bool" =
  pp pp_const pbool "true\n";
  [%expect {| (Bool true) |}]
;;

let%expect_test "unit" =
  pp pp_const punit " ()\n";
  [%expect {| Unit |}]
;;

(* ================================== patterns ================================= *)
let%expect_test "pat_var" =
  pp pp_pat ppat_var "  meow\n";
  [%expect {| (PatVar "meow") |}]
;;

let%expect_test "pat_cons" =
  pp pp_pat ppat_const "  \r{|meow|}\n";
  [%expect {| (PatConst (Str "meow")) |}]
;;

let%expect_test "pat_empty_str" =
  pp pp_pat ppat_const "  \r{||}\n";
  [%expect {| (PatConst (Str "")) |}]
;;

let%expect_test "pat_any" =
  pp pp_pat ppat_any "  \r_\n";
  [%expect {| PatAny |}]
;;

let%expect_test "pat_simple_tuple" =
  pp pp_pat (ppat_tuple ppat) "(1,2,3)";
  [%expect {| (PatTup ((PatConst (Int 1)), (PatConst (Int 2)), [(PatConst (Int 3))])) |}]
;;

let%expect_test "pat_tuple_of_tuple" =
  pp pp_pat (ppat_tuple ppat) "(1,2,(3, 4, 5))";
  [%expect
    {|
    (PatTup ((PatConst (Int 1)), (PatConst (Int 2)),
       [(PatTup ((PatConst (Int 3)), (PatConst (Int 4)), [(PatConst (Int 5))]))]
       )) |}]
;;

let%expect_test "incorrect_pat_tuple_of_one_element" =
  pp pp_pat (ppat_tuple ppat) "(1)";
  [%expect {|
    Syntax error |}]
;;

let%expect_test "pat_x" =
  pp pp_pat ppat "  x ";
  [%expect {| (PatVar "x") |}]
;;

let%expect_test "mixed_pat" =
  pp pp_pat ppat "  \r(\r(  (_,\r_), _)\n, _)\n\n";
  [%expect
    {|
    (PatTup ((PatTup ((PatTup (PatAny, PatAny, [])), PatAny, [])), PatAny, [])) |}]
;;

(* ================================ expressions ================================ *)
let%expect_test "expr_var" =
  pp pp_expr pexpr_var "  meow\n   ";
  [%expect {|
    (Var "meow") |}]
;;

let%expect_test "expr_cons_int" =
  pp pp_expr pexpr_const "1";
  [%expect {|
    (Const (Int 1)) |}]
;;

let%expect_test "expr_cons_str" =
  pp pp_expr pexpr_const "  {|meow|}  ";
  [%expect {|
    (Const (Str "meow")) |}]
;;

let%expect_test "expr_cons_unit" =
  pp pp_expr pexpr_const "  ()  ";
  [%expect {|
    (Const Unit) |}]
;;

let%expect_test "num_with_plus" =
  pp pp_expr (pexpr_unary pexpr) "+2024";
  [%expect {| (Const (Int 2024)) |}]
;;

let%expect_test "num_with_minus" =
  pp pp_expr (pexpr_unary pexpr) "-2024";
  [%expect {| (BinOp (Sub, (Const (Int 0)), (Const (Int 2024)))) |}]
;;

let%expect_test "simple_list" =
  pp pp_expr (pexpr_list pexpr) "   [ 1 ; 2\n; 3]   ";
  [%expect {| (List [(Const (Int 1)); (Const (Int 2)); (Const (Int 3))]) |}]
;;

let%expect_test "complex_list" =
  pp pp_expr (pexpr_list pexpr) "   [ [1;2;3] ; 2\n; 3]   ";
  [%expect
    {|
    (List
       [(List [(Const (Int 1)); (Const (Int 2)); (Const (Int 3))]);
         (Const (Int 2)); (Const (Int 3))]) |}]
;;

let%expect_test "empty_list" =
  pp pp_expr (pexpr_list pexpr) "[]";
  [%expect {| (List []) |}]
;;

let%expect_test "incorrect_list_diff_types" =
  pp pp_expr (pexpr_list pexpr) "[1, {|meow|}]";
  [%expect {| Syntax error |}]
;;

let%expect_test "simple_tuple" =
  pp pp_expr (pexpr_tuple pexpr) "(1, 2, 3)";
  [%expect {| (Tup ((Const (Int 1)), (Const (Int 2)), [(Const (Int 3))])) |}]
;;

let%expect_test "incorrect_tuple_one_el" =
  pp pp_expr (pexpr_tuple pexpr) "(1)";
  [%expect {| Syntax error |}]
;;

let%expect_test "complex_tuple" =
  pp pp_expr (pexpr_tuple pexpr) "((1, 1, 1), (2, 2, 2), {|meow|})";
  [%expect
    {|
    (Tup ((Tup ((Const (Int 1)), (Const (Int 1)), [(Const (Int 1))])),
       (Tup ((Const (Int 2)), (Const (Int 2)), [(Const (Int 2))])),
       [(Const (Str "meow"))])) |}]
;;

let%expect_test "simple_fun" =
  pp pp_expr (pexpr_fun ppat pexpr) "fun x -> x";
  [%expect {| (Fun ((PatVar "x"), (Var "x"))) |}]
;;

let%expect_test "fun_two_var" =
  pp pp_expr (pexpr_fun ppat pexpr) "fun x y -> x + y";
  [%expect
    {|
    (Fun ((PatVar "x"), (Fun ((PatVar "y"), (BinOp (Add, (Var "x"), (Var "y")))))
       )) |}]
;;

let%expect_test "fun_of_fun" =
  pp pp_expr (pexpr_fun ppat pexpr) "fun x -> fun _ -> x";
  [%expect {| (Fun ((PatVar "x"), (Fun (PatAny, (Var "x"))))) |}]
;;

let%expect_test "simple_let" =
  pp pp_expr (pexpr_let pexpr) "let x = 1";
  [%expect
    {|
    (Let ({ is_rec = NonRec; pat = (PatVar "x"); expr = (Const (Int 1)) },
       (Const Unit))) |}]
;;

let%expect_test "let_two_var" =
  pp pp_expr (pexpr_let pexpr) "let x y = x + y";
  [%expect
    {|
    (Let (
       { is_rec = NonRec; pat = (PatVar "x");
         expr = (Fun ((PatVar "y"), (BinOp (Add, (Var "x"), (Var "y"))))) },
       (Const Unit))) |}]
;;

let%expect_test "let_rec" =
  pp pp_expr (pexpr_let pexpr) "let rec x = 1";
  [%expect
    {|
    (Let ({ is_rec = Rec; pat = (PatVar "x"); expr = (Const (Int 1)) },
       (Const Unit))) |}]
;;

let%expect_test "let_unit" =
  pp pp_expr (pexpr_let pexpr) "let _ = ()";
  [%expect
    {|
    (Let ({ is_rec = NonRec; pat = PatAny; expr = (Const Unit) }, (Const Unit))) |}]
;;

let%expect_test "let_with_in" =
  pp pp_expr (pexpr_let pexpr) "let meow = 5 in meow + 1";
  [%expect
    {|
    (Let ({ is_rec = NonRec; pat = (PatVar "meow"); expr = (Const (Int 5)) },
       (BinOp (Add, (Var "meow"), (Const (Int 1)))))) |}]
;;

let%expect_test "simple_app" =
  pp pp_expr (pexpr_app pexpr) "fact 1";
  [%expect {|
    (App ((Var "fact"), (Const (Int 1)))) |}]
;;

let%expect_test "app_two_par" =
  pp pp_expr (pexpr_app pexpr) "foo 1 2";
  [%expect {|
    (App ((App ((Var "foo"), (Const (Int 1)))), (Const (Int 2)))) |}]
;;

let%expect_test "app_in_app" =
  pp pp_expr (pexpr_app pexpr) "foo (g 1) 2";
  [%expect
    {|
    (App ((App ((Var "foo"), (App ((Var "g"), (Const (Int 1)))))),
       (Const (Int 2)))) |}]
;;

let%expect_test "simple_branch" =
  pp pp_expr (pexpr_branch pexpr) "if x = 5 then 7 else 6";
  [%expect
    {|
    (Branch ((BinOp (Eq, (Var "x"), (Const (Int 5)))), (Const (Int 7)),
       (Const (Int 6)))) |}]
;;

let%expect_test "branch_in_branch" =
  pp pp_expr (pexpr_branch pexpr) "if x = 5 then (if x = 7 then 7) else 4";
  [%expect
    {|
    (Branch ((BinOp (Eq, (Var "x"), (Const (Int 5)))),
       (Branch ((BinOp (Eq, (Var "x"), (Const (Int 7)))), (Const (Int 7)),
          (Const Unit))),
       (Const (Int 4)))) |}]
;;

let%expect_test "simple_match" =
  pp pp_expr (pexpr_match ppat pexpr) "match x with | _ -> x + 5 | c -> meow";
  [%expect
    {|
      (Match ((Var "x"),
         { match_pat = PatAny;
           match_expr = (BinOp (Add, (Var "x"), (Const (Int 5)))) },
         [{ match_pat = (PatVar "c"); match_expr = (Var "meow") }])) |}]
;;

let%expect_test "factorial" =
  pp pp_expr pexpr "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)";
  [%expect
    {|
    (Let (
       { is_rec = Rec; pat = (PatVar "fact");
         expr =
         (Fun ((PatVar "n"),
            (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
               (BinOp (Mul, (Var "n"),
                  (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                  ))
               ))
            ))
         },
       (Const Unit))) |}]
;;
