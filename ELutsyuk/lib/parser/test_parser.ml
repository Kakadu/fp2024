(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Expressions
open Auxiliaries
open Constants
open Patterns

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"
;;

(* auxiliaries *)

let%expect_test "normal_id" =
  pp Format.pp_print_string parse_id "id";
  [%expect {| id |}]
;;

let%expect_test "keyword_id" =
  pp Format.pp_print_string parse_id "while";
  [%expect {| Syntax error |}]
;;

(* ================================= constants ================================= *)

let%expect_test "num_with_plus" =
  pp pp_constant parse_int "+2024";
  [%expect {| (Int 2024) |}]
;;

let%expect_test "num_with_minus" =
  pp pp_constant parse_int "-2024";
  [%expect {| (Int -2024) |}]
;;

let%expect_test "num_without_sign_before" =
  pp pp_constant parse_int "2024\n";
  [%expect {| (Int 2024) |}]
;;

let%expect_test "two_signs_before_number" =
  pp pp_constant parse_int "+-2024";
  [%expect {| Syntax error |}]
;;

let%expect_test "string_with_//" =
  pp pp_constant parse_str "{|str//|}";
  [%expect {| (Str "str//") |}]
;;

let%expect_test "incorrect_string" =
  pp pp_constant parse_str "str//";
  [%expect {| Syntax error |}]
;;

let%expect_test "empty_string" =
  pp pp_constant parse_str "{||}";
  [%expect {| (Str "") |}]
;;

let%expect_test "one_space_string" =
  pp pp_constant parse_str "{| |}";
  [%expect {| (Str " ") |}]
;;

let%expect_test "true" =
  pp pp_constant parse_bool "true";
  [%expect {| (Bool true) |}]
;;

let%expect_test "false" =
  pp pp_constant parse_bool "false";
  [%expect {| (Bool false) |}]
;;

let%expect_test "incorrect_bool_with_char_after" =
  pp pp_constant parse_bool "truee";
  [%expect {| Syntax error |}]
;;

let%expect_test "'\n'_after_bool" =
  pp pp_constant parse_bool "true\n";
  [%expect {| (Bool true) |}]
;;

let%expect_test "unit" =
  pp pp_constant parse_unit " ()\n";
  [%expect {| Unit |}]
;;

(* patterns *)
let%expect_test "pat_var" =
  pp pp_pattern parse_pat_var "  meow\n";
  [%expect {| (PVar "meow") |}]
;;

let%expect_test "pat_cons" =
  pp pp_pattern parse_pat_cons "  \r{|meow|}\n";
  [%expect {| (PCons (Str "meow")) |}]
;;

let%expect_test "pat_empty_str" =
  pp pp_pattern parse_pat_cons "  \r{||}\n";
  [%expect {| (PCons (Str "")) |}]
;;

let%expect_test "pat_any" =
  pp pp_pattern parse_pat_any "  \r_\n";
  [%expect {| PAny |}]
;;

let%expect_test "pat_simple_tuple" =
  pp pp_pattern (parse_pat_tuple parse_pat) "(1,2,3)";
  [%expect {| (PTuple ((PCons (Int 1)), (PCons (Int 2)), [(PCons (Int 3))])) |}]
;;

let%expect_test "pat_tuple_of_tuple" =
  pp pp_pattern (parse_pat_tuple parse_pat) "(1,2,(3, 4, 5))";
  [%expect
    {|
    (PTuple ((PCons (Int 1)), (PCons (Int 2)),
       [(PTuple ((PCons (Int 3)), (PCons (Int 4)), [(PCons (Int 5))]))])) |}]
;;

let%expect_test "incorrect_pat_tuple_of_one_element" =
  pp pp_pattern (parse_pat_tuple parse_pat) "(1)";
  [%expect {|
    Syntax error |}]
;;

let%expect_test "pat_x" =
  pp pp_pattern parse_pat "  x ";
  [%expect {| (PVar "x") |}]
;;

let%expect_test "mixed_pat" =
  pp pp_pattern parse_pat "  \r(\r(  (_,\r_), _)\n, _)\n\n";
  [%expect {|
    (PTuple ((PTuple ((PTuple (PAny, PAny, [])), PAny, [])), PAny, [])) |}]
;;

(* expressions *)
let%expect_test "expr_var" =
  pp pp_expression parse_expr_var "  meow\n   ";
  [%expect {|
    (Var "meow") |}]
;;

let%expect_test "expr_cons_int" =
  pp pp_expression parse_expr_cons "1";
  [%expect {|
    (Cons (Int 1)) |}]
;;

let%expect_test "expr_cons_str" =
  pp pp_expression parse_expr_cons "  {|meow|}  ";
  [%expect {|
    (Cons (Str "meow")) |}]
;;

let%expect_test "expr_cons_unit" =
  pp pp_expression parse_expr_cons "  ()  ";
  [%expect {|
    (Cons Unit) |}]
;;

(* let%expect_test "parse_bin_op" =
   pp pp_expression (chainl1 parse_mul "6 * 9") "6 * 9";
   [%expect {| |}]
   ;; *)

let%expect_test "simple_list" =
  pp pp_expression (parse_expr_list parse_expr) "   [ 1 ; 2\n; 3]   ";
  [%expect {| (List [(Cons (Int 1)); (Cons (Int 2)); (Cons (Int 3))]) |}]
;;

let%expect_test "complex_list" =
  pp pp_expression (parse_expr_list parse_expr) "   [ [1;2;3] ; 2\n; 3]   ";
  [%expect
    {|
    (List
       [(List [(Cons (Int 1)); (Cons (Int 2)); (Cons (Int 3))]); (Cons (Int 2));
         (Cons (Int 3))]) |}]
;;

let%expect_test "empty_list" =
  pp pp_expression (parse_expr_list parse_expr) "[]";
  [%expect {| (List []) |}]
;;

let%expect_test "incorrect_list_diff_types" =
  pp pp_expression (parse_expr_list parse_expr) "[1, {|meow|}]";
  [%expect {| Syntax error |}]
;;

let%expect_test "simple_tuple" =
  pp pp_expression (parse_expr_tuple parse_expr) "(1, 2, 3)";
  [%expect {| (Tuple ((Cons (Int 1)), (Cons (Int 2)), [(Cons (Int 3))])) |}]
;;

let%expect_test "incorrect_tuple_one_el" =
  pp pp_expression (parse_expr_tuple parse_expr) "(1)";
  [%expect {| Syntax error |}]
;;

let%expect_test "complex_tuple" =
  pp pp_expression (parse_expr_tuple parse_expr) "((1, 1, 1), (2, 2, 2), {|meow|})";
  [%expect
    {|
    (Tuple ((Tuple ((Cons (Int 1)), (Cons (Int 1)), [(Cons (Int 1))])),
       (Tuple ((Cons (Int 2)), (Cons (Int 2)), [(Cons (Int 2))])),
       [(Cons (Str "meow"))])) |}]
;;

let%expect_test "simple_fun" =
  pp pp_expression (parse_expr_fun parse_pat parse_expr) "fun x -> x";
  [%expect {| (Fun ((PVar "x"), (Var "x"))) |}]
;;

let%expect_test "fun_two_var" =
  pp pp_expression (parse_expr_fun parse_pat parse_expr) "fun x y -> x + y";
  [%expect
    {|
    (Fun ((PVar "x"), (Fun ((PVar "y"), (BinaryOp (Add, (Var "x"), (Var "y")))))
       )) |}]
;;

let%expect_test "fun_of_fun" =
  pp pp_expression (parse_expr_fun parse_pat parse_expr) "fun x -> fun _ -> x";
  [%expect {| (Fun ((PVar "x"), (Fun (PAny, (Var "x"))))) |}]
;;

let%expect_test "simple_let" =
  pp pp_expression (parse_expr_let parse_pat parse_expr) "let x = 1";
  [%expect
    {|
    (Let ({ is_rec = NonRec; pat = (PVar "x"); expr = (Cons (Int 1)) },
       (Cons Unit))) |}]
;;

let%expect_test "let_two_var" =
  pp pp_expression (parse_expr_let parse_pat parse_expr) "let x y = x + y";
  [%expect
    {|
    (Let (
       { is_rec = NonRec; pat = (PVar "x");
         expr = (Fun ((PVar "y"), (BinaryOp (Add, (Var "x"), (Var "y"))))) },
       (Cons Unit))) |}]
;;

let%expect_test "let_rec" =
  pp pp_expression (parse_expr_let parse_pat parse_expr) "let rec x = 1";
  [%expect
    {|
    (Let ({ is_rec = Rec; pat = (PVar "x"); expr = (Cons (Int 1)) }, (Cons Unit)
       )) |}]
;;

let%expect_test "let_unit" =
  pp pp_expression (parse_expr_let parse_pat parse_expr) "let _ = ()";
  [%expect
    {|
    (Let ({ is_rec = NonRec; pat = PAny; expr = (Cons Unit) }, (Cons Unit))) |}]
;;

let%expect_test "let_with_in" =
  pp pp_expression (parse_expr_let parse_pat parse_expr) "let meow = 5 in meow + 1";
  [%expect
    {|
    (Let ({ is_rec = NonRec; pat = (PVar "meow"); expr = (Cons (Int 5)) },
       (App ((Var "meow"), (Cons (Int 1)))))) |}]
;;

let%expect_test "simple_app" =
  pp pp_expression (parse_expr_app parse_expr) "fact 1";
  [%expect {|
    (App ((Var "fact"), (Cons (Int 1)))) |}]
;;

let%expect_test "app_two_par" =
  pp pp_expression (parse_expr_app parse_expr) "foo 1 2";
  [%expect {|
    (App ((App ((Var "foo"), (Cons (Int 1)))), (Cons (Int 2)))) |}]
;;

let%expect_test "app_in_app" =
  pp pp_expression (parse_expr_app parse_expr) "foo (g 1) 2";
  [%expect
    {|
    (App ((App ((Var "foo"), (App ((Var "g"), (Cons (Int 1)))))), (Cons (Int 2))
       )) |}]
;;

let%expect_test "simple_branch" =
  pp pp_expression (parse_expr_branch parse_expr) "if x = 5 then 7 else 6";
  [%expect
    {|
    (Branch ((BinaryOp (Eq, (Var "x"), (Cons (Int 5)))), (Cons (Int 7)),
       (Cons (Int 6)))) |}]
;;

let%expect_test "branch_in_branch" =
  pp pp_expression (parse_expr_branch parse_expr) "if x = 5 then (if x = 7 then 7) else 4";
  [%expect
    {|
    (Branch ((BinaryOp (Eq, (Var "x"), (Cons (Int 5)))),
       (Branch ((BinaryOp (Eq, (Var "x"), (Cons (Int 7)))), (Cons (Int 7)),
          (Cons Unit))),
       (Cons (Int 4)))) |}]
;;

let%expect_test "simple_match" =
  pp
    pp_expression
    (parse_expr_match parse_expr parse_pat)
    "match x with | _ -> x + 5 | c -> meow";
  [%expect {|
    Syntax error |}]
;;

(* let%expect_test "factorial" =
   pp pp_expression parse_expr "let rec fact n = if n <= 1 then 1 * 5 else fact 1";
   [%expect
    {|
    (Let (
       { is_rec = Rec; pat = (PVar "fact");
         expr =
         (Fun ((PVar "n"),
            (Branch ((BinaryOp (LtEq, (Var "n"), (Cons (Int 1)))),
               (BinaryOp (Mult, (Cons (Int 1)), (Cons (Int 5)))),
               (BinaryOp (Mult, (Cons (Int 4)), (Cons (Int 5))))))
            ))
         },
       (Cons Unit))) |}]
   ;; *)

let%expect_test "factorial" =
  pp pp_expression parse_expr "let rec fact n = if n <= 1 then 1 else n * fact (n-1)";
  [%expect
    {|
    (Let (
       { is_rec = Rec; pat = (PVar "fact");
         expr =
         (Fun ((PVar "n"),
            (Branch ((BinaryOp (LtEq, (Var "n"), (Cons (Int 1)))),
               (Cons (Int 1)),
               (BinaryOp (Mult, (Var "n"),
                  (App ((Var "fact"), (App ((Var "n"), (Cons (Int -1))))))))
               ))
            ))
         },
       (Cons Unit))) |}]
;;
