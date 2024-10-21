(** Copyright 2024-2025, Sultanov Muhammet and Kudrya Alexander *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_oop_lib
open Parser

let parse_test s =
  match parse_prefix s with
  | Ok v -> Format.printf "%s\n" Ast.(show_structure v)
  | Error error -> Format.printf "%s\n" error
;;

(* basic operators tests*)

let%expect_test "basic_test" =
  let () = parse_test "1 + 2 / 3;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_binary ((Exp_constant (Int 1)), Add,
           (Exp_binary ((Exp_constant (Int 2)), Div, (Exp_constant (Int 3)))))))
      ]
    |}]
;;

(* if-then_else and match tests*)

let%expect_test "test_ite" =
  let () = parse_test {| 
    let x = if a > b then c else d in
   |} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrec; d_pat = (Pat_var "x");
          d_exp =
          (Exp_ifthenelse (
             (Exp_binary ((Exp_ident "a"), GreaterThan, (Exp_ident "b"))),
             (Exp_ident "c"), (Some (Exp_ident "d"))))
          })
      ]
     |}]
;;

let%expect_test "test_it" =
  let () = parse_test {| 
    let x = if a > b then c in
   |} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrec; d_pat = (Pat_var "x");
          d_exp =
          (Exp_ifthenelse (
             (Exp_binary ((Exp_ident "a"), GreaterThan, (Exp_ident "b"))),
             (Exp_ident "c"), None))
          })
      ]
    |}]
;;

let%expect_test "test_match" =
  let () =
    parse_test
      {| 
    let n = 
      match a with 
      | 1 -> a
      | 2 -> 2 
      | _ -> 3 
    ;;
   |}
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrec; d_pat = (Pat_var "n");
          d_exp =
          (Exp_match ((Exp_ident "a"),
             [((Pat_constant (Int 1)), (Exp_ident "a"));
               ((Pat_constant (Int 2)), (Exp_constant (Int 2)));
               (Pat_any, (Exp_constant (Int 3)))]
             ))
          })
      ]
    |}]
;;

(* let tests *)

let%expect_test "test_with_let_function" =
  let () = parse_test {|let f a = fun x -> x + a|} in
  [%expect
    {| 
    [(Str_value
        { d_rec = Nonrec; d_pat = (Pat_var "f");
          d_exp =
          (Exp_function ([(Pat_var "a")],
             (Exp_function ([(Pat_var "x")],
                (Exp_binary ((Exp_ident "x"), Add, (Exp_ident "a")))))
             ))
          })
      ] 
    |}]
;;

let%expect_test "test_with_let_types" =
  let () = parse_test {| 
    let a = 15;;
    let b = "Help";;
    let c = [];;
   |} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrec; d_pat = (Pat_var "a"); d_exp = (Exp_constant (Int 15))
          })
      ] 
    |}]
;;

(* big tests *)
let%expect_test "test_with_let_types" =
  let () =
    parse_test
      {| 
    let fib a b n = 
      if n < 2 then n else fib b (b + a) (n - 1) 
   |}
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrec; d_pat = (Pat_var "fib");
          d_exp =
          (Exp_function ([(Pat_var "a")],
             (Exp_function ([(Pat_var "b")],
                (Exp_function ([(Pat_var "n")],
                   (Exp_ifthenelse (
                      (Exp_binary ((Exp_ident "n"), LessThan,
                         (Exp_constant (Int 2)))),
                      (Exp_ident "n"),
                      (Some (Exp_apply (
                               (Exp_apply (
                                  (Exp_apply ((Exp_ident "fib"),
                                     [(Exp_ident "b")])),
                                  [(Exp_binary ((Exp_ident "b"), Add,
                                      (Exp_ident "a")))
                                    ]
                                  )),
                               [(Exp_binary ((Exp_ident "n"), Subt,
                                   (Exp_constant (Int 1))))
                                 ]
                               )))
                      ))
                   ))
                ))
             ))
          })
      ]
    |}]
;;
