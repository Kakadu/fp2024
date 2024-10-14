(** Copyright 2024-2025, Sultanov Muhammet and Kudrya Alexander *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_oop_lib
open Parser

let parse_test s =
  match parse_prefix s with
  | Ok v -> Format.printf "%s\n" Ast.(show_structure v)
  | Error error -> Format.printf "%s\n" error
;;

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

let%expect_test "test_with_let" =
  let () = parse_test {|let f a = fun x -> x + a|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "f");
          d_expr =
          (Exp_function ((Pat_var "a"),
             (Exp_function ((Pat_var "x"),
                (Exp_bin_op (Plus, (Exp_ident "x"), (Exp_ident "a")))))
             ))
          })
      ] |}]
;;
