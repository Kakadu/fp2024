(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Parser
open Pr_printer

let parse str =
  match parse_expr str with
  | Ok structure ->
    Stdlib.print_endline (Stdlib.Format.asprintf "%a" prpr_structure structure);
    Stdlib.print_endline (show_structure structure)
  | Error _ -> Stdlib.print_endline "Parsing failed"
;;
(*
let%expect_test _ =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
    let rec factorial = fun n -> if (n = 0) then 1 else (n * factorial (n - 1)) in factorial 5

    [(SValue (Recursive, "factorial",
        (Efun ([(PVar "n")],
           (Eif_then_else ((Ebin_op (Eq, (Evar "n"), (Econst (Int 0)))),
              (Econst (Int 1)),
              (Some (Ebin_op (Mult, (Evar "n"),
                       (Efun_application ((Evar "factorial"),
                          (Ebin_op (Sub, (Evar "n"), (Econst (Int 1))))))
                       )))
              ))
           )),
        (Efun_application ((Evar "factorial"), (Econst (Int 5))))))
      ]
    |}]
;;

let%expect_test _ =
  parse "[]";
  [%expect {|
    [];

    [(SEval (Elist []))]    
    |}]
;;

let%expect_test _ =
  parse "if 5 > 6 then let x = 5";
  [%expect {|
    [];

    [(SEval (Elist []))]    
    |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 6 in (x < y) and (x == y)";
  [%expect {|
    [];

    [(SEval (Elist []))]    
    |}]
;;



let%expect_test _ =
  parse "[1; [2; 3]; 4]";
  [%expect
    {|
  [1; [2; 3]; 4];

  [(SEval
      (Elist
         [(Econst (Int 1)); (Elist [(Econst (Int 2)); (Econst (Int 3))]);
           (Econst (Int 4))]))
    ]
  |}]
;;

let%expect_test _ =
  parse "[1 + 2; 3 * 4; 5 - 6]";
  [%expect
    {|
  [(1 + 2); (3 * 4); (5 - 6)];

  [(SEval
      (Elist
         [(Ebin_op (Add, (Econst (Int 1)), (Econst (Int 2))));
           (Ebin_op (Mult, (Econst (Int 3)), (Econst (Int 4))));
           (Ebin_op (Sub, (Econst (Int 5)), (Econst (Int 6))))]))
    ]
  |}]
;;

let%expect_test _ =
  parse "let a = (1, 2, 3)";
  [%expect
    {|
    let  a = (1, 2, 3) in ()

    [(SValue (Non_recursive, "a",
        (Etuple ((Econst (Int 1)), (Econst (Int 2)), [(Econst (Int 3))])),
        (Econst Unit)))
      ]
  |}]
;;

let%expect_test _ =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {|
    ((1234 + 676) - (9002 * (52 / 2)));

    [(SEval
        (Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
           (Ebin_op (Mult, (Econst (Int 9002)),
              (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
           )))
      ]
    |}]
;;

let%expect_test _ =
  parse "if 1234 + 1 = 1235 then let x = 4";
  [%expect
    {|
    if ((1234 + 1) = 1235) then let  x = 4 in ();

    [(SEval
        (Eif_then_else (
           (Ebin_op (Eq, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 1)))),
              (Econst (Int 1235)))),
           (Elet (Non_recursive, "x", (Econst (Int 4)), (Econst Unit))), None)))
      ]
    |}]
;;

let%expect_test _ =
  parse "let rec 5 = ()";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "39482309482390842309482438208 + 2";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 3 in let n = x + y;; if 13 > 12 then let a = 2";
  [%expect
    {|
    let  x = 5 in let  y = 3 in let  n = (x + y) in ()
    if (13 > 12) then let  a = 2 in ();

    [(SValue (Non_recursive, "x", (Econst (Int 5)),
        (Elet (Non_recursive, "y", (Econst (Int 3)),
           (Elet (Non_recursive, "n", (Ebin_op (Add, (Evar "x"), (Evar "y"))),
              (Econst Unit)))
           ))
        ));
      (SEval
         (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
            (Elet (Non_recursive, "a", (Econst (Int 2)), (Econst Unit))), None)))
      ]
  |}]
;;

let%expect_test _ =
  parse "let x = 5 ;; if 13 > 12 then let a = 2";
  [%expect
    {|
    let  x = 5 in ()
    if (13 > 12) then let  a = 2 in ();

    [(SValue (Non_recursive, "x", (Econst (Int 5)), (Econst Unit)));
      (SEval
         (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
            (Elet (Non_recursive, "a", (Econst (Int 2)), (Econst Unit))), None)))
      ]
  |}]
;;

*)