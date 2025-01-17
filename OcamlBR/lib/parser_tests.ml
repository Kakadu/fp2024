(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parser
open Ast

let parse str =
  match parse_expr str with
  | Ok ast -> Stdlib.print_endline (show_structure ast)
  | _ -> Stdlib.print_endline "Parsing failed"
;;


(*factorial*)
let%expect_test _ =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
 [(SEval
     (Elet (Recursive,
        (Evalue_binding (((PVar (Id "factorial")), None),
           (Efun (((PVar (Id "n")), None), [],
              (Eif_then_else (
                 (Ebin_op (Eq, (Evar (Id "n")), (Econst (Int 0)))),
                 (Econst (Int 1)),
                 (Some (Ebin_op (Mult, (Evar (Id "n")),
                          (Efun_application ((Evar (Id "factorial")),
                             (Ebin_op (Sub, (Evar (Id "n")), (Econst (Int 1))
                                ))
                             ))
                          )))
                 ))
              ))
           )),
        [], (Efun_application ((Evar (Id "factorial")), (Econst (Int 5)))))))
   ]
 |}]
;;

(*calculetion sequence*)
let%expect_test _ =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {|
  [(SEval
      (Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
         (Ebin_op (Mult, (Econst (Int 9002)),
            (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
         )))
    ]
  |}]
;;

let%expect_test _ =
  parse "if 1234 + 1 = 1235 then let x = 4 in x * 2";
  [%expect
    {|
    [(SEval
        (Eif_then_else (
           (Ebin_op (Eq, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 1)))),
              (Econst (Int 1235)))),
           (Elet (Non_recursive,
              (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 4)))),
              [], (Ebin_op (Mult, (Evar (Id "x")), (Econst (Int 2)))))),
           None)))
      ]
  |}]
;;

(*unallowable range for the int type*)
let%expect_test _ =
  parse "39482309482390842309482438208 + 2";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 3 in x + y;; if 13 > 12 then let a = 2 in a - 4";
  [%expect
    {|
  [(SEval
      (Elet (Non_recursive,
         (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 5)))),
         [],
         (Elet (Non_recursive,
            (Evalue_binding (((PVar (Id "y")), None), (Econst (Int 3)))),
            [], (Ebin_op (Add, (Evar (Id "x")), (Evar (Id "y"))))))
         )));
    (SEval
       (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
          (Elet (Non_recursive,
             (Evalue_binding (((PVar (Id "a")), None), (Econst (Int 2)))),
             [], (Ebin_op (Sub, (Evar (Id "a")), (Econst (Int 4)))))),
          None)))
    ] |}]
;;

let%expect_test _ =
  parse "let x = 5 ;; if 13 > 12 then let a = 2 in a + x";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "x")), None), (Econst (Int 5)))), []));
    (SEval
       (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
          (Elet (Non_recursive,
             (Evalue_binding (((PVar (Id "a")), None), (Econst (Int 2)))),
             [], (Ebin_op (Add, (Evar (Id "a")), (Evar (Id "x")))))),
          None)))
    ] |}]
;;

let%expect_test _ =
  parse "let rec factorial n = match n with 5 0 -> 1 5 1 -> 1 5 _ -> n * factorial(n - 1)";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let x = match 3 with | 1 -> 10 | 2 -> 20 | _ -> 30 ;;";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "x")), None),
         (Ematch ((Econst (Int 3)),
            (Ecase ((PConst (Int 1)), (Econst (Int 10)))),
            [(Ecase ((PConst (Int 2)), (Econst (Int 20))));
              (Ecase (PAny, (Econst (Int 30))))]
            ))
         )),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "(5 + 6) * 4";
  [%expect
    {|
  [(SEval
      (Ebin_op (Mult, (Ebin_op (Add, (Econst (Int 5)), (Econst (Int 6)))),
         (Econst (Int 4)))))
    ]
  |}]
;;

let%expect_test _ =
  parse "let (|?) a b = a/b + b*a in (|?) 3 ((|?) 5 6)";
  [%expect
    {|
  [(SEval
      (Elet (Non_recursive,
         (Evalue_binding (((PVar (Id "|?")), None),
            (Efun (((PVar (Id "a")), None), [((PVar (Id "b")), None)],
               (Ebin_op (Add,
                  (Ebin_op (Div, (Evar (Id "a")), (Evar (Id "b")))),
                  (Ebin_op (Mult, (Evar (Id "b")), (Evar (Id "a"))))))
               ))
            )),
         [],
         (Efun_application (
            (Efun_application ((Evar (Id "|?")), (Econst (Int 3)))),
            (Efun_application (
               (Efun_application ((Evar (Id "|?")), (Econst (Int 5)))),
               (Econst (Int 6))))
            ))
         )))
    ]
  |}]
;;

let%expect_test _ =
  parse "let x = match n with | [] -> 10 | h::tl -> 20 | h::m::tl -> 30 ;;";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "x")), None),
         (Ematch ((Evar (Id "n")), (Ecase ((PList []), (Econst (Int 10)))),
            [(Ecase ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                (Econst (Int 20))));
              (Ecase (
                 (PCons ((PVar (Id "h")),
                    (PCons ((PVar (Id "m")), (PVar (Id "tl")))))),
                 (Econst (Int 30))))
              ]
            ))
         )),
      []))
    ]
  |}]
;;

let%expect_test _ =
  parse "let w (Some c) (2::v)  = c";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding (((PVar (Id "w")), None),
         (Efun (((POption (Some (PVar (Id "c")))), None),
            [((PCons ((PConst (Int 2)), (PVar (Id "v")))), None)],
            (Evar (Id "c"))))
         )),
      []))
    ]
  |}]
;;
