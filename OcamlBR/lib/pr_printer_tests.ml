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

let%expect_test _ =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
    let rec factorial = (fun n -> if n = 0 then 1 else n * factorial n - 1) in factorial 5 ;;
    [(SEval
        (Elet (Recursive,
           (Evalue_binding ((PVar (Id "factorial")),
              (Efun ((PVar (Id "n")), [],
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

let%expect_test _ =
  parse "if 5 > 6 then let x = 5 in x + 5";
  [%expect
    {|
    if 5 > 6 then let  x = 5 in x + 5 ;;
    [(SEval
        (Eif_then_else ((Ebin_op (Gt, (Econst (Int 5)), (Econst (Int 6)))),
           (Elet (Non_recursive,
              (Evalue_binding ((PVar (Id "x")), (Econst (Int 5)))), [],
              (Ebin_op (Add, (Evar (Id "x")), (Econst (Int 5)))))),
           None)))
      ]
    |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 6 in (x < y) and (x == y)";
  [%expect {|
    Parsing failed
    |}]
;;

let%expect_test _ =
  parse "[1; [2; 3]; 4]";
  [%expect
    {|
  [1; [2; 3]; 4] ;;
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
  [1 + 2; 3 * 4; 5 - 6] ;;
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
    let  a = (1, 2, 3) ;;
    [(SValue (Non_recursive,
        (Evalue_binding ((PVar (Id "a")),
           (Etuple ((Econst (Int 1)), (Econst (Int 2)), [(Econst (Int 3))])))),
        []))
      ]
  |}]
;;

let%expect_test _ =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {|
    1234 + 676 - 9002 * 52 / 2 ;;
    [(SEval
        (Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
           (Ebin_op (Mult, (Econst (Int 9002)),
              (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
           )))
      ]
    |}]
;;

let%expect_test _ =
  parse "if 1234 + 1 = 1235 then let x = 4 in (x, 2)";
  [%expect
    {|
    if 1234 + 1 = 1235 then let  x = 4 in (x, 2) ;;
    [(SEval
        (Eif_then_else (
           (Ebin_op (Eq, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 1)))),
              (Econst (Int 1235)))),
           (Elet (Non_recursive,
              (Evalue_binding ((PVar (Id "x")), (Econst (Int 4)))), [],
              (Etuple ((Evar (Id "x")), (Econst (Int 2)), [])))),
           None)))
      ]
    |}]
;;

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
    let  x = 5 in let  y = 3 in x + y ;;
    if 13 > 12 then let  a = 2 in a - 4 ;;
    [(SEval
        (Elet (Non_recursive,
           (Evalue_binding ((PVar (Id "x")), (Econst (Int 5)))), [],
           (Elet (Non_recursive,
              (Evalue_binding ((PVar (Id "y")), (Econst (Int 3)))), [],
              (Ebin_op (Add, (Evar (Id "x")), (Evar (Id "y"))))))
           )));
      (SEval
         (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
            (Elet (Non_recursive,
               (Evalue_binding ((PVar (Id "a")), (Econst (Int 2)))), [],
               (Ebin_op (Sub, (Evar (Id "a")), (Econst (Int 4)))))),
            None)))
      ]
  |}]
;;

let%expect_test _ =
  parse "let x = 5 ;; if 13 > 12 then let a = 2 in a + x";
  [%expect
    {|
    let  x = 5 ;;
    if 13 > 12 then let  a = 2 in a + x ;;
    [(SValue (Non_recursive,
        (Evalue_binding ((PVar (Id "x")), (Econst (Int 5)))), []));
      (SEval
         (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
            (Elet (Non_recursive,
               (Evalue_binding ((PVar (Id "a")), (Econst (Int 2)))), [],
               (Ebin_op (Add, (Evar (Id "a")), (Evar (Id "x")))))),
            None)))
      ]
  |}]
;;

let%expect_test _ =
  parse "let x = match 3 with | 1 -> 10 | 2 -> 20 | _ -> 30 ;;";
  [%expect
    {|
  let  x = match 3 with | 1 -> 10 | 2 -> 20 | _ -> 30 ;;
  [(SValue (Non_recursive,
      (Evalue_binding ((PVar (Id "x")),
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
  parse "((5 + 6) * (4 - 7)) - 1232";
  [%expect
    {|
  (5 + 6) * (4 - 7) - 1232 ;;
  [(SEval
      (Ebin_op (Sub,
         (Ebin_op (Mult, (Ebin_op (Add, (Econst (Int 5)), (Econst (Int 6)))),
            (Ebin_op (Sub, (Econst (Int 4)), (Econst (Int 7)))))),
         (Econst (Int 1232)))))
    ]
  |}]
;;

let%expect_test _ =
  parse "let x = 5";
  [%expect
    {|
  let  x = 5 ;;
  [(SValue (Non_recursive,
      (Evalue_binding ((PVar (Id "x")), (Econst (Int 5)))), []))
    ]
  |}]
;;

let%expect_test _ =
  parse "[1; 2; 3] = 1";
  [%expect
    {|
  [1; 2; 3] = 1 ;;
  [(SEval
      (Ebin_op (Eq,
         (Elist [(Econst (Int 1)); (Econst (Int 2)); (Econst (Int 3))]),
         (Econst (Int 1)))))
    ]
  |}]
;;

let%expect_test _ =
  parse "let x : int = 42;;";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let f : int -> string = fun x -> string_of_int x;;";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let y : (int * string * bool) = (1, \"hello\", true);;";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let l : int list = [1; 2; 3];;";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let g : (int -> bool) list = [(fun x -> x > 0); (fun x -> x < 0)];;";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let f : string -> (int -> bool) = fun x -> fun y -> x + y";
  [%expect {|
   Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let f = fun (3, true): int*bool -> 4";
  [%expect {|
   Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let f ((3, true) : int * bool) x ([ 7; 5 ] : int list) = 4";
  [%expect
    {|
   let  f = (fun ((3, true) : (int * bool)) x ([7; 5] : int list) -> 4) ;;
   [(SValue (Non_recursive,
       (Evalue_binding ((PVar (Id "f")),
          (Efun (
             (PConstraint (
                (PTuple ((PConst (Int 3)), (PConst (Bool true)), [])),
                (int * bool))),
             [(PVar (Id "x"));
               (PConstraint ((PList [(PConst (Int 7)); (PConst (Int 5))]),
                  int list))
               ],
             (Econst (Int 4))))
          )),
       []))
     ]
  |}]
;;
