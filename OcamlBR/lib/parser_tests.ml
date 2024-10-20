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
 [(Elet (Recursive, "factorial",
     (Efun (["n"],
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

(*calculetion sequence*)
let%expect_test _ =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {| 
   [(Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
       (Ebin_op (Mult, (Econst (Int 9002)),
          (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
       ))
     ]
  |}]
;;

(*unallowed function name*)
let%expect_test _ =
  parse "let rec 5 = ()";
  [%expect {| 
  Parsing failed
  |}]
;;

(*unallowable range for the int type*)
let%expect_test _ =
  parse "39482309482390842309482438208 + 2";
  [%expect {| 
  Parsing failed
  |}]
;;
