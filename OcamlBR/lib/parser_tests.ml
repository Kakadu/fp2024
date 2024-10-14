(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Parser

let parse p s show_program =
  match parse_string ~consume:All p s with
  | Ok ast -> print_endline (show_program ast)
  | _ -> Stdlib.print_endline "Parsing failed"
;;

(*factorial*)
let%expect_test _ =
  parse
    pexpr
    "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5"
    Ast.show_expr;
  [%expect
    {| 
   (Elet (Recursive, "factorial",
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
   |}]
;;

(*calculetion sequence*)
let%expect_test _ =
  parse pexpr "1234 + 676 - 9002 * (52 / 2)" Ast.show_expr;
  [%expect
    {| 
  (Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
     (Ebin_op (Mult, (Econst (Int 9002)),
        (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
     ))
  |}]
;;

(*unallowed function name*)
let%expect_test _ =
  parse pexpr "let rec 5 = ()" Ast.show_expr;
  [%expect {| 
  Parsing failed
  |}]
;;

(*unallowable range for the int type*)
let%expect_test _ =
  parse pexpr "39482309482390842309482438208 + 2" Ast.show_expr;
  [%expect {| 
  Parsing failed
  |}]
;;
