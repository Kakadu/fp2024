(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Schemecwcc_lib.Ast

let () = let factorial : program = [
  Expr_define (
    "factorial", 
    Expr_lambda (
      ["n"],
      [Expr_if (
        Expr_apply (Expr_id "=", [Expr_id "n"; Expr_int 0]),
        Expr_int 1,
        Expr_apply (Expr_id "*", [Expr_id "n"; Expr_apply (Expr_id "factorial", [Expr_apply (Expr_id "-", [Expr_id "n"; Expr_int 1])])])
      )]
    )
  )  
] in 
print_endline (show_program factorial)
