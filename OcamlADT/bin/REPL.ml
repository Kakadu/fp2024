(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlADT.Lib

let () = 
  let factorial_ast: program = 
    (Exp_let (
      Recursive, 
      [
        { pat = Pat_var (Up_name "fact");
          expr = Exp_fun (
            [Pat_var (Up_name "n")],
            Exp_if (
              Exp_binop (Eq, Exp_var (Up_name "n"), Exp_constant (Const_integer 0)),
              Exp_constant (Const_integer 1 ),
              Some (
                Exp_binop (
                  Mul,
                  Exp_var (Up_name "n"),
                  Exp_apply (Exp_var (Up_name "fact"),
                    Exp_binop(Sub, Exp_var (Up_name "n"), Exp_constant (Const_integer 1))
                  )
                )
              )
            )
          )
        }
      ], 
      Exp_var (Up_name "fact"))
    )
  in
  print_endline(show_program factorial_ast)
;;