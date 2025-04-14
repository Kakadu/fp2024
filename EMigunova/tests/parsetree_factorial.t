(** Copyright 2025, Migunova Anastasia *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/REPL.exe --dparsetree <<EOF
  > let rec factorial n  =  if n <= 1 then 1 else n * factorial (n-1) 
  [(Let_binding (Recursive, (Let_fun ("factorial", [(Pattern_var "n")])),
      (Expr_if_then_else (
         (Expr_binary_op (LessEqual, (Expr_var "n"), (Expr_const (Const_int 1))
            )),
         (Expr_const (Const_int 1)),
         (Expr_binary_op (Mul, (Expr_var "n"),
            (Expr_application ((Expr_var "factorial"),
               [(Expr_binary_op (Sub, (Expr_var "n"),
                   (Expr_const (Const_int 1))))
                 ]
               ))
            ))
         ))
      ))
    ]
 

