Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev
SPDX-License-Identifier: MIT

  $ ../bin/astExample.exe
  ("factorial",
   { args = [("n", Type_int)]; returns = (Some (Only_types [Type_int]));
     body =
     [Stmt_if {init = None;
        cond =
        (Expr_bin_oper (Bin_equal, (Expr_ident "n"), (Expr_const (Const_int 0))
           ));
        if_body = [(Stmt_return [(Expr_const (Const_int 1))])];
        else_body =
        (Some (Stmt_block
                 [(Stmt_return
                     [(Expr_bin_oper (Bin_multiply, (Expr_ident "n"),
                         (Expr_call
                            ((Expr_ident "factorial"),
                             [(Expr_bin_oper (Bin_subtract, (Expr_ident "n"),
                                 (Expr_const (Const_int 1))))
                               ]))
                         ))
                       ])
                   ]))}
       ]
     })
