Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev
SPDX-License-Identifier: MIT

  $ ../bin/astExample.exe
  ("factorial",
   { args = [("n", Type_int)]; return_types = (Some (Only_types [Type_int]));
     body =
     [(Stmt_if (None,
         (Expr_bin_oper (Bin_equal, (Expr_ident "n"),
            (Expr_const (Const_int 0)))),
         [(Stmt_return (Some (Expr_const (Const_int 1))))],
         (Some [(Stmt_return
                   (Some (Expr_bin_oper (Bin_multiply, (Expr_ident "n"),
                            (Expr_call
                               ((Expr_ident "factorial"),
                                [(Expr_bin_oper (Bin_subtract,
                                    (Expr_ident "n"),
                                    (Expr_const (Const_int 1))))
                                  ]))
                            ))))
                 ])
         ))
       ]
     })
