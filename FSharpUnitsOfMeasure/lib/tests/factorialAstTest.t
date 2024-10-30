  $ ../../bin/main.exe
  [(Str_item_def (Recursive,
      [(Binding ((Pattern_ident "factorial"),
          (Expr_ifthenelse (
             (Expr_apply (
                (Expr_apply ((Expr_ident_or_op "<="),
                   (Expr_const (Const_int 1)))),
                (Expr_ident_or_op "n"))),
             (Expr_const (Const_int 1)),
             (Some (Expr_apply (
                      (Expr_apply ((Expr_ident_or_op "*"),
                         (Expr_apply ((Expr_ident_or_op "factorial"),
                            (Expr_ident_or_op "n-1")))
                         )),
                      (Expr_ident_or_op "n"))))
             ))
          ))
        ]
      ))
    ]
