  $ ../../bin/main.exe
  [(Str_item_def (Recursive,
      [(Binding ((Pattern_ident "factorial"),
          (Expr_ifthenelse (
             (Expr_apply (
                (Expr_apply ((Expr_ident "<="), (Expr_const (Const_int 1)))),
                (Expr_ident "n"))),
             (Expr_const (Const_int 1)),
             (Some (Expr_apply (
                      (Expr_apply ((Expr_ident "*"),
                         (Expr_apply ((Expr_ident "factorial"),
                            (Expr_ident "n-1")))
                         )),
                      (Expr_ident "n"))))
             ))
          ))
        ]
      ))
    ]
