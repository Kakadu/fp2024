  $ ../bin/ast_fact.exe
  (Program
     (Class ([Public], (Id "Program"),
        [(Method ([Public; Static], (TypeBase TypeVoid), (Id "Main"), [],
            (StmtsBlock [])));
          (Method ([Public], (TypeBase TypeInt), (Id "Factorial"),
             [((TypeBase TypeInt), "n")],
             (StmtsBlock
                [(If ((BinOp (Equal, (IdExpr (Id "n")), (Value (ValInt 0)))),
                    (Return (Value (ValInt 1))),
                    (Some (Return
                             (BinOp (Mul, (IdExpr (Id "n")),
                                (FuncCall ((Id "Factorial"),
                                   [(BinOp (Sub, (IdExpr (Id "n")),
                                       (Value (ValInt 1))))
                                     ]
                                   ))
                                ))))
                    ))
                  ])
             ))
          ]
        )))
