  $ ../bin/ast_fact.exe
  (Program
     (Class ([MPublic], (Id "Program"),
        [(Method ([MPublic; MStatic], (TypeVar (TypeBase TypeVoid)),
            (Id "Main"), (Params []), (SBlock [])));
          (Method ([MPublic], (TypeVar (TypeBase TypeInt)), (Id "Factorial"),
             (Params [(Var ((TypeVar (TypeBase TypeInt)), (Id "n")))]),
             (SBlock
                [(SIf ((EBinOp (OpEqual, (EId (Id "n")), (EValue (ValInt 0)))),
                    (SReturn (Some (EValue (ValInt 1)))),
                    (Some (SReturn
                             (Some (EBinOp (OpMul, (EId (Id "n")),
                                      (EFuncCall ((EId (Id "Factorial")),
                                         [(EBinOp (OpSub, (EId (Id "n")),
                                             (EValue (ValInt 1))))
                                           ]
                                         ))
                                      )))))
                    ))
                  ])
             ))
          ]
        )))
