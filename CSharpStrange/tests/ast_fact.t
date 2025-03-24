  $ ../bin/ast_fact.exe
  (Program
     (Class ([MPublic], (Id "Program"),
        [(Method ([MPublic; MStatic], (TypeBase TypeVoid), (Id "Main"), 
            [], (SBlock [])));
          (Method ([MPublic], (TypeBase TypeInt), (Id "Factorial"),
             [((TypeBase TypeInt), "n")],
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
