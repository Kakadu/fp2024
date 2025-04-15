  $ ../bin/REPL.exe -parseast -filepath="../bin/factorial.cs"
  (Program
     (Class ([MPublic], (Id "Program"),
        [(Method ([MPublic], (TypeBase TypeInt), (Id "Factorial"),
            (Params [(Var ((TypeVar (TypeBase TypeInt)), (Id "n")))]),
            (SBlock
               [(SIf ((EBinOp (OpEqual, (EId (Id "n")), (EValue (ValInt 0)))),
                   (SBlock [(SReturn (Some (EValue (ValInt 1))))]),
                   (Some (SBlock
                            [(SReturn
                                (Some (EBinOp (OpMul, (EId (Id "n")),
                                         (EFuncCall ((EId (Id "Factorial")),
                                            [(EBinOp (OpSub, (EId (Id "n")),
                                                (EValue (ValInt 1))))
                                              ]
                                            ))
                                         ))))
                              ]))
                   ))
                 ])
            ));
          (Method ([MPublic; MStatic], TypeVoid, (Id "Main"), (Params []),
             (SBlock [])))
          ]
        )))
