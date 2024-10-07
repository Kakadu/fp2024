
   $ ../bin/main.exe
   (ExpLet (true, "factorial",         
   (ExpLambda ([(PatVariable "n")],
      (ExpIfThenElse (
         (ExpBinOper (LowerThan, (ExpIdent "n"), (ExpConst (ConstInt 2)))),
         (ExpConst (ConstInt 1)),
         (Some (ExpBinOper (Multiply, (ExpIdent "n"),
                  (ExpFunction ((ExpIdent "factorial"),
                     (ExpBinOper (Minus, (ExpIdent "n"),
                        (ExpConst (ConstInt 1))))
                     ))
                  )))
         ))
      )),
   (ExpIdent "factorial")))