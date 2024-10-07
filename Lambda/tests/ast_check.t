
  $ ../bin/main.exe

  (Ast.ExpLet (true, "factorial",     
   (Ast.ExpLambda ([(PatVariable "n")],
      (Ast.ExpIfThenElse (
         (Ast.ExpBinOper (LowerThan, (Ast.ExpIdent "n"),
            (Ast.ExpConst (ConstInt 2)))),
         (Ast.ExpConst (ConstInt 1)),
         (Some (Ast.ExpBinOper (Multiply, (Ast.ExpIdent "n"),
                  (Ast.ExpFunction ((Ast.ExpIdent "factorial"),
                     (Ast.ExpBinOper (Minus, (Ast.ExpIdent "n"),
                        (Ast.ExpConst (ConstInt 2))))
                     ))