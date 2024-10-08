  $ ../bin/main.exe
  [(ExprLet (Rec,
      [((PatVar "fact"),
        (ExprFunc ("x",
           (ExprCond (
              (ExprBinop (Lesq, (ExprVar "x"), (ExprConst (ConstInt 1)))),
              (ExprConst (ConstInt 1)),
              (ExprBinop (Mul, (ExprVar "x"),
                 (ExprApp ((ExprVar "fact"),
                    (ExprBinop (Sub, (ExprVar "x"), (ExprConst (ConstInt 1))))
                    ))
                 ))
              ))
           )))
        ],
      (ExprVar "n")))
    ]
