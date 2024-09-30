Copyright 2024-2027, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

  $ ../bin/main.exe
  ("factorial.ml",
   [(DefineItem
       ("factorial", Recursive, [(PVar "n")],
        (EvalStatement
           (If (
              (EvalStatement
                 (Binary ((Variable "n"), Gt, (Const (IntLiteral 1))))),
              (EvalStatement
                 (Binary ((Variable "n"), Multiply,
                    (Apply ((Variable "factorial"),
                       [(EvalStatement
                           (Binary ((Variable "n"), Subtract,
                              (Const (IntLiteral 1)))))
                         ]
                       ))
                    ))),
              (EvalStatement (Const (IntLiteral 1))))))));
     (StatementItem
        (EvalStatement
           (Apply ((Variable "factorial"),
              [(EvalStatement (Const (IntLiteral 5)))]))))
     ])
