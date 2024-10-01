Copyright 2024-2027, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

  $ ../bin/main.exe
  ("factorial.ml",
   [(DefineItem
       ("factorial", Recursive, [(PVar "n")],
        (EvalStatement
           (If (
              (EvalStatement
                 (Binary ((EvalStatement (Variable "n")), Gt,
                    (EvalStatement (Const (IntLiteral 1)))))),
              (EvalStatement
                 (Binary ((EvalStatement (Variable "n")), Multiply,
                    (EvalStatement
                       (Apply ((EvalStatement (Variable "factorial")),
                          [(EvalStatement
                              (Binary ((EvalStatement (Variable "n")),
                                 Subtract,
                                 (EvalStatement (Const (IntLiteral 1))))))
                            ]
                          )))
                    ))),
              (EvalStatement (Const (IntLiteral 1))))))));
     (StatementItem
        (EvalStatement
           (Apply ((EvalStatement (Variable "factorial")),
              [(EvalStatement (Const (IntLiteral 5)))]))))
     ])
