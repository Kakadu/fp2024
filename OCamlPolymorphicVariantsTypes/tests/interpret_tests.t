Copyright 2024-2027, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

  $ ../bin/main.exe
  ("let rec factorial n = if (n > 1) then n * factorial(n-1) else 1",
   [(DefinitionStatement (Recursive, Global, "factorial", ["n"],
       [(EvalStatement
           (IfExpression (
              [(EvalStatement
                  (BinaryExpression ((Variable "n"), Gt, (Const (IntLiteral 1))
                     )))
                ],
              [(EvalStatement
                  (BinaryExpression ((Variable "n"), Multiply,
                     (Apply ((Variable "factorial"),
                        [(EvalStatement
                            (BinaryExpression ((Variable "n"), Subtract,
                               (Const (IntLiteral 1)))))
                          ]
                        ))
                     )))
                ],
              [(EvalStatement (Const (IntLiteral 1)))])))
         ]
       ))
     ])
