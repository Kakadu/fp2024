Copyright 2024-2027, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

  $ ../bin/REPL.exe -i=factorial.test
  
  $ ../bin/REPL.exe -dparsetree -i=factorial.test
  [(DefineItem
      (Recursive,
       [((PVar "factorial"),
         (Lambda ([(PVar "n")],
            (If ((Binary ((Variable "n"), Gt, (Const (IntLiteral 1)))),
               (Binary ((Variable "n"), Multiply,
                  (Apply ((Variable "factorial"),
                     [(Binary ((Variable "n"), Subtract, (Const (IntLiteral 1))
                         ))
                       ]
                     ))
                  )),
               (Some (Const (IntLiteral 1)))))
            )))
         ]));
    (EvalItem (Apply ((Variable "factorial"), [(Const (IntLiteral 5))])))]

  $ ../bin/REPL.exe -i=invalid-factorial.test
  ParseError(line=1 pos=66): Not found expression after keyword 'in' of let-xpression
