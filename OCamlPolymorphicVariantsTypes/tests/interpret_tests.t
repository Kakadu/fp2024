Copyright 2024-2027, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

  $ ../bin/main.exe
  ("factorial.ml",
   [(DefineItem
       (Recursive,
        [("factorial", [(PVar "n")],
          (If ((Binary ((Variable "n"), Gt, (Const (IntLiteral 1)))),
             (Binary ((Variable "n"), Multiply,
                (Apply ((Variable "factorial"),
                   [(Binary ((Variable "n"), Subtract, (Const (IntLiteral 1))))
                     ]
                   ))
                )),
             (Const (IntLiteral 1)))))
          ]));
     (EvalItem (Apply ((Variable "factorial"), [(Const (IntLiteral 5))])))])
