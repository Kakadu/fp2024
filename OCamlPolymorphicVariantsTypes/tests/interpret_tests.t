Copyright 2024-2027, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

  $ ../bin/main.exe
  [(DefineItem
      (Recursive,
       [((PVar "factorial"),
         (Lambda ([(PVar "n")],
            (If ((Binary ((Variable "n"), Gt, (Const (IntLiteral 1)))),
               (Binary ((Variable "n"), Multiply, (Const (IntLiteral 1)))),
               (Some (Const (IntLiteral 1)))))
            )))
         ]));
    (EvalItem (Apply ((Variable "factorial"), [(Const (IntLiteral 5))])))]
