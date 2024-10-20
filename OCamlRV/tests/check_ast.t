Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan
SPDX-License-Identifier: LGPL-3.0-or-later

  $ echo "let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;" | ../bin/main.exe -dparsetree
  [(SValue (Rec,
      ((PVar "fact"),
       (ExprFun ((PVar "n"),
          (ExprIf (
             (ExprBinOperation (Lte, (ExprVariable "n"),
                (ExprLiteral (IntLiteral 1)))),
             (ExprLiteral (IntLiteral 1)),
             (Some (ExprBinOperation (Mul, (ExprVariable "n"),
                      (ExprApply ((ExprVariable "fact"),
                         (ExprBinOperation (Sub, (ExprVariable "n"),
                            (ExprLiteral (IntLiteral 1))))
                         ))
                      )))
             ))
          )))
      ))
    ]
