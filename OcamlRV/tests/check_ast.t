Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/main.exe
  [(ExprLet (Rec,
      [(PLiteral (StringLiteral "fact")); (PLiteral (StringLiteral "n"))],
      [(ExprIf (
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
        ]
      ))
    ]
