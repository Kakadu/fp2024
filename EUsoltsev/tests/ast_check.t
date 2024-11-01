(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)  
  $ ../bin/main.exe
  [(ExpLet (true, (PatVariable "factorial"),
      (ExpLambda ([(PatVariable "n")],
         (ExpBranch (
            (ExpBinOper (LowerThan, (ExpIdent "n"), (ExpConst (ConstInt 2)))),
            (ExpConst (ConstInt 1)),
            (Some (ExpBinOper (Multiply, (ExpIdent "n"),
                     (ExpFunction ((ExpIdent "factorial"),
                        (ExpBinOper (Minus, (ExpIdent "n"),
                           (ExpConst (ConstInt 1))))
                        ))
                     )))
            ))
         )),
      None))
    ]
