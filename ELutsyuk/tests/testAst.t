(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

  $ ../bin/printFactorialAst.exe
  [(Value (Rec,
      (Binding ((PatVar "fact"),
         (Fun ((PatVar "n"), [],
            (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
               (BinOp (Mul, (Var "n"),
                  (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1))))
                     ))
                  ))
               ))
            ))
         )),
      []))
    ]
