(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

  $ ../../../bin/print_ast.exe
  [(Binding
      { is_rec = Rec; pat = (PVar "fact");
        expr =
        (Fun ((PVar "n"),
           (Branch ((BinaryOp (LtEq, (Var "n"), (Lit (Int 1)))), (Lit (Int 1)),
              (BinaryOp (Mult, (Var "n"),
                 (App ((Var "fact"), (BinaryOp (Sub, (Var "n"), (Lit (Int 1))))
                    ))
                 ))
              ))
           ))
        })
    ]
