(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

  $ ../../../bin/print_ast.exe
  [(Binding
      { is_rec = Rec; pat = (PVar "fact");
        expr =
        (Fun ((PVar "n"),
           (Branch ((BinaryOp (LtEq, (Var "n"), (Cons (Int 1)))),
              (Cons (Int 1)),
              (BinaryOp (Mult, (Var "n"),
                 (App ((Var "fact"),
                    (BinaryOp (Sub, (Var "n"), (Cons (Int 1))))))
                 ))
              ))
           ))
        })
    ]
