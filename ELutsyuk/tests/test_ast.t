(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

  $ ../bin/print_ast.exe
  [(ExpLet (NonRec,
      [("fac",
        (ExpLambda ("n",
           (ExpLet (Rec,
              [("aux",
                (ExpLambda ("n",
                   (ExpLambda ("acc",
                      (ExpMatch ((ExpVar "n"),
                         [((PatLiteral (Int 0)), (ExpVar "acc"));
                           (PatAny,
                            (ExpApp (
                               (ExpApp ((ExpVar "aux"),
                                  (ExpBinop (Sub, (ExpVar "n"),
                                     (ExpConst (Int 1))))
                                  )),
                               (ExpBinop (Mul, (ExpVar "acc"), (ExpVar "n"))))))
                           ]
                         ))
                      ))
                   )))
                ],
              (ExpApp ((ExpApp ((ExpVar "aux"), (ExpVar "n"))),
                 (ExpConst (Int 1))))
              ))
           )))
        ],
      ExpOptNone))
    ]
