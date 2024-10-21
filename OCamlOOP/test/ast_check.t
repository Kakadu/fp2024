Copyright 2024-2025, VersusXX, AlexandrKudrya
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/main.exe
  [(Str_value
      { d_rec = Rec; d_pat = (Pat_var "factorial");
        d_exp =
        (Exp_function ([(Pat_var "n")],
           (Exp_ifthenelse (
              (Exp_apply ((Exp_ident "<="),
                 [(Exp_ident "n"); (Exp_constant (Int 1))])),
              (Exp_constant (Int 1)),
              (Some (Exp_apply ((Exp_ident "*"),
                       [(Exp_ident "n");
                         (Exp_apply ((Exp_ident "factorial"),
                            [(Exp_apply ((Exp_ident "-"),
                                [(Exp_ident "n"); (Exp_constant (Int 1))]))
                              ]
                            ))
                         ]
                       )))
              ))
           ))
        })
    ]
