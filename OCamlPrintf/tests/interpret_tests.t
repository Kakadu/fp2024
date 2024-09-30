Copyright 2024-2025, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/REPL.exe
  [(Str_value (Recursive,
      [{ pat = (Pat_var "factorial");
         exp =
         (Exp_fun ([(Pat_var "n")],
            (Exp_ifthenelse (
               (Exp_apply ((Exp_ident "<="),
                  [(Exp_ident "n"); (Exp_constant (Const_integer 1))])),
               (Exp_constant (Const_integer 1)),
               (Some (Exp_apply ((Exp_ident "*"),
                        [(Exp_ident "n");
                          (Exp_apply ((Exp_ident "factorial"),
                             [(Exp_apply ((Exp_ident "-"),
                                 [(Exp_ident "n");
                                   (Exp_constant (Const_integer 1))]
                                 ))
                               ]
                             ))
                          ]
                        )))
               ))
            ))
         }
        ]
      ))
    ]
