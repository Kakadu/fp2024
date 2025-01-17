Copyright 2024, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/factorial_ast.exe
  (Pstr_value (Recursive,
     [{ pvb_pat = (Ppat_var "factorial");
        pvb_expr =
        (Pexp_fun ((Ppat_var "n"),
           (Pexp_ifthenelse (
              (Pexp_apply ((Pexp_ident (Id "=")),
                 [(Pexp_ident (Id "n")); (Pexp_constant (Pconst_int 5))])),
              (Pexp_constant (Pconst_int 1)),
              (Some (Pexp_apply ((Pexp_ident (Id "*")),
                       [(Pexp_ident (Id "n"));
                         (Pexp_apply ((Pexp_ident (Id "factorial")),
                            [(Pexp_apply ((Pexp_ident (Id "-")),
                                [(Pexp_ident (Id "n"));
                                  (Pexp_constant (Pconst_int 1))]
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
