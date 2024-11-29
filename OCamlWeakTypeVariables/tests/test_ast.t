Copyright 2024, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/factorial_ast.exe
  (Pstr_value (Recursive, (Ppat_var "factorial"),
     (Pexpr_fun ((Ppat_var "n"),
        (Pexpr_ifThenElse (
           (Pexpr_apply ((Pexpr_ident (Id "=")),
              [(Pexpr_ident (Id "n")); (Pexpr_const (Pconst_int 5))])),
           (Pexpr_const (Pconst_int 1)),
           (Some (Pexpr_apply ((Pexpr_ident (Id "*")),
                    [(Pexpr_ident (Id "n"));
                      (Pexpr_apply ((Pexpr_ident (Id "factorial")),
                         [(Pexpr_apply ((Pexpr_ident (Id "-")),
                             [(Pexpr_ident (Id "n"));
                               (Pexpr_const (Pconst_int 1))]
                             ))
                           ]
                         ))
                      ]
                    )))
           ))
        ))
     ))
