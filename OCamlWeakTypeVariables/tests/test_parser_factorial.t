Copyright 2024-2025, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let rec factorial n = if n = 0 then 1 else n * factorial (n - 1)
  Parsed result: (Pstr_value (Recursive,
                    [{ pvb_pat = (Ppat_var "factorial");
                       pvb_expr =
                       (Pexp_fun ((Ppat_var "n"),
                          (Pexp_ifthenelse (
                             (Pexp_apply ((Pexp_ident "="),
                                [(Pexp_ident "n");
                                  (Pexp_constant (Pconst_int 0))]
                                )),
                             (Pexp_constant (Pconst_int 1)),
                             (Some (Pexp_apply ((Pexp_ident "*"),
                                      [(Pexp_ident "n");
                                        (Pexp_apply ((Pexp_ident "factorial"),
                                           [(Pexp_apply ((Pexp_ident "-"),
                                               [(Pexp_ident "n");
                                                 (Pexp_constant (Pconst_int 1))
                                                 ]
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
