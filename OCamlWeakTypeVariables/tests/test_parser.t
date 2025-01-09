Copyright 2024-2025, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 2 * 2
  Parsed result: (Pstr_eval
                    (Pexp_apply ((Pexp_ident (Id "*")),
                       [(Pexp_constant (Pconst_int 2));
                         (Pexp_constant (Pconst_int 2))]
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 2 * ((2 * (124 * homka))) * (((2 * 1)))
  Parsed result: (Pstr_eval
                    (Pexp_apply ((Pexp_ident (Id "*")),
                       [(Pexp_apply ((Pexp_ident (Id "*")),
                           [(Pexp_constant (Pconst_int 2));
                             (Pexp_apply ((Pexp_ident (Id "*")),
                                [(Pexp_constant (Pconst_int 2));
                                  (Pexp_apply ((Pexp_ident (Id "*")),
                                     [(Pexp_constant (Pconst_int 124));
                                       (Pexp_ident (Id "homka"))]
                                     ))
                                  ]
                                ))
                             ]
                           ));
                         (Pexp_apply ((Pexp_ident (Id "*")),
                            [(Pexp_constant (Pconst_int 2));
                              (Pexp_constant (Pconst_int 1))]
                            ))
                         ]
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 2 * ((2 / (124 / homka))) * (1) * (2 / 2) * (((2 * 1)))
  Parsed result: (Pstr_eval
                    (Pexp_apply ((Pexp_ident (Id "*")),
                       [(Pexp_apply ((Pexp_ident (Id "*")),
                           [(Pexp_apply ((Pexp_ident (Id "*")),
                               [(Pexp_apply ((Pexp_ident (Id "*")),
                                   [(Pexp_constant (Pconst_int 2));
                                     (Pexp_apply ((Pexp_ident (Id "/")),
                                        [(Pexp_constant (Pconst_int 2));
                                          (Pexp_apply ((Pexp_ident (Id "/")),
                                             [(Pexp_constant (Pconst_int 124));
                                               (Pexp_ident (Id "homka"))]
                                             ))
                                          ]
                                        ))
                                     ]
                                   ));
                                 (Pexp_constant (Pconst_int 1))]
                               ));
                             (Pexp_apply ((Pexp_ident (Id "/")),
                                [(Pexp_constant (Pconst_int 2));
                                  (Pexp_constant (Pconst_int 2))]
                                ))
                             ]
                           ));
                         (Pexp_apply ((Pexp_ident (Id "*")),
                            [(Pexp_constant (Pconst_int 2));
                              (Pexp_constant (Pconst_int 1))]
                            ))
                         ]
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x -> 5
  Parsed result: (Pstr_eval
                    (Pexp_fun ((Ppat_var "x"), (Pexp_constant (Pconst_int 5)))))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x -> fun y -> fun z -> 5
  Parsed result: (Pstr_eval
                    (Pexp_fun ((Ppat_var "x"),
                       (Pexp_fun ((Ppat_var "y"),
                          (Pexp_fun ((Ppat_var "z"),
                             (Pexp_constant (Pconst_int 5))))
                          ))
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x y z -> 5
  Parsed result: (Pstr_eval
                    (Pexp_fun ((Ppat_var "x"),
                       (Pexp_fun ((Ppat_var "y"),
                          (Pexp_fun ((Ppat_var "z"),
                             (Pexp_constant (Pconst_int 5))))
                          ))
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > if x then y else z
  Parsed result: (Pstr_eval
                    (Pexp_ifthenelse ((Pexp_ident (Id "x")),
                       (Pexp_ident (Id "y")), (Some (Pexp_ident (Id "z"))))))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > if x then if y then z
  Parsed result: (Pstr_eval
                    (Pexp_ifthenelse ((Pexp_ident (Id "x")),
                       (Pexp_ifthenelse ((Pexp_ident (Id "y")),
                          (Pexp_ident (Id "z")), None)),
                       None)))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 2 * if true then 2 else 1
  Parsed result: (Pstr_eval
                    (Pexp_apply ((Pexp_ident (Id "*")),
                       [(Pexp_constant (Pconst_int 2));
                         (Pexp_ifthenelse (
                            (Pexp_constant (Pconst_boolean true)),
                            (Pexp_constant (Pconst_int 2)),
                            (Some (Pexp_constant (Pconst_int 1)))))
                         ]
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x y -> if x then y
  Parsed result: (Pstr_eval
                    (Pexp_fun ((Ppat_var "x"),
                       (Pexp_fun ((Ppat_var "y"),
                          (Pexp_ifthenelse ((Pexp_ident (Id "x")),
                             (Pexp_ident (Id "y")), None))
                          ))
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x -> fun y -> if x then y else x
  Parsed result: (Pstr_eval
                    (Pexp_fun ((Ppat_var "x"),
                       (Pexp_fun ((Ppat_var "y"),
                          (Pexp_ifthenelse ((Pexp_ident (Id "x")),
                             (Pexp_ident (Id "y")),
                             (Some (Pexp_ident (Id "x")))))
                          ))
                       )))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > f y z
  Parsed result: (Pstr_eval
                    (Pexp_apply ((Pexp_ident (Id "f")),
                       [(Pexp_ident (Id "y")); (Pexp_ident (Id "z"))])))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let homka = 5 in homka
  Parsed result: (Pstr_eval
                    (Pexp_let (NonRecursive,
                       [{ pvb_pat = (Ppat_var "homka");
                          pvb_expr = (Pexp_constant (Pconst_int 5)) }
                         ],
                       (Pexp_ident (Id "homka")))))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let homka = fun x -> x + 2 in homka
  Parsed result: (Pstr_eval
                    (Pexp_let (NonRecursive,
                       [{ pvb_pat = (Ppat_var "homka");
                          pvb_expr =
                          (Pexp_fun ((Ppat_var "x"),
                             (Pexp_apply ((Pexp_ident (Id "+")),
                                [(Pexp_ident (Id "x"));
                                  (Pexp_constant (Pconst_int 2))]
                                ))
                             ))
                          }
                         ],
                       (Pexp_ident (Id "homka")))))
