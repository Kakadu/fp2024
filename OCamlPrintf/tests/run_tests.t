Copyright 2024-2025, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../repl/REPL.exe -dparsetree -fromfile factorial.txt
  [(Struct_value (Recursive,
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

  $ ../repl/REPL.exe -dparsetree <<EOF
  > let prime n = 
  >  let rec check_zero x d = 
  >    match d with 
  >    | 1 -> true
  >    | _ -> x mod d <> 0 && check_zero x (d - 1) 
  >  in
  >  match n with
  >  | 0 -> false
  >  | 1 -> false
  >  | _ -> check_zero n (n - 1) ;;
  [(Struct_value (Nonrecursive,
      [{ pat = (Pat_var "prime");
         exp =
         (Exp_fun ([(Pat_var "n")],
            (Exp_let (Recursive,
               [{ pat = (Pat_var "check_zero");
                  exp =
                  (Exp_fun ([(Pat_var "x"); (Pat_var "d")],
                     (Exp_match ((Exp_ident "d"),
                        [{ left = (Pat_constant (Const_integer 1));
                           right = (Exp_construct ("true", None)) };
                          { left = Pat_any;
                            right =
                            (Exp_apply ((Exp_ident "&&"),
                               [(Exp_apply ((Exp_ident "<>"),
                                   [(Exp_apply ((Exp_ident "x"),
                                       [(Exp_ident "mod"); (Exp_ident "d")]));
                                     (Exp_constant (Const_integer 0))]
                                   ));
                                 (Exp_apply ((Exp_ident "check_zero"),
                                    [(Exp_ident "x");
                                      (Exp_apply ((Exp_ident "-"),
                                         [(Exp_ident "d");
                                           (Exp_constant (Const_integer 1))]
                                         ))
                                      ]
                                    ))
                                 ]
                               ))
                            }
                          ]
                        ))
                     ))
                  }
                 ],
               (Exp_match ((Exp_ident "n"),
                  [{ left = (Pat_constant (Const_integer 0));
                     right = (Exp_construct ("false", None)) };
                    { left = (Pat_constant (Const_integer 1));
                      right = (Exp_construct ("false", None)) };
                    { left = Pat_any;
                      right =
                      (Exp_apply ((Exp_ident "check_zero"),
                         [(Exp_ident "n");
                           (Exp_apply ((Exp_ident "-"),
                              [(Exp_ident "n");
                                (Exp_constant (Const_integer 1))]
                              ))
                           ]
                         ))
                      }
                    ]
                  ))
               ))
            ))
         }
        ]
      ))
    ]
