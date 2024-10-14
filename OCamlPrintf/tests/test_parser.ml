(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let run str =
  match Ocaml_printf_lib.Parser.parse str with
  | Ok ast -> print_endline (Ocaml_printf_lib.Ast.show_structure ast)
  | Error error -> print_endline error
;;

let%expect_test "parse_factorial" =
  run "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)";
  [%expect
    {|
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
      |}]
;;

let%expect_test "parse_factorial_with_match" =
  run
    "let rec factorial n =\n\
    \  match n with\n\
    \  | 0 -> 1\n\
    \  | 1 -> 1\n\
    \  | _ -> n * factorial (n - 1)\n\
     ;;";
  [%expect
    {|
    [(Struct_value (Recursive,
        [{ pat = (Pat_var "factorial");
           exp =
           (Exp_fun ([(Pat_var "n")],
              (Exp_match ((Exp_ident "n"),
                 [{ left = (Pat_constant (Const_integer 0));
                    right = (Exp_constant (Const_integer 1)) };
                   { left = (Pat_constant (Const_integer 1));
                     right = (Exp_constant (Const_integer 1)) };
                   { left = Pat_any;
                     right =
                     (Exp_apply ((Exp_ident "*"),
                        [(Exp_ident "n");
                          (Exp_apply ((Exp_ident "factorial"),
                             [(Exp_apply ((Exp_ident "-"),
                                 [(Exp_ident "n");
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
          ]
        ))
      ]
      |}]
;;

let%expect_test "parse_pat_exp_tuple" =
  run "let a, b = 1, 2";
  [%expect
    {|
   [(Struct_value (Nonrecursive,
       [{ pat = (Pat_tuple [(Pat_var "a"); (Pat_var "b")]);
          exp =
          (Exp_tuple
             [(Exp_constant (Const_integer 1)); (Exp_constant (Const_integer 2))
               ])
          }
         ]
       ))
     ]
    |}]
;;

let%expect_test "parse_struct_eval" =
  run "8 / 800 - 555 * (35 + 35)";
  [%expect
    {|
    [(Struct_eval
        (Exp_apply ((Exp_ident "-"),
           [(Exp_apply ((Exp_ident "/"),
               [(Exp_constant (Const_integer 8));
                 (Exp_constant (Const_integer 800))]
               ));
             (Exp_apply ((Exp_ident "*"),
                [(Exp_constant (Const_integer 555));
                  (Exp_apply ((Exp_ident "+"),
                     [(Exp_constant (Const_integer 35));
                       (Exp_constant (Const_integer 35))]
                     ))
                  ]
                ))
             ]
           )))
      ]
      |}]
;;

let%expect_test "parse_several_structure_items" =
  run "let square x = x * x;; square 5";
  [%expect
    {|
    [(Struct_value (Nonrecursive,
        [{ pat = (Pat_var "square");
           exp =
           (Exp_fun ([(Pat_var "x")],
              (Exp_apply ((Exp_ident "*"), [(Exp_ident "x"); (Exp_ident "x")]))))
           }
          ]
        ));
      (Struct_eval
         (Exp_apply ((Exp_ident "square"), [(Exp_constant (Const_integer 5))])))
      ]
      |}]
;;
