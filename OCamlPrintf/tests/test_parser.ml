(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let run str =
  match Parser.parse str with
  | Ok ast -> print_endline (Ast.show_structure ast)
  | Error error -> print_endline error
;;

let%expect_test "parse_factorial_with_match" =
  run
    {|
    let rec factorial n =
      match n with
      | 0 -> 1
      | 1 -> 1
      | _ -> n * factorial (n - 1)
    ;;
    |};
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

let%expect_test "parse_pat_exp_tuples" =
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

let%expect_test "parse_exp_let" =
  run "1 + let one = 1 in f one 2 (3 + 4)";
  [%expect
    {|
      [(Struct_eval
          (Exp_apply ((Exp_ident "+"),
             [(Exp_constant (Const_integer 1));
               (Exp_let (Nonrecursive,
                  [{ pat = (Pat_var "one"); exp = (Exp_constant (Const_integer 1))
                     }
                    ],
                  (Exp_apply ((Exp_ident "f"),
                     [(Exp_ident "one"); (Exp_constant (Const_integer 2));
                       (Exp_apply ((Exp_ident "+"),
                          [(Exp_constant (Const_integer 3));
                            (Exp_constant (Const_integer 4))]
                          ))
                       ]
                     ))
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
