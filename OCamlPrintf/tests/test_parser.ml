(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Ast
open Ocaml_printf_lib.Parser

let run str =
  match parse str with
  | Ok ast -> print_endline (show_structure ast)
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

let%expect_test "parse_exp_fun" =
  run "let sum = fun x -> (fun y -> x + y);;";
  [%expect
    {|
    [(Struct_value (Nonrecursive,
        [{ pat = (Pat_var "sum");
           exp =
           (Exp_fun ([(Pat_var "x")],
              (Exp_fun ([(Pat_var "y")],
                 (Exp_apply ((Exp_ident "+"), [(Exp_ident "x"); (Exp_ident "y")]
                    ))
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

let%expect_test "parse_exp_construct" =
  run "let list_ = [1; 2; 3]";
  [%expect
    {|
    [(Struct_value (Nonrecursive,
        [{ pat = (Pat_var "list_");
           exp =
           (Exp_construct ("::",
              (Some (Exp_tuple
                       [(Exp_constant (Const_integer 1));
                         (Exp_construct ("::",
                            (Some (Exp_tuple
                                     [(Exp_constant (Const_integer 2));
                                       (Exp_construct ("::",
                                          (Some (Exp_tuple
                                                   [(Exp_constant
                                                       (Const_integer 3));
                                                     (Exp_construct ("[]", None))
                                                     ]))
                                          ))
                                       ]))
                            ))
                         ]))
              ))
           }
          ]
        ))
      ]
    |}]
;;

let%expect_test "check parse_chain_right_associative" =
  run "let f x y z = if x = 0 && y = 1 || z >= 2 then 2 else 26;;";
  [%expect
    {|
    [(Struct_value (Nonrecursive,
        [{ pat = (Pat_var "f");
           exp =
           (Exp_fun ([(Pat_var "x"); (Pat_var "y"); (Pat_var "z")],
              (Exp_ifthenelse (
                 (Exp_apply ((Exp_ident "||"),
                    [(Exp_apply ((Exp_ident "&&"),
                        [(Exp_apply ((Exp_ident "="),
                            [(Exp_ident "x"); (Exp_constant (Const_integer 0))]));
                          (Exp_apply ((Exp_ident "="),
                             [(Exp_ident "y"); (Exp_constant (Const_integer 1))]
                             ))
                          ]
                        ));
                      (Exp_apply ((Exp_ident ">="),
                         [(Exp_ident "z"); (Exp_constant (Const_integer 2))]))
                      ]
                    )),
                 (Exp_constant (Const_integer 2)),
                 (Some (Exp_constant (Const_integer 26)))))
              ))
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
  run "1 + let two = 2 in two * 3";
  [%expect
    {|
      [(Struct_eval
          (Exp_apply ((Exp_ident "+"),
             [(Exp_constant (Const_integer 1));
               (Exp_let (Nonrecursive,
                  [{ pat = (Pat_var "two"); exp = (Exp_constant (Const_integer 2))
                     }
                    ],
                  (Exp_apply ((Exp_ident "*"),
                     [(Exp_ident "two"); (Exp_constant (Const_integer 3))]))
                  ))
               ]
             )))
        ]
    |}]
;;

let%expect_test "parse_sequence_and_construct" =
  run {| [1; 2; 3]; "qwerty123" |};
  [%expect
    {|
    [(Struct_eval
        (Exp_sequence (
           (Exp_construct ("::",
              (Some (Exp_tuple
                       [(Exp_constant (Const_integer 1));
                         (Exp_construct ("::",
                            (Some (Exp_tuple
                                     [(Exp_constant (Const_integer 2));
                                       (Exp_construct ("::",
                                          (Some (Exp_tuple
                                                   [(Exp_constant
                                                       (Const_integer 3));
                                                     (Exp_construct ("[]", None))
                                                     ]))
                                          ))
                                       ]))
                            ))
                         ]))
              )),
           (Exp_constant (Const_string "qwerty123")))))
      ]
    |}]
;;

let%expect_test "parse_several_structure_items" =
  run "let squared x = x * x;; squared 5";
  [%expect
    {|
    [(Struct_value (Nonrecursive,
        [{ pat = (Pat_var "squared");
           exp =
           (Exp_fun ([(Pat_var "x")],
              (Exp_apply ((Exp_ident "*"), [(Exp_ident "x"); (Exp_ident "x")]))))
           }
          ]
        ));
      (Struct_eval
         (Exp_apply ((Exp_ident "squared"), [(Exp_constant (Const_integer 5))])))
      ]
    |}]
;;

let%expect_test "parse_sequence_and_list" =
  run {| [1; 2; 3]; "qwerty123" |};
  [%expect
    {|
    [(Struct_eval
        (Exp_sequence (
           (Exp_construct ("::",
              (Some (Exp_tuple
                       [(Exp_constant (Const_integer 1));
                         (Exp_construct ("::",
                            (Some (Exp_tuple
                                     [(Exp_constant (Const_integer 2));
                                       (Exp_construct ("::",
                                          (Some (Exp_tuple
                                                   [(Exp_constant
                                                       (Const_integer 3));
                                                     (Exp_construct ("[]", None))
                                                     ]))
                                          ))
                                       ]))
                            ))
                         ]))
              )),
           (Exp_constant (Const_string "qwerty123")))))
      ]
    |}]
;;

let%expect_test "parse_option_and_bool" =
  run {|
  let f a =
    match a with
    | Some _ -> true
    | None -> false
  ;;
  |};
  [%expect
    {|
    [(Struct_value (Nonrecursive,
        [{ pat = (Pat_var "f");
           exp =
           (Exp_fun ([(Pat_var "a")],
              (Exp_match ((Exp_ident "a"),
                 [{ left = (Pat_construct ("Some", (Some Pat_any)));
                    right = (Exp_construct ("true", None)) };
                   { left = (Pat_construct ("None", None));
                     right = (Exp_construct ("false", None)) }
                   ]
                 ))
              ))
           }
          ]
        ))
      ]
    |}]
;;
