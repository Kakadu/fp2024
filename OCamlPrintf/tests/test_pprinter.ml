(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let run = Format.printf "%a \n" Pprinter.pp_structure

let%expect_test "pp_factorial_with_match" =
  run
    [ Struct_value
        ( Recursive
        , [ { pat = Pat_var "factorial"
            ; exp =
                Exp_fun
                  ( [ Pat_var "n" ]
                  , Exp_match
                      ( Exp_ident "n"
                      , [ { left = Pat_constant (Const_integer 0)
                          ; right = Exp_constant (Const_integer 1)
                          }
                        ; { left = Pat_constant (Const_integer 1)
                          ; right = Exp_constant (Const_integer 1)
                          }
                        ; { left = Pat_any
                          ; right =
                              Exp_apply
                                ( Exp_ident "*"
                                , [ Exp_ident "n"
                                  ; Exp_apply
                                      ( Exp_ident "factorial"
                                      , [ Exp_apply
                                            ( Exp_ident "-"
                                            , [ Exp_ident "n"
                                              ; Exp_constant (Const_integer 1)
                                              ] )
                                        ] )
                                  ] )
                          }
                        ] ) )
            }
          ] )
    ];
  [%expect
    {|
    let rec factorial = (fun n -> (match n with | 0 -> 1 | 1 -> 1 | _ -> n * (factorial n - 1)));;
    |}]
;;

let%expect_test "pp_exp_fun" =
  run
    [ Struct_value
        ( Nonrecursive
        , [ { pat = Pat_var "sum"
            ; exp =
                Exp_fun
                  ( [ Pat_var "x" ]
                  , Exp_fun
                      ( [ Pat_var "y" ]
                      , Exp_apply (Exp_ident "+", [ Exp_ident "x"; Exp_ident "y" ]) ) )
            }
          ] )
    ];
  [%expect "let sum = (fun x -> (fun y -> x + y));;"]
;;

let%expect_test "pp_pat_exp_tuples" =
  run
    [ Struct_value
        ( Nonrecursive
        , [ { pat = Pat_tuple [ Pat_var "a"; Pat_var "b" ]
            ; exp =
                Exp_tuple
                  [ Exp_constant (Const_integer 1); Exp_constant (Const_integer 2) ]
            }
          ] )
    ];
  [%expect "let (a, b) = (1, 2);;"]
;;

let%expect_test "check pp_chain_right_associative" =
  run
    [ Struct_value
        ( Nonrecursive
        , [ { pat = Pat_var "f"
            ; exp =
                Exp_fun
                  ( [ Pat_var "x"; Pat_var "y"; Pat_var "z" ]
                  , Exp_ifthenelse
                      ( Exp_apply
                          ( Exp_ident "||"
                          , [ Exp_apply
                                ( Exp_ident "&&"
                                , [ Exp_apply
                                      ( Exp_ident "="
                                      , [ Exp_ident "x"; Exp_constant (Const_integer 0) ]
                                      )
                                  ; Exp_apply
                                      ( Exp_ident "="
                                      , [ Exp_ident "y"; Exp_constant (Const_integer 1) ]
                                      )
                                  ] )
                            ; Exp_apply
                                ( Exp_ident ">="
                                , [ Exp_ident "z"; Exp_constant (Const_integer 2) ] )
                            ] )
                      , Exp_constant (Const_integer 2)
                      , Some (Exp_constant (Const_integer 26)) ) )
            }
          ] )
    ];
  [%expect "let f = (fun x y z -> (if x = 0 && y = 1 || z >= 2 then 2 else 26));;"]
;;

let%expect_test "pp_struct_eval" =
  run
    [ Struct_eval
        (Exp_apply
           ( Exp_ident "-"
           , [ Exp_apply
                 ( Exp_ident "/"
                 , [ Exp_constant (Const_integer 8); Exp_constant (Const_integer 800) ] )
             ; Exp_apply
                 ( Exp_ident "*"
                 , [ Exp_constant (Const_integer 555)
                   ; Exp_apply
                       ( Exp_ident "+"
                       , [ Exp_constant (Const_integer 35)
                         ; Exp_constant (Const_integer 35)
                         ] )
                   ] )
             ] ))
    ];
  [%expect "8 / 800 - 555 * (35 + 35);;"]
;;

let%expect_test "pp_exp_let" =
  run
    [ Struct_eval
        (Exp_apply
           ( Exp_ident "+"
           , [ Exp_constant (Const_integer 1)
             ; Exp_let
                 ( Nonrecursive
                 , [ { pat = Pat_var "two"; exp = Exp_constant (Const_integer 2) } ]
                 , Exp_apply
                     (Exp_ident "*", [ Exp_ident "two"; Exp_constant (Const_integer 3) ])
                 )
             ] ))
    ];
  [%expect "1 + (let two = 2 in two * 3);;"]
;;

let%expect_test "pp_sequence_and_construct" =
  run
    [ Struct_eval
        (Exp_sequence
           ( Exp_construct
               ( "::"
               , Some
                   (Exp_tuple
                      [ Exp_constant (Const_integer 1)
                      ; Exp_construct
                          ( "::"
                          , Some
                              (Exp_tuple
                                 [ Exp_constant (Const_integer 2)
                                 ; Exp_construct
                                     ( "::"
                                     , Some
                                         (Exp_tuple
                                            [ Exp_constant (Const_integer 3)
                                            ; Exp_construct ("[]", None)
                                            ]) )
                                 ]) )
                      ]) )
           , Exp_constant (Const_string "qwerty123") ))
    ];
  [%expect {| ([1; 2; 3]); ("qwerty123");;|}]
;;

let%expect_test "parse_several_structure_items" =
  run
    [ Struct_value
        ( Nonrecursive
        , [ { pat = Pat_var "squared"
            ; exp =
                Exp_fun
                  ( [ Pat_var "x" ]
                  , Exp_apply (Exp_ident "*", [ Exp_ident "x"; Exp_ident "x" ]) )
            }
          ] )
    ; Struct_eval (Exp_apply (Exp_ident "squared", [ Exp_constant (Const_integer 5) ]))
    ];
  [%expect {|
    let squared = (fun x -> x * x);;
    (squared 5);;
    |}]
;;
