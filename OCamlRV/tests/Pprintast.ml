(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Pprintast

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval (ExprApply (ExprVariable "f", ExprVariable "x")) ];
  [%expect {| f x;; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval (ExprCons (ExprVariable "f", ExprVariable "x")) ];
  [%expect {| f::x;; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue (NonRec, [ PVar "x", ExprLiteral (IntLiteral 5) ]) ];
  [%expect {| let x = 5;; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( Rec
        , [ ( PVar "fact"
            , ExprFun
                ( PVar "n"
                , ExprIf
                    ( ExprBinOperation (Lte, ExprVariable "n", ExprLiteral (IntLiteral 1))
                    , ExprLiteral (IntLiteral 1)
                    , Some
                        (ExprBinOperation
                           ( Mul
                           , ExprVariable "n"
                           , ExprApply
                               ( ExprVariable "fact"
                               , ExprBinOperation
                                   (Sub, ExprVariable "n", ExprLiteral (IntLiteral 1)) )
                           )) ) ) )
          ] )
    ];
  [%expect {| let rec fact n = if (n <= 1) then 1 else (n * fact ((n - 1)));; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , [ ( PVar "a"
            , ExprLet
                ( NonRec
                , [ PVar "b", ExprLiteral (IntLiteral 1) ]
                , ExprLet
                    ( NonRec
                    , [ PVar "c", ExprLiteral (IntLiteral 1) ]
                    , ExprBinOperation (Add, ExprVariable "b", ExprVariable "c") ) ) )
          ] )
    ];
  [%expect {| let a = let b = 1 in let c = 1 in (b + c);; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprTuple
           [ ExprLiteral (IntLiteral 1)
           ; ExprLiteral (StringLiteral "2")
           ; ExprLiteral (IntLiteral 3)
           ])
    ];
  [%expect {| (1, "2", 3);; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprCons
           ( ExprLiteral (IntLiteral 1)
           , ExprCons
               ( ExprLiteral (IntLiteral 2)
               , ExprCons (ExprLiteral (IntLiteral 3), ExprLiteral NilLiteral) ) ))
    ];
  [%expect {| 1::2::3::[];; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprBinOperation
           ( Add
           , ExprLiteral (IntLiteral 1)
           , ExprBinOperation (Mul, ExprLiteral (IntLiteral 2), ExprLiteral (IntLiteral 2))
           ))
    ];
  [%expect {| (1 + (2 * 2));; |}]
;;

let%expect_test _ =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprBinOperation
           ( Mul
           , ExprBinOperation (Add, ExprLiteral (IntLiteral 1), ExprLiteral (IntLiteral 2))
           , ExprLiteral (IntLiteral 2) ))
    ];
  [%expect {| ((1 + 2) * 2);; |}]
;;
