(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Pprintast

let%expect_test "apply test" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval (ExprApply (ExprVariable "f", ExprVariable "x")) ];
  [%expect {| f x;; |}]
;;

let%expect_test "cons test" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval (ExprCons (ExprVariable "f", ExprVariable "x")) ];
  [%expect {| f::x;; |}]
;;

let%expect_test "let expression test" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue (NonRec, (PVar "x", ExprLiteral (IntLiteral 5)), []) ];
  [%expect {| let x = 5;; |}]
;;

let%expect_test "factorial test" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( Rec
        , ( PVar "fact"
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
                                 (Sub, ExprVariable "n", ExprLiteral (IntLiteral 1)) ) ))
                  ) ) )
        , [] )
    ];
  [%expect {| let rec fact n = if (n <= (1)) then 1 else (n * (fact (n - (1))));; |}]
;;

let%expect_test "complex let test" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "a"
          , ExprLet
              ( NonRec
              , (PVar "b", ExprLiteral (IntLiteral 1))
              , []
              , ExprLet
                  ( NonRec
                  , (PVar "c", ExprLiteral (IntLiteral 1))
                  , []
                  , ExprBinOperation (Add, ExprVariable "b", ExprVariable "c") ) ) )
        , [] )
    ];
  [%expect {| let a = let b = 1 in let c = 1 in b + c;; |}]
;;

let%expect_test "tuple test" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "a"
          , ExprTuple
              ( ExprLiteral (IntLiteral 1)
              , ExprLiteral (StringLiteral "2")
              , [ ExprLiteral (IntLiteral 3) ] ) )
        , [] )
    ];
  [%expect {| let a = (1, "2", 3);; |}]
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
  [%expect {| 1::(2::(3::[]));; |}]
;;

let%expect_test "bin op with parentheses" =
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
  [%expect {| 1 + (2 * 2);; |}]
;;

let%expect_test "bin op with parentheses" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprBinOperation
           ( Mul
           , ExprBinOperation (Add, ExprLiteral (IntLiteral 1), ExprLiteral (IntLiteral 2))
           , ExprLiteral (IntLiteral 2) ))
    ];
  [%expect {| (1 + 2) * 2;; |}]
;;

let%expect_test "let expressions" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , (PVar "a", ExprLiteral (IntLiteral 1))
        , [ PVar "b", ExprLiteral (IntLiteral 2); PVar "c", ExprLiteral (IntLiteral 3) ]
        )
    ];
  [%expect {| let a = 1 and b = 2 and c = 3;; |}]
;;

let%expect_test "complex let epressions" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "a"
          , ExprLet
              ( NonRec
              , (PVar "x", ExprLiteral (IntLiteral 1))
              , [ PVar "y", ExprLiteral (IntLiteral 2) ]
              , ExprBinOperation (Add, ExprVariable "x", ExprVariable "y") ) )
        , [] )
    ];
  [%expect {| let a = let x = 1 and y = 2 in x + y;; |}]
;;

let%expect_test "let none expression" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue (NonRec, (PVar "a", ExprOption None), []) ];
  [%expect {| let a = None;; |}]
;;

let%expect_test "let some expression" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue (NonRec, (PVar "a", ExprOption (Some (ExprLiteral (IntLiteral 1)))), []) ];
  [%expect {| let a = Some (1);; |}]
;;

let%expect_test "bin op with if then else" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprBinOperation
           ( Add
           , ExprLiteral (IntLiteral 1)
           , ExprIf (ExprVariable "a", ExprVariable "b", Some (ExprVariable "c")) ))
    ];
  [%expect {| 1 + (if a then b else c);; |}]
;;

let%expect_test "bin op with if then else" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprBinOperation
           ( Add
           , ExprLiteral (IntLiteral 1)
           , ExprIf (ExprVariable "a", ExprVariable "b", Some (ExprVariable "c")) ))
    ];
  [%expect {| 1 + (if a then b else c);; |}]
;;

let%expect_test "pretty print match with multiple branches" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprMatch
           ( ExprVariable "x"
           , (PLiteral (IntLiteral 0), ExprLiteral (StringLiteral "zero"))
           , [ PLiteral (IntLiteral 1), ExprLiteral (StringLiteral "one")
             ; PAny, ExprLiteral (StringLiteral "other")
             ] ))
    ];
  [%expect {|
match x with
| 0 -> "zero"
| 1 -> "one"
| _ -> "other";;
|}]
;;

let%expect_test "let expr with match match with multiple branches" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "numder"
          , ExprMatch
              ( ExprVariable "arabic"
              , (PLiteral (IntLiteral 1), ExprLiteral (StringLiteral "one"))
              , [ PLiteral (IntLiteral 2), ExprLiteral (StringLiteral "two")
                ; PLiteral (IntLiteral 3), ExprLiteral (StringLiteral "three")
                ] ) )
        , [] )
    ];
  [%expect
    {|
let numder = match arabic with
| 1 -> "one"
| 2 -> "two"
| 3 -> "three";;
|}]
;;

let%expect_test "" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval (ExprUnOperation (UnaryNeg, ExprVariable "a")) ];
  [%expect {| not (a);; |}]
;;

let%expect_test "" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue (NonRec, (PType (PVar "a", AInt), ExprLiteral (IntLiteral 5)), []) ];
  [%expect {| let (a : int) = 5;; |}]
;;

let%expect_test "" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "f"
          , ExprFun
              ( PType (PVar "x", AInt)
              , ExprFun
                  ( PType (PVar "y", AInt)
                  , ExprBinOperation (Add, ExprVariable "x", ExprVariable "y") ) ) )
        , [] )
    ];
  [%expect {| let f (x : int) = fun (y : int) -> x + y;; |}]
;;

let%expect_test "" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue (NonRec, (PType (PVar "a", AList AInt), ExprLiteral NilLiteral), []) ];
  [%expect {| let (a : int list) = [];; |}]
;;
