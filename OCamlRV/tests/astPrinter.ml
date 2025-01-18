(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.AstPrinter

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
    [ SValue (NonRec, (PVar "x", ExprConstant (CInt 5)), []) ];
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
                  ( ExprBinOperation (Lte, ExprVariable "n", ExprConstant (CInt 1))
                  , ExprConstant (CInt 1)
                  , Some
                      (ExprBinOperation
                         ( Mul
                         , ExprVariable "n"
                         , ExprApply
                             ( ExprVariable "fact"
                             , ExprBinOperation
                                 (Sub, ExprVariable "n", ExprConstant (CInt 1)) ) )) ) )
          )
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
              , (PVar "b", ExprConstant (CInt 1))
              , []
              , ExprLet
                  ( NonRec
                  , (PVar "c", ExprConstant (CInt 1))
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
              ( ExprConstant (CInt 1)
              , ExprConstant (CString "2")
              , [ ExprConstant (CInt 3) ] ) )
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
           ( ExprConstant (CInt 1)
           , ExprCons
               (ExprConstant (CInt 2), ExprCons (ExprConstant (CInt 3), ExprConstant CNil))
           ))
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
           , ExprConstant (CInt 1)
           , ExprBinOperation (Mul, ExprConstant (CInt 2), ExprConstant (CInt 2)) ))
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
           , ExprBinOperation (Add, ExprConstant (CInt 1), ExprConstant (CInt 2))
           , ExprConstant (CInt 2) ))
    ];
  [%expect {| (1 + 2) * 2;; |}]
;;

let%expect_test "let expressions" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , (PVar "a", ExprConstant (CInt 1))
        , [ PVar "b", ExprConstant (CInt 2); PVar "c", ExprConstant (CInt 3) ] )
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
              , (PVar "x", ExprConstant (CInt 1))
              , [ PVar "y", ExprConstant (CInt 2) ]
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
    [ SValue (NonRec, (PVar "a", ExprOption (Some (ExprConstant (CInt 1)))), []) ];
  [%expect {| let a = Some (1);; |}]
;;

let%expect_test "bin op with if then else" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SEval
        (ExprBinOperation
           ( Add
           , ExprConstant (CInt 1)
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
           , ExprConstant (CInt 1)
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
           , (PConstant (CInt 0), ExprConstant (CString "zero"))
           , [ PConstant (CInt 1), ExprConstant (CString "one")
             ; PAny, ExprConstant (CString "other")
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
              , (PConstant (CInt 1), ExprConstant (CString "one"))
              , [ PConstant (CInt 2), ExprConstant (CString "two")
                ; PConstant (CInt 3), ExprConstant (CString "three")
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

let%expect_test "let expr with match match with multiple branches" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "a"
          , ExprFunction
              ( (POption None, ExprConstant CUnit)
              , [ POption (Some (PVar "e")), ExprConstant CUnit ] ) )
        , [] )
    ];
  [%expect {|
let a = function
| None -> ()
| Some (e) -> ();;
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
    [ SValue (NonRec, (PType (PVar "a", AInt), ExprConstant (CInt 5)), []) ];
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
    [ SValue (NonRec, (PType (PVar "a", AList AInt), ExprConstant CNil), []) ];
  [%expect {| let (a : int list) = [];; |}]
;;

let%expect_test "" =
  Format.printf
    "%a\n"
    pp_structure_item_list
    [ SValue
        ( NonRec
        , ( PVar "addi"
          , ExprFun
              ( PVar "f"
              , ExprFun
                  ( PVar "g"
                  , ExprFun
                      ( PVar "x"
                      , ExprType
                          ( ExprApply
                              ( ExprApply (ExprVariable "f", ExprVariable "x")
                              , ExprType
                                  (ExprApply (ExprVariable "g", ExprVariable "x"), ABool)
                              )
                          , AInt ) ) ) ) )
        , [] )
    ];
  [%expect {| let addi f = fun g -> fun x -> ((f x) ((g x : bool)) : int);; |}]
;;
