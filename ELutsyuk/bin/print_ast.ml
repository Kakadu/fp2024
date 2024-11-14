(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open MiniML.Ast

(* let fac_ast =
   let rec aux n acc =
   match n with
   | 0 -> acc
   | _ -> aux (n - 1) (acc * n)
   in
   aux n 1
   ;; *)

let () =
  let fac : program =
    [ ExpLet
        ( NonRec
        , [ ( "fac"
            , ExpLambda
                ( "n"
                , ExpLet
                    ( Rec
                    , [ ( "aux"
                        , ExpLambda
                            ( "n"
                            , ExpLambda
                                ( "acc"
                                , ExpMatch
                                    ( ExpVar "n"
                                    , [ PatLiteral (Int 0), ExpVar "acc"
                                      ; ( PatAny
                                        , ExpApp
                                            ( ExpApp
                                                ( ExpVar "aux"
                                                , ExpBinop
                                                    (Sub, ExpVar "n", ExpConst (Int 1)) )
                                            , ExpBinop (Mul, ExpVar "acc", ExpVar "n") ) )
                                      ] ) ) ) )
                      ]
                    , ExpApp (ExpApp (ExpVar "aux", ExpVar "n"), ExpConst (Int 1)) ) ) )
          ]
        , ExpOptNone )
    ]
  in
  print_endline (show_program fac)
;;
