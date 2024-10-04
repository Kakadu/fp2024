(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Haskell_lib.Ast

(* fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0  = 1 | otherwise = y * save_fac (y - 1) *)

let () =
  let fac_ast : binding =
    FunBind
      (* bind a function name with it's body *)
      ( (Ident "fac", None) (* name of the function without explicit return type *)
      , (None, PIdentificator (Ident "n"), None)
        (* single parameter n without explicit type *)
      , [] (* no more parameters *)
      , OrdBody
          (* function contains ordinary body (without guards) *)
          ( IfThenEsle
              (* start of conditional operator if-then-else *)
              ( ( Binop ((Identificator (Ident "n"), None), Less, (Const (Int 0), None))
                , None )
                (* comparison of parameter n with 0 *)
              , (OptionBld Nothing, None)
                (* case n < 0: return constructor "Nothing" of type "Maybe" *)
              , ( OptionBld
                    (* else case *)
                    (Just
                       (* due to the fact that function returns value of type "Maybe", we need to wrap result in "Just" *)
                       ( FunctionApply
                           (* call a function "save_fac" with argument n *)
                           ( (Identificator (Ident "save_fac"), None)
                             (* name of function without it's type *)
                           , (Identificator (Ident "n"), None)
                             (* argument n without type *)
                           , [] )
                         (* no more arguments *)
                       , None ))
                , None ) )
          , None )
      , [ FunBind
            (* function binding, in our case - "where save_fuc" *)
            ( (Ident "save_fac", None) (* name of function *)
            , (None, PIdentificator (Ident "y"), None) (* parameter of "save_fac" *)
            , [] (* no more arguments *)
            , Guards
                (* pattern match y with 0 and other cases *)
                (* case y == 0: return 1 *)
                ( ( ( Binop
                        ( (Identificator (Ident "y"), None)
                        , Equality
                        , (Const (Int 0), None) )
                    , None )
                  , (Const (Int 1), None) )
                  (* otherwise case *)
                , [ ( (Const (Bool true), None)
                    , ( Binop
                          (* multiply y with called function *)
                          ( (Identificator (Ident "y"), None)
                          , Multiply
                          , ( FunctionApply
                                (* call "save_fac" with argument (y - 1)*)
                                ( (Identificator (Ident "save_fac"), None)
                                , ( Binop
                                      (* decrement of y *)
                                      ( (Identificator (Ident "y"), None)
                                      , Minus
                                      , (Const (Int 1), None) )
                                  , None )
                                , [] )
                            , None ) )
                      , None ) )
                  ] )
            , [] )
        ] )
  in
  print_endline (show_binding fac_ast)
;;
