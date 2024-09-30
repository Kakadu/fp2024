open Haskell_lib.Ast

(* fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0  = 1 | otherwise = y * save_fac (y - 1) *)

let n =
  FunBind
    ( Ident ("fac", None) (* name of the function *)
    , (None, PIdentificator (Ident ("n", None))) (* parameter *)
    , []
    , OrdBody
        (* function body *)
        (IfThenEsle
           ( Binop (Identificator (Ident ("n", None)), Less, Const (Int 0))
             (* comparison n with 0 *)
           , OptionBld Nothing (* case n < 0: return Nothing *)
           , OptionBld
               (Just
                  (* else case *)
                  (FunctionApply
                     (* call a function "save_fac" with argument n *)
                     ( Identificator (Ident ("save_fac", None))
                     , Identificator (Ident ("n", None))
                     , [] ))) ))
    , [ FunBind
          (* function binding, in our case - "where save_fuc" *)
          ( Ident ("save_fac", None)
          , (None, PIdentificator (Ident ("y", None)))
          , []
          , Guards
              (* pattern match y with 0 and other cases *)
              (* case y == 0 *)
              ( ( Binop (Identificator (Ident ("y", None)), Equality, Const (Int 0))
                , Const (Int 1) )
                (* otherwise case *)
              , [ ( Const (Bool true)
                  , Binop
                      (* multiply y with called function *)
                      ( Identificator (Ident ("y", None))
                      , Multiply
                      , FunctionApply
                          (* call "save_fac" with argument (y - 1)*)
                          ( Identificator (Ident ("save_fac", None))
                          , Binop (Identificator (Ident ("y", None)), Minus, Const (Int 1))
                          , [] ) ) )
                ] )
          , [] )
      ] )
;;
