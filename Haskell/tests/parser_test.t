Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0  = 1 | otherwise = y * save_fac (y - 1)
  (FunBind (((Ident "fac"), None), ([], (PIdentificator (Ident "n")), None),
     [],
     (OrdBody
        ((IfThenEsle (
            ((Binop (((Identificator (Ident "n")), None), Less,
                ((Const (Integer 0)), None))),
             None),
            ((OptionBld Nothing), None),
            ((FunctionApply (
                ((Lambda (([], (PIdentificator (Ident "X")), None), [],
                    ((OptionBld (Just ((Identificator (Ident "X")), None))),
                     None)
                    )),
                 None),
                ((FunctionApply (((Identificator (Ident "save_fac")), None),
                    ((Identificator (Ident "n")), None), [])),
                 None),
                [])),
             None)
            )),
         None)),
     [(FunBind (((Ident "save_fac"), None),
         ([], (PIdentificator (Ident "y")), None), [],
         (Guards (
            (((Binop (((Identificator (Ident "y")), None), Equality,
                 ((Const (Integer 0)), None))),
              None),
             ((Const (Integer 1)), None)),
            [(((Identificator (Ident "otherwise")), None),
              ((Binop (((Identificator (Ident "y")), None), Multiply,
                  ((FunctionApply (((Identificator (Ident "save_fac")), None),
                      ((Binop (((Identificator (Ident "y")), None), Minus,
                          ((Const (Integer 1)), None))),
                       None),
                      [])),
                   None)
                  )),
               None))
              ]
            )),
         []))
       ]
     ))
