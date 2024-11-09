Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0  = 1 | otherwise = y * save_fac (y - 1)
  [(FunDef ((Ident "fac"), ([], (PIdentificator (Ident "n")), []), [],
      (OrdBody
         ((IfThenEsle (
             ((Binop (((Identificator (Ident "n")), []), Less,
                 ((Const (Integer 0)), []))),
              []),
             ((OptionBld Nothing), []),
             ((FunctionApply (
                 ((Lambda (([], (PIdentificator (Ident "X")), []), [],
                     ((OptionBld (Just ((Identificator (Ident "X")), []))), [])
                     )),
                  []),
                 ((FunctionApply (((Identificator (Ident "save_fac")), []),
                     ((Identificator (Ident "n")), []), [])),
                  []),
                 [])),
              [])
             )),
          [])),
      [(FunDef ((Ident "save_fac"), ([], (PIdentificator (Ident "y")), []), 
          [],
          (Guards (
             (((Binop (((Identificator (Ident "y")), []), Equality,
                  ((Const (Integer 0)), []))),
               []),
              ((Const (Integer 1)), [])),
             [(((Identificator (Ident "otherwise")), []),
               ((Binop (((Identificator (Ident "y")), []), Multiply,
                   ((FunctionApply (((Identificator (Ident "save_fac")), []),
                       ((Binop (((Identificator (Ident "y")), []), Minus,
                           ((Const (Integer 1)), []))),
                        []),
                       [])),
                    [])
                   )),
                []))
               ]
             )),
          []))
        ]
      ))
    ]
