Copyright 2024, Kostya Oreshin and Nikita Shchutskii

SPDX-License-Identifier: MIT

  $ ../bin/main.exe
  (FunBind ((Ident ("fac", None)),    
   (None, (PIdentificator (Ident ("n", None)))), [],
   (OrdBody
      (IfThenEsle (
         (Binop ((Identificator (Ident ("n", None))), Less, (Const (Int 0)))),
         (OptionBld Nothing),
         (OptionBld
            (Just
               (FunctionApply ((Identificator (Ident ("save_fac", None))),
                  (Identificator (Ident ("n", None))), []))))
         ))),
   [(FunBind ((Ident ("save_fac", None)),
       (None, (PIdentificator (Ident ("y", None)))), [],
       (Guards (
          ((Binop ((Identificator (Ident ("y", None))), Equality,
              (Const (Int 0)))),
           (Const (Int 1))),
          [((Const (Bool true)),
            (Binop ((Identificator (Ident ("y", None))), Multiply,
               (FunctionApply ((Identificator (Ident ("save_fac", None))),
                  (Binop ((Identificator (Ident ("y", None))), Minus,
                     (Const (Int 1)))),
                  []))
               )))
            ]
          )),
       []))
     ]
   ))
