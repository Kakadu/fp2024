Copyright 2021-2024, Ilia Suponev
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe
  ("factorial_example.ml",
   [(DefinitionStatement (Recursive, Global, "factorial", ["n"],
       [(IfStatement (
           (BinaryExpression ((Variable "n"), Gt, (Const (IntLiteral 1)))),
           [(EvalStatement
               (BinaryExpression ((Variable "n"), Multiply,
                  (Apply ("factorial",
                     [(BinaryExpression ((Variable "n"), BinaryMinus,
                         (Const (IntLiteral 1))))
                       ]
                     ))
                  )))
             ],
           [(EvalStatement (Const (IntLiteral 1)))]))
         ]
       ))
     ])
