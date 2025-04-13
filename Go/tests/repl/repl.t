Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev
SPDX-License-Identifier: MIT

  $ ../../bin/interpret.exe --help
  Go subset interpreter
  
  Usage: interpret.exe <options> <filepath>
  
  If filepath isn't specified, REPL will start running and the program will be read from stdin
  In REPL mode type:
  
  	"guit" - to quit REPL mode
  	"help" - to display this message
  
  Options are:
  
  	--ast  Dump abstract syntax tree of a program
  	--typecheck  Typecheck the program and print result
    -help  Display this list of options
    --help  Display this list of options

  $ echo 'func main() { println("Hello, world!") }' > example.go
  $ ../../bin/interpret.exe --ast --typecheck example.go
  Running...          AST dump:
  [(Decl_func
      ("main",
       { args = []; returns = [];
         body =
         [(Stmt_call
             ((Expr_ident "println"),
              [(Arg_expr (Expr_const (Const_string "Hello, world!")))]))
           ]
         }))
    ]
  
  Typecheck result: correct
  Hello, world!
