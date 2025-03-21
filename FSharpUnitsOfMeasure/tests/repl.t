(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(************************** do_not_type **************************)

  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/001.ml
  val fac : <type> = <fun>
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/002if.ml
  val main : <type> = 1
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/003occurs.ml
  val fix : <type> = <fun>
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/004let_poly.ml
  val _1 : <type> = (1, true)
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/005.ml
  val _2 : <type> = <fun>
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/015tuples.ml
  Interpreter error: Invalid syntax
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/016tuples_mismatch.ml
  Interpreter error: Match failure
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/097fun_vs_list.ml
  Interpreter error: Match failure
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/097fun_vs_unit.ml
  Interpreter error: Match failure
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/098rec_int.ml
  Interpreter error: Unbound identificator: x
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/099.ml
  Syntax error

(************************** typed **************************)

$ ../bin/repl.exe --no-hi --file manytests/typed/001fac.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/002fac.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/003fib.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/004manyargs.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/005fix.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/006partial.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/006partial2.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/006partial3.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/007order.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/008ascription.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/009let_poly.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/010sukharev.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/015tuples.ml
$ ../bin/repl.exe --no-hi --file manytests/typed/016lists.ml

