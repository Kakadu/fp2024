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
  Interpreter error: Invalid syntax
  

(************************** typed **************************)

  $ ../bin/repl.exe --no-hi --file manytests/typed/001fac.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  24
  val fac : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/002fac.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  24
  val fac_cps : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/003fib.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  3
  3
  val fib : <type> = <fun>
  val fib_acc : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/004manyargs.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  1111111111
  1
  10
  100
  val main : <type> = 0
  val test10 : <type> = <fun>
  val test3 : <type> = <fun>
  val wrap : <type> = <fun>
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/005fix.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  720
  val fac : <type> = <fun>
  val fix : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  1122
  val foo : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial2.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  1
  2
  3
  7
  val foo : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial3.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  4
  8
  9
  val foo : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/007order.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start : <type> = <fun>
  val main : <type> = ()
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/008ascription.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  8
  val addi : <type> = <fun>
  val main : <type> = 0
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/009let_poly.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  val temp : <type> = (1, true)
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/010sukharev.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  val _1 : <type> = <fun>
  val _2 : <type> = 1
  val _3 : <type> = Some (1, "hi")
  val _4 : <type> = <fun>
  val _42 : <type> = <fun>
  val _5 : <type> = 42
  val _6 : <type> = <fun>
  val id1 : <type> = <fun>
  val id2 : <type> = <fun>
  val int_of_option : <type> = <fun>
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/015tuples.ml
  Inference can't be done. To turn off this message, run REPL with --do-not-type option
  1
  1
  1
  1
  val feven : <type> = <fun>
  val fix : <type> = <fun>
  val fixpoly : <type> = <fun>
  val fodd : <type> = <fun>
  val main : <type> = 0
  val map : <type> = <fun>
  val meven : <type> = <fun>
  val modd : <type> = <fun>
  val tie : <type> = (<fun>, <fun>)
  
$ ../bin/repl.exe --no-hi --file manytests/typed/016lists.ml

