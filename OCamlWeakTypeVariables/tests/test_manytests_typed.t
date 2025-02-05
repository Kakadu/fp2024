  $ ../bin/REPL.exe -dinferprogram < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/005fix.ml
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/007order.ml
  Error: : end_of_input

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/008ascription.ml
  Error: : end_of_input

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/009let_poly.ml
  val temp : (int * bool)
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/010sukharev.ml
  Error: : end_of_input

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/015tuples.ml
  Fatal error: exception "Match_failure lib/infer.ml:361:2"
  Raised at Lib__Infer.infer_pattern.(fun) in file "lib/infer.ml", line 361, characters 2-446
  Called from Lib__Infer.infer_expr.helper.(fun) in file "lib/infer.ml", line 458, characters 30-65
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.(>>=) in file "lib/infer.ml", line 45, characters 10-13
  Called from Lib__Infer.R.run in file "lib/infer.ml" (inlined), line 113, characters 23-31
  Called from Lib__Infer.run_program_inferencer in file "lib/infer.ml", line 595, characters 2-65
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 18, characters 13-49
  [2]
  $ ../bin/REPL.exe -dinferprogram < manytests/typed/016lists.ml
  Error: : end_of_input
