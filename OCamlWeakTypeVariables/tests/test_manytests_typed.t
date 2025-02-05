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
  Fatal error: exception (Failure OccursCheckFailed)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lib__Infer.run_structure_inferencer in file "lib/infer.ml" (inlined), line 592, characters 22-45
  Called from Lib__Infer.run_program_inferencer.(fun) in file "lib/infer.ml", line 599, characters 33-72
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Lib__Infer.run_program_inferencer in file "lib/infer.ml", line 597, characters 4-222
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 18, characters 23-59
  [2]

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
  Called from Lib__Infer.R.run in file "lib/infer.ml" (inlined), line 113, characters 23-31
  Called from Lib__Infer.run_structure_inferencer in file "lib/infer.ml" (inlined), line 583, characters 4-169
  Called from Lib__Infer.run_program_inferencer.(fun) in file "lib/infer.ml", line 599, characters 33-72
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Lib__Infer.run_program_inferencer in file "lib/infer.ml", line 597, characters 4-222
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 18, characters 23-59
  [2]
  
  $ ../bin/REPL.exe -dinferprogram < manytests/typed/016lists.ml
  Error: : end_of_input
