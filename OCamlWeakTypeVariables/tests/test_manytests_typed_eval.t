  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) (fun x -> x) (fun x -> x) 5
  Value: 5
  $ ../bin/REPL.exe < manytests/typed/001fac.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/002fac.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/003fib.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/004manyargs.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/005fix.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/006partial2.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/006partial3.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/006partial.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/007order.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/008ascription.ml
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/009let_poly.ml
  Fatal error: exception "Match_failure lib/interpreter.ml:185:27"
  Raised at Lib__Interpreter.Inter.eval_structure in file "lib/interpreter.ml", line 185, characters 27-119
  Called from Lib__Interpreter.run_interpret_exn in file "lib/interpreter.ml", line 213, characters 8-31
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 50, characters 19-58
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 48, characters 6-249
  [2]
  $ cat manytests/typed/010sukharev.ml | ../bin/REPL.exe
  Error: : end_of_input

  $ ../bin/REPL.exe < manytests/typed/015tuples.ml
  Error: : end_of_input
  $ ../bin/REPL.exe < manytests/typed/016lists.ml
  Error: : end_of_input
