  $ ../bin/REPL.exe -fromfile manytests/do_not_type/001.ml
  Type checking failed: Undefined variable 'fac'
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/002if.ml
  Type checking failed: unification failed on int and bool
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/003occurs.ml
  Type checking failed: Occurs check failed
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/004let_poly.ml
  Type checking failed: unification failed on bool and int
  
  Type checking failed: unification failed on string and int
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/015tuples.ml
  Type checking failed: Only variables are allowed as left-hand side of `let rec'
  Type checking failed: unification failed on '_0 * '_1 and int * int * int
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/099.ml
  Type checking failed: Only variables are allowed as left-hand side of `let rec'
  Type checking failed: Undefined variable 'x'
  Type checking failed: unification failed on '_1 option and '_0 -> '_0
  
  Type checking failed: unification failed on unit and '_2 -> '_2
  
  $ ../bin/REPL.exe -fromfile manytests/typed/001fac.ml
  fac : int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/002fac.ml
  fac_cps : int -> (int -> '_7) -> '_7
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/003fib.ml
  fib : int -> int
  fib_acc : int -> int -> int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/004manyargs.ml
  main : int
  test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  test3 : int -> int -> int -> int
  wrap : '_0 -> '_0
  $ ../bin/REPL.exe -fromfile manytests/typed/005fix.ml
  fac : (int -> int) -> int -> int
  fix : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial.ml
  foo : int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial2.ml
  foo : int -> int -> int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial3.ml
  foo : int -> int -> int -> unit
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/007order.ml
  _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  main : unit
  $ ../bin/REPL.exe -fromfile manytests/typed/008ascription.ml
  addi : ('_0 -> bool -> int) -> ('_0 -> bool) -> '_0 -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/009let_poly.ml
  temp : int * bool
  $ ../bin/REPL.exe -fromfile manytests/typed/010sukharev.ml
  _1 : int -> int -> int * '_1 -> bool
  _2 : int
  _3 : (int * string) option
  _4 : int -> '_14
  _42 : int -> bool
  _5 : int
  _6 : '_30 option -> '_30
  id1 : '_44 -> '_44
  id2 : '_45 -> '_45
  int_of_option : int option -> int
  $ ../bin/REPL.exe -fromfile manytests/typed/015tuples.ml
  feven : '_33 * (int -> int) -> int -> int
  fix : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5
  fixpoly : (('_21 -> '_25) * ('_21 -> '_25) -> '_21 -> '_25) * (('_21 -> '_25) * ('_21 -> '_25) -> '_21 -> '_25) -> ('_21 -> '_25) * ('_21 -> '_25)
  fodd : (int -> int) * '_41 -> int -> int
  main : int
  map : ('_9 -> '_11) -> '_9 * '_9 -> '_11 * '_11
  meven : int -> int
  modd : int -> int
  tie : (int -> int) * (int -> int)
  $ ../bin/REPL.exe -fromfile manytests/typed/016lists.ml
  append : '_67 list -> '_67 list -> '_67 list
  cartesian : '_98 list -> '_105 list -> '_98 * '_105 list
  concat : '_81 list list -> '_81 list
  iter : ('_87 -> unit) -> '_87 list -> unit
  length : '_3 list -> int
  length_tail : '_18 list -> int
  main : int
  map : ('_25 -> '_56) -> '_25 list -> '_56 list
