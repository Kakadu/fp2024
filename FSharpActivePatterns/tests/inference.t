  $ ../bin/REPL.exe -fromfile manytests/do_not_type/001.ml
  Type checking failed: Undefined variable 'fac'
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/002if.ml
  Type checking failed: unification failed on int
   and bool
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/003occurs.ml
  Type checking failed: Occurs check failed
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/004let_poly.ml
  _2 : int -> int option -> int
  Type checking failed: unification failed on bool
   and int
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/015tuples.ml
  Type checking failed: Only variables are allowed as left-hand side of `let rec'
  Type checking failed: unification failed on ('_0, '_1)
   and (int, int, int)
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/099.ml
  Type checking failed: Only variables are allowed as left-hand side of `let rec'
  Type checking failed: Undefined variable 'x'
  Type checking failed: unification failed on '_1 option
   and '_0 -> '_0
  
  Type checking failed: unification failed on unit
   and '_2 -> '_2
  
  $ ../bin/REPL.exe -fromfile manytests/typed/001fac.ml
  fac : int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/002fac.ml
  fac_cps : int -> (int -> '_7) -> '_7
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/003fib.ml
  fib_acc : int -> int -> int -> int
  fib : int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/004manyargs.ml
  wrap : '_0 -> '_0
  test3 : '_5 -> '_4 -> '_3 -> int
  test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/005fix.ml
  fix : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5
  fac : (int -> int) -> int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial.ml
  foo : bool -> int -> int
  foo : int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial2.ml
  foo : int -> int -> int -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial3.ml
  foo : '_0 -> '_2 -> int -> unit
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/007order.ml
  _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> '_0 -> int
  main : unit
  $ ../bin/REPL.exe -fromfile manytests/typed/008ascription.ml
  addi : ('_0 -> bool -> int) -> ('_0 -> bool) -> '_0 -> int
  main : int
  $ ../bin/REPL.exe -fromfile manytests/typed/009let_poly.ml
  temp : (int, bool)
  $ ../bin/REPL.exe -fromfile manytests/typed/010sukharev.ml
  _1 : int -> int -> (int, '_1) -> bool
  _2 : int
  _3 : (int, string) option
  _4 : int -> '_14
  _5 : int
  _6 : '_28 option -> '_28
  int_of_option : int option -> int
  _42 : int -> bool
  id1 : '_42 -> '_42
  id2 : '_43 -> '_43
  $ ../bin/REPL.exe -fromfile manytests/typed/015tuples.ml
  fix : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5
  map : ('_9 -> '_11) -> '_7 -> ('_11, '_11)
  fixpoly : '_14 -> ('_22 -> '_26, '_22 -> '_26)
  feven : '_33 -> int -> int
  fodd : '_40 -> int -> int
  tie : ('_47 -> '_48, '_47 -> '_48)
  Type checking failed: Undefined variable 'modd'
  : string
  $ ../bin/REPL.exe -fromfile manytests/typed/016lists.ml
  length : '_3 list -> int
  length_tail : '_18 list -> int
  map : ('_25 -> '_56) -> '_25 list -> '_56 list
  append : '_67 list -> '_67 list -> '_67 list
  concat : '_81 list list -> '_81 list
  iter : '_85 -> '_87 list -> unit
  cartesian : '_98 list -> '_105 list -> ('_98, '_105) list
  main : int
