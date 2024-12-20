  $ ../bin/REPL.exe -fromfile manytests/do_not_type/001.ml
  Type checking failed: Undefined variable 'fac'
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/002if.ml
  Type checking failed: unification failed on int
   and bool
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/003occurs.ml
  Type checking failed: Occurs check failed
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/004let_poly.ml
  - : int -> int option -> int
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
  - : int -> int
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/002fac.ml
  - : int -> (int -> '_7) -> '_7
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/003fib.ml
  - : int -> int -> int -> int
  - : int -> int
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/004manyargs.ml
  - : '_0 -> '_0
  - : '_5 -> '_4 -> '_3 -> int
  - : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/005fix.ml
  - : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5
  - : (int -> int) -> int -> int
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial.ml
  - : bool -> int -> int
  Error occured
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial2.ml
  - : int -> int -> int -> int
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial3.ml
  - : '_0 -> '_2 -> int -> unit
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/007order.ml
  - : unit -> unit -> int -> unit -> int -> int -> unit -> int -> '_0 -> int
  - : unit
  $ ../bin/REPL.exe -fromfile manytests/typed/008ascription.ml
  - : ('_0 -> bool -> int) -> ('_0 -> bool) -> '_0 -> int
  - : int
  $ ../bin/REPL.exe -fromfile manytests/typed/009let_poly.ml
  - : (int, bool)
  $ ../bin/REPL.exe -fromfile manytests/typed/010sukharev.ml
  - : int -> int -> (int, '_1) -> bool
  - : int
  - : (int, string) option
  - : int -> '_14
  - : int
  - : '_28 option -> '_28
  - : int option -> int
  - : int -> bool
  - : '_42 -> '_42
  - : '_43 -> '_43
  $ ../bin/REPL.exe -fromfile manytests/typed/015tuples.ml
  - : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5
  - : ('_9 -> '_11) -> '_7 -> ('_11, '_11)
  - : '_14 -> ('_22 -> '_26, '_22 -> '_26)
  - : '_33 -> int -> int
  - : '_40 -> int -> int
  - : ('_47 -> '_48, '_47 -> '_48)
  Type checking failed: Undefined variable 'modd'
  Error occured
  $ ../bin/REPL.exe -fromfile manytests/typed/016lists.ml
  - : '_3 list -> int
  - : '_18 list -> int
  - : ('_25 -> '_56) -> '_25 list -> '_56 list
  - : '_67 list -> '_67 list -> '_67 list
  - : '_81 list list -> '_81 list
  - : '_85 -> '_87 list -> unit
  - : '_98 list -> '_105 list -> ('_98, '_105) list
  - : int
