  $ ../bin/REPL.exe -fromfile manytests/do_not_type/001.ml
  Error occured: Undefined variable 'fac'
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/002if.ml
  Error occured: unification failed on int and bool
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/003occurs.ml
  Error occured: Occurs check failed
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/004let_poly.ml
  Error occured: unification failed on bool and int
  
  Error occured: unification failed on string and int
  
  Type checking failed: unification failed on string and int
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/015tuples.ml
  Error occured: Only variables are allowed as left-hand side of `let rec'
  Error occured: unification failed on '_2 * '_3 and int * int * int
  
  $ ../bin/REPL.exe -fromfile manytests/do_not_type/099.ml
  Error occured: Only variables are allowed as left-hand side of `let rec'
  Error occured: Unbound variable : x
  
  Error occured: unification failed on '_4 option and '_3 -> '_3
  
  Error occured: unification failed on unit and '_5 -> '_5
  
  $ ../bin/REPL.exe -fromfile manytests/typed/001fac.ml
  24
  val fac : int -> int = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/002fac.ml
  24
  val fac_cps : int -> (int -> '_7) -> '_7 = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/003fib.ml
  3
  3
  val fib : int -> int = <fun> 
  val fib_acc : int -> int -> int -> int = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100
  val main : int = 0 
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = <fun> 
  val test3 : int -> int -> int -> int = <fun> 
  val wrap : '_0 -> '_0 = <fun> 
  $ ../bin/REPL.exe -fromfile manytests/typed/005fix.ml
  720
  val fac : (int -> int) -> int -> int = <fun> 
  val fix : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5 = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial.ml
  1122
  val foo : int -> int = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial2.ml
  1
  2
  3
  7
  val foo : int -> int -> int -> int = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/006partial3.ml
  4
  8
  9
  val foo : int -> int -> int -> unit = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int = <fun> 
  val main : unit = () 
  $ ../bin/REPL.exe -fromfile manytests/typed/008ascription.ml
  8
  val addi : ('_0 -> bool -> int) -> ('_0 -> bool) -> '_0 -> int = <fun> 
  val main : int = 0 
  $ ../bin/REPL.exe -fromfile manytests/typed/009let_poly.ml
  val temp : int * bool = (1 , true ) 
  $ ../bin/REPL.exe -fromfile manytests/typed/010sukharev.ml
  val _1 : int -> int -> int * '_1 -> bool = <fun> 
  val _2 : int = 1 
  val _3 : (int * string) option = Some (1 , "hi" )  
  val _4 : int -> '_14 = <fun> 
  val _42 : int -> bool = <fun> 
  val _5 : int = 42 
  val _6 : '_30 option -> '_30 = <fun> 
  val id1 : '_44 -> '_44 = <fun> 
  val id2 : '_45 -> '_45 = <fun> 
  val int_of_option : int option -> int = <fun> 
  $ ../bin/REPL.exe -fromfile manytests/typed/015tuples.ml
  1
  1
  1
  1
  val feven : '_33 * (int -> int) -> int -> int = <fun> 
  val fix : (('_1 -> '_5) -> '_1 -> '_5) -> '_1 -> '_5 = <fun> 
  val fixpoly : (('_21 -> '_25) * ('_21 -> '_25) -> '_21 -> '_25) * (('_21 -> '_25) * ('_21 -> '_25) -> '_21 -> '_25) -> ('_21 -> '_25) * ('_21 -> '_25) = <fun> 
  val fodd : (int -> int) * '_41 -> int -> int = <fun> 
  val main : int = 0 
  val map : ('_9 -> '_11) -> '_9 * '_9 -> '_11 * '_11 = <fun> 
  val meven : int -> int = <fun> 
  val modd : int -> int = <fun> 
  val tie : (int -> int) * (int -> int) = (<fun> , <fun> ) 
  $ ../bin/REPL.exe -fromfile manytests/typed/016lists.ml
  3
  2
  1
  8
  val append : '_67 list -> '_67 list -> '_67 list = <fun> 
  val cartesian : '_98 list -> '_105 list -> '_98 * '_105 list = <fun> 
  val concat : '_81 list list -> '_81 list = <fun> 
  val iter : ('_87 -> unit) -> '_87 list -> unit = <fun> 
  val length : '_3 list -> int = <fun> 
  val length_tail : '_18 list -> int = <fun> 
  val main : int = 0 
  val map : ('_25 -> '_56) -> '_25 list -> '_56 list = <fun> 
