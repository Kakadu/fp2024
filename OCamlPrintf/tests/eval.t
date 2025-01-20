Copyright 2024-2025, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../repl/REPL.exe -fromfile manytests/typed/001fac.ml
  *** Printed ***
  24
  *** Output  ***
  val fac : int -> int = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/002fac.ml
  *** Printed ***
  24
  *** Output  ***
  val fac_cps : int -> (int -> 'a) -> 'a = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/003fib.ml
  *** Printed ***
  33
  *** Output  ***
  val fib_acc : int -> int -> int -> int = <fun>
  val fib : int -> int = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/004manyargs.ml
  *** Printed ***
  1111111111110100
  *** Output  ***
  val wrap : 'a -> 'a = <fun>
  val test3 : int -> int -> int -> int = <fun>
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/005fix.ml
  *** Printed ***
  720
  *** Output  ***
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  val fac : (int -> int) -> int -> int = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/006partial.ml
  *** Printed ***
  1122
  *** Output  ***
  val foo : int -> int = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/006partial2.ml
  *** Printed ***
  1237
  *** Output  ***
  val foo : int -> int -> int -> int = <fun>
  val main : int = 0
 
  $ ../repl/REPL.exe -fromfile manytests/typed/006partial3.ml
  *** Printed ***
  489
  *** Output  ***
  val foo : int -> int -> int -> unit = <fun>
  val main : int = 0

  $ ../repl/REPL.exe -fromfile manytests/typed/007order.ml
  *** Printed ***
  124-1103-55555510000
  *** Output  ***
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int = <fun>
  val main : unit = ()

  $ ../repl/REPL.exe -fromfile manytests/typed/008ascription.ml
  *** Printed ***
  8
  *** Output  ***
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int = <fun>
  val main : int = 0

  $ ../repl/REPL.exe -fromfile manytests/typed/009let_poly.ml
  *** Printed ***
  
  *** Output  ***
  val temp : int * bool = (1, true)

  $ ../repl/REPL.exe -fromfile manytests/typed/010sukharev.ml
  *** Printed ***
  
  *** Output  ***
  val _1 : int -> int -> int * 'a -> bool = <fun>
  val _2 : int = 1
  val _3 : (int * string) option = Some (1, "hi")
  val _4 : int -> 'a = <fun>
  val _5 : int = 42
  val _6 : 'a option -> 'a = <fun>
  val int_of_option : int option -> int = <function>
  val _42 : int -> bool = <function>
  val id1 : 'a -> 'a = <fun>
  val id2 : 'b -> 'b = <fun>

  $ ../repl/REPL.exe -fromfile manytests/typed/015tuples.ml
  *** Printed ***
  1111
  *** Output  ***
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  val map : ('b -> 'a) -> 'b * 'b -> 'a * 'a = <fun>
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b) = <fun>
  val feven : 'a * (int -> int) -> int -> int = <fun>
  val fodd : (int -> int) * 'a -> int -> int = <fun>
  val tie : (int -> int) * (int -> int) = (<fun>, <fun>)
  val meven : int -> int = <fun>
  val modd : int -> int = <fun>
  val main : int = 0

  $ ../repl/REPL.exe -fromfile manytests/typed/016lists.ml
  *** Printed ***
  1238
  *** Output  ***
  val length : 'a list -> int = <fun>
  val length_tail : 'a list -> int = <fun>
  val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
  val append : 'a list -> 'a list -> 'a list = <fun>
  val concat : 'a list list -> 'a list = <fun>
  val iter : ('a -> unit) -> 'a list -> unit = <fun>
  val cartesian : 'b list -> 'a list -> ('b * 'a) list = <fun>
  val main : int = 0
