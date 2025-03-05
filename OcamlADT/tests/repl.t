(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/interpret.exe  manytests/typed/001fac.ml
  Running... 
  24
  val fac : int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/002fac.ml
  Running... 
  24
  val fac_cps : int -> (int -> 'a) -> 'a = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/003fib.ml
  Running... 
  3
  3
  val fib_acc : int -> int -> int -> int = <fun>
  val fib : int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/004manyargs.ml
  Running... 
  1111111111
  1
  10
  100
  val wrap : 'a -> 'a = <fun>
  val test3 : int -> int -> int -> int = <fun>
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/005fix.ml
  Running... 
  720
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  val fac : (int -> int) -> int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/006partial.ml
  Running... 
  1122
  val foo : int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/006partial2.ml
  Running... 
  1
  2
  3
  7
  val foo : int -> int -> int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/006partial3.ml
  Running... 
  4
  8
  9
  val foo : int -> int -> int -> unit = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/007order.ml
  Running... 
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int = <fun>
  val main : unit = ""

  $ ../bin/interpret.exe  manytests/typed/008ascription.ml
  Running... 
  8
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe  manytests/typed/009let_poly.ml
  Running... 
  val temp : int * bool = (1, true)

  $ ../bin/interpret.exe manytests/typed/010sukharev.ml
  Running... 
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

  $ ../bin/interpret.exe  manytests/typed/015tuples.ml
  Running... 
  1
  1
  1
  1
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b = <fun>
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b) = <fun>
  val feven : 'a * (int -> int) -> int -> int = <fun>
  val fodd : (int -> int) * 'a -> int -> int = <fun>
  val tie : (int -> int) * (int -> int) = (<fun>, <fun>)
  val meven : int -> int = <fun>
  val modd : int -> int = <fun>
  val main : int = 0

  $ ../bin/interpret.exe manytests/typed/016lists.ml
  Running... 
  1
  2
  3
  8
  val length : 'a list -> int = <fun>
  val length_tail : 'a list -> int = <fun>
  val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
  val append : 'a list -> 'a list -> 'a list = <fun>
  val concat : 'a list list -> 'a list = <fun>
  val iter : ('a -> unit) -> 'a list -> unit = <fun>
  val cartesian : 'a list -> 'b list -> ('a * 'b) list = <fun>
  val main : int = 0


  $ ../bin/interpret.exe manytests/do_not_type/001.ml
  Running... 
  Type error: Unbound_variable: "fac"

  $ ../bin/interpret.exe manytests/do_not_type/002if.ml
  Running... 
  Type error: Unification_failed: int # bool
  $ ../bin/interpret.exe manytests/do_not_type/003occurs.ml
  Running... 
  Type error: Occurs_check: 'c and 'c -> 'b
  
  $ ../bin/interpret.exe  manytests/do_not_type/004let_poly.ml
  Running... 
  Type error: Unification_failed: int # bool
  $ ../bin/interpret.exe manytests/do_not_type/015tuples.ml
  Running... 
  Type error: Wrong right value in rec
  $ ../bin/interpret.exe  manytests/do_not_type/099.ml    
  Running... 
  Type error: Wrong right value in rec
