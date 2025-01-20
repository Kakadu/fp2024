Copyright 2024-2025, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../repl/REPL.exe -inference -fromfile factorial.txt
  val factorial : int -> int

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/001.ml
  Inferencer error: Undefined variable 'fac'

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/002if.ml
  Inferencer error: Unification failed on int and bool

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/003occurs.ml
  Inferencer error: Occurs check failed: the type variable 'ty1 occurs inside 'ty1 -> 'ty3

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/004let_poly.ml
  Inferencer error: Unification failed on int and bool

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/005.ml
  Inferencer error: Unification failed on string and int

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/015tuples.ml
  Inferencer error: Only variables are allowed as left-hand side of `let rec'

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/016tuples_mismatch.ml
  Inferencer error: Unification failed on int * int * int and 'ty0 * 'ty1

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/097fun_vs_list.ml
  Inferencer error: Unification failed on 'ty0 -> 'ty0 and 'ty1 list

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/097fun_vs_unit.ml
  Inferencer error: Unification failed on 'ty0 -> 'ty0 and unit

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/098rec_int.ml
  Inferencer error: This kind of expression is not allowed as right-hand side of `let rec'

  $ ../repl/REPL.exe -inference -fromfile manytests/do_not_type/099.ml
  Inferencer error: Only variables are allowed as left-hand side of `let rec'

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/004manyargs.ml
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/005fix.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/010sukharev.ml
  val _1 : int -> int -> int * 'a -> bool
  val _2 : int
  val _3 : (int * string) option
  val _4 : int -> 'a
  val _5 : int
  val _6 : 'a option -> 'a
  val int_of_option : int option -> int
  val _42 : int -> bool
  val id1 : 'a -> 'a
  val id2 : 'b -> 'b

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/015tuples.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('b -> 'a) -> 'b * 'b -> 'a * 'a
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ../repl/REPL.exe -inference -fromfile manytests/typed/016lists.ml
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'b list -> 'a list -> ('b * 'a) list
  val main : int
