(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

Test '001fac':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

Test '002fac':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/002fac.ml
  val fac_cps : int -> ((int -> int) -> int)
  val main : int

Test '003fib':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> (int -> (int -> int))
  val main : int

Test '004manyargs':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int)))))))))
  val test3 : int -> (int -> (int -> int))
  val wrap : '0 -> '0

Test '005fix':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/005fix.ml
  val fac : (int -> int) -> (int -> int)
  val fix : ((int -> int) -> (int -> int)) -> (int -> int)
  val main : int

Test '006partial':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

Test '006partial2':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/006partial2.ml
  val foo : int -> (int -> (int -> int))
  val main : int

Test '006partial3':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/006partial3.ml
  val foo : int -> (int -> (int -> unit))
  val main : int

Test '007order':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/007order.ml
  val _start : unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int))))))))
  val main : unit

Test '008ascription':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/008ascription.ml
  val addi : ('2 -> (bool -> int)) -> (('2 -> bool) -> ('2 -> int))
  val main : int

Test '009letpoly':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/009let_poly.ml  
  val temp : (int * bool)

Test '010':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/010sukharev.ml
  val _1 : int -> (int -> ((int * '3) -> bool))
  val _2 : int
  val _3 : ((int * string)) option
  val _4 : int -> '10
  val _42 : int -> bool
  val _5 : int
  val _6 : ('23) option -> '23
  val id1 : '32 -> '32
  val id2 : '33 -> '33
  val int_of_option : (int) option -> int

Test '015tuples':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/015tuples.ml
  val feven : ('29 * int -> int) -> (int -> int)
  val fix : ((((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int))) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int))
  val fixpoly : ((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)
  val fodd : (int -> int * '36) -> (int -> int)
  val main : int
  val map : ('9 -> '11) -> (('9 * '9) -> ('10 * '11))
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int * int -> int)

Test '016lists':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/typed/016lists.ml
  val append : (int * int) list -> ((int * int) list -> (int * int) list)
  val cartesian : int list -> (int list -> (int * int) list)
  val concat : (int * int) list list -> (int * int) list
  val iter : (int -> unit) -> (int list -> unit)
  val length : (int * int) list -> int
  val length_tail : '16 list -> int
  val main : int
  val map : (int -> (int * int)) -> (int list -> (int * int) list)

Test '001':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/001.ml
  Infer error: Undefined variable "fac"

Test '002if':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/002if.ml
  Infer error: Unification failed on int and bool

Test '003occurs':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/003occurs.ml
  Infer error: Occurs check failed: type variable 1 inside type '1 -> '3

Test '004let_poly':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/004let_poly.ml
  Infer error: Unification failed on int and bool

Test '015tuples':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/015tuples.ml
  Infer error: Ill left-hand side : only variables are allowed

Test '016tuples_mismatch':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/016tuples_mismatch.ml
  Infer error: Unification failed on ('0 * '1) and (int * int * int)

Test '097fun_vs_list':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/097fun_vs_list.ml
  Infer error: Unification failed on '2 list and '0 -> '0

Test '097fun_vs_unit':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/097fun_vs_unit.ml
  Infer error: Unification failed on unit and '0 -> '0

Test '098rec_int':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/098rec_int.ml
  Infer error: Ill right-hand side of let rec

Test '099':

  $ dune exec ../repl/repl.exe -- -inference -fromfile ../../../../../../manytests/do_not_type/099.ml
  Infer error: Ill left-hand side : only variables are allowed


