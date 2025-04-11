(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

Test '001fac':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/001fac.ml
  24
  {
  val fac : int -> int = <fun>
  val main : int = 0
  }

Test '002fac':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/002fac.ml
  24
  {
  val fac_cps : int -> ((int -> int) -> int) = <fun>
  val main : int = 0
  }

Test '003fib':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/003fib.ml
  33
  {
  val fib : int -> int = <fun>
  val fib_acc : int -> (int -> (int -> int)) = <fun>
  val main : int = 0
  }

Test '004manyargs':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/004manyargs.ml
  1111111111110100
  {
  val main : int = 0
  val test10 : int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int))))))))) = <fun>
  val test3 : int -> (int -> (int -> int)) = <fun>
  val wrap : '0 -> '0 = <fun>
  }

Test '005fix':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/005fix.ml
  720
  {
  val fac : (int -> int) -> (int -> int) = <fun>
  val fix : ((int -> int) -> (int -> int)) -> (int -> int) = <fun>
  val main : int = 0
  }

Test '006partial':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/006partial.ml
  1122
  {
  val foo : int -> int = <fun>
  val main : int = 0
  }

Test '006partial2':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/006partial2.ml
  1237
  {
  val foo : int -> (int -> (int -> int)) = <fun>
  val main : int = 0
  }

Test '006partial3':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/006partial3.ml
  489
  {
  val foo : int -> (int -> (int -> unit)) = <fun>
  val main : int = 0
  }

Test '007order':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/007order.ml
  124-1103-55555510000
  {
  val _start : unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int)))))))) = <fun>
  val main : unit = ()
  }

Test '008ascription':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/008ascription.ml
  8
  {
  val addi : ('2 -> (bool -> int)) -> (('2 -> bool) -> ('2 -> int)) = <fun>
  val main : int = 0
  }

Test '009letpoly':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/009let_poly.ml  
  
  {
  val temp : (int * bool) = (1, true)
  }

Test '010':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/010sukharev.ml
  
  {
  val _1 : int -> (int -> ((int * '3) -> bool)) = <fun>
  val _2 : int = 1
  val _3 : ((int * string)) option = Some (1, "hi")
  val _4 : int -> '10 = <fun>
  val _42 : int -> bool = <fun>
  val _5 : int = 42
  val _6 : ('23) option -> '23 = <fun>
  val id1 : '32 -> '32 = <fun>
  val id2 : '33 -> '33 = <fun>
  val int_of_option : (int) option -> int = <fun>
  }

Test '015tuples':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/015tuples.ml
  1111
  {
  val feven : ('29 * int -> int) -> (int -> int) = <fun>
  val fix : ((((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int))) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)) = <fun>
  val fixpoly : ((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int) = <fun>
  val fodd : (int -> int * '36) -> (int -> int) = <fun>
  val main : int = 0
  val map : ('9 -> '11) -> (('9 * '9) -> ('10 * '11)) = <fun>
  val meven : int -> int = <fun>
  val modd : int -> int = <fun>
  val tie : (int -> int * int -> int) = (<fun>, <fun>)
  }

Test '016lists':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/typed/016lists.ml
  1238
  {
  val append : (int * int) list -> ((int * int) list -> (int * int) list) = <fun>
  val cartesian : int list -> (int list -> (int * int) list) = <fun>
  val concat : (int * int) list list -> (int * int) list = <fun>
  val iter : (int -> unit) -> (int list -> unit) = <fun>
  val length : (int * int) list -> int = <fun>
  val length_tail : '16 list -> int = <fun>
  val main : int = 0
  val map : (int -> (int * int)) -> (int list -> (int * int) list) = <fun>
  }

Test '001':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/001.ml
  Infer error: Undefined variable "fac"

Test '002if':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/002if.ml
  Infer error: Unification failed on int and bool

Test '003occurs':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/003occurs.ml
  Infer error: Occurs check failed: type variable 1 inside type '1 -> '3

Test '004let_poly':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/004let_poly.ml
  Infer error: Unification failed on int and bool

Test '015tuples':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/015tuples.ml
  Infer error: Ill left-hand side : only variables are allowed

Test '016tuples_mismatch':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/016tuples_mismatch.ml
  Infer error: Unification failed on ('0 * '1) and (int * int * int)

Test '097fun_vs_list':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/097fun_vs_list.ml
  Infer error: Unification failed on '2 list and '0 -> '0

Test '097fun_vs_unit':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/097fun_vs_unit.ml
  Infer error: Unification failed on unit and '0 -> '0

Test '098rec_int':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/098rec_int.ml
  Infer error: Ill right-hand side of let rec

Test '099':

  $ dune exec ../repl/repl.exe -- -interpret -fromfile ../../../../../../manytests/do_not_type/099.ml
  Infer error: Ill left-hand side : only variables are allowed


