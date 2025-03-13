(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/main.exe --interpret manytests/typed/001fac.ml
  24

  $ ../bin/main.exe --interpret manytests/typed/002fac.ml
  24

  $ ../bin/main.exe --interpret manytests/typed/003fib.ml
  3
  3

  $ ../bin/main.exe --interpret manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100

  $ ../bin/main.exe --interpret manytests/typed/005fix.ml
  720

  $ ../bin/main.exe --interpret manytests/typed/006partial.ml
  1122

  $ ../bin/main.exe --interpret manytests/typed/006partial2.ml
  1
  2
  3
  7

  $ ../bin/main.exe --interpret manytests/typed/006partial3.ml
  4
  8
  9

  $ ../bin/main.exe --interpret manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000

  $ ../bin/main.exe --interpret manytests/typed/009let_poly.ml

  $ ../bin/main.exe --interpret manytests/typed/015tuples.ml
  1
  1
  1
  1

  $ ../bin/main.exe --infer manytests/typed/001fac.ml
  val fac : (int -> int)
  val main : int

  $ ../bin/main.exe --infer manytests/typed/002fac.ml
  val fac_cps : (int -> ((int -> 🍏) -> 🍏))
  val main : int

  $ ../bin/main.exe --infer manytests/typed/003fib.ml
  val fib : (int -> int)
  val fib_acc : (int -> (int -> (int -> int)))
  val main : int

  $ ../bin/main.exe --infer manytests/typed/004manyargs.ml
  val main : int
  val test10 : (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int))))))))))
  val test3 : (int -> (int -> (int -> int)))
  val wrap : (🍏 -> 🍏)

  $ ../bin/main.exe --infer manytests/typed/005fix.ml
  val fac : ((int -> int) -> (int -> int))
  val fix : (((🍏 -> 🍎) -> (🍏 -> 🍎)) -> (🍏 -> 🍎))
  val main : int

  $ ../bin/main.exe --infer manytests/typed/006partial.ml
  val foo : (int -> int)
  val main : int

  $ ../bin/main.exe --infer manytests/typed/006partial2.ml
  val foo : (int -> (int -> (int -> int)))
  val main : int

  $ ../bin/main.exe --infer manytests/typed/006partial3.ml
  val foo : (int -> (int -> (int -> unit)))
  val main : int

  $ ../bin/main.exe --infer manytests/typed/007order.ml
  val _start : (unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int)))))))))
  val main : unit

  $ ../bin/main.exe --infer manytests/typed/008ascription.ml
  val addi : ((🍏 -> (bool -> int)) -> ((🍏 -> bool) -> (🍏 -> int)))
  val main : int

  $ ../bin/main.exe --infer manytests/typed/009let_poly.ml
  val temp : (int * bool)

  $ ../bin/main.exe --infer manytests/typed/015tuples.ml
  val feven : ((🍏 * ((int -> int))) -> (int -> int))
  val fix : (((🍏 -> 🍎) -> (🍏 -> 🍎)) -> (🍏 -> 🍎))
  val fixpoly : (((((((🍏 -> 🍎)) * ((🍏 -> 🍎))) -> (🍏 -> 🍎))) * (((((🍏 -> 🍎)) * ((🍏 -> 🍎))) -> (🍏 -> 🍎)))) -> (((🍏 -> 🍎)) * ((🍏 -> 🍎))))
  val fodd : ((((int -> int)) * 🍏) -> (int -> int))
  val main : int
  val map : ((🍏 -> 🍎) -> ((🍏 * 🍏) -> (🍎 * 🍎)))
  val meven : (int -> int)
  val modd : (int -> int)
  val tie : (((int -> int)) * ((int -> int)))


  $ ../bin/main.exe --infer manytests/do_not_type/001.ml
  Type inference error: Undefined variable 'fac'

  $ ../bin/main.exe --infer manytests/do_not_type/002if.ml
  Type inference error: Unification failed on int and bool

  $ ../bin/main.exe --infer manytests/do_not_type/003occurs.ml
  Type inference error: Cannot construct type: 🍏 appears within (🍏 -> 🍎)

  $ ../bin/main.exe --infer manytests/do_not_type/004let_poly.ml
  Type inference error: Unification failed on bool and int

  $ ../bin/main.exe --infer manytests/do_not_type/015tuples.ml
  Type inference error: Invalid left hand side

  $ ../bin/main.exe --infer manytests/do_not_type/016tuples_mismatch.ml
  Type inference error: Unification failed on (🍏 * 🍎) and (int * int * int)

  $ ../bin/main.exe --infer manytests/do_not_type/097fun_vs_list.ml
  Type inference error: Unification failed on 🍏 list and (🍏 -> 🍏)

  $ ../bin/main.exe --infer manytests/do_not_type/097fun_vs_unit.ml
  Type inference error: Unification failed on unit and (🍏 -> 🍏)

  $ ../bin/main.exe --infer manytests/do_not_type/098rec_int.ml
  Type inference error: Invalid right hand side
