(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

  $ ../bin/main.exe -eval -file manytests/typed/001fac.ml
  24

  $ ../bin/main.exe -eval -file manytests/typed/002fac.ml
  24

  $ ../bin/main.exe -eval -file manytests/typed/003fib.ml
  3
  3

  $ ../bin/main.exe -eval -file manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100

  $ ../bin/main.exe -eval -file manytests/typed/005fix.ml
  720

  $ ../bin/main.exe -eval -file manytests/typed/006partial.ml
  1122


  $ ../bin/main.exe -eval -file manytests/typed/006partial2.ml
  1
  2
  3
  7


  $ ../bin/main.exe -eval -file manytests/typed/006partial3.ml
  4
  8
  9

  $ ../bin/main.exe -eval -file manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000


  $ ../bin/main.exe -eval -file manytests/typed/008ascription.ml
  8

  $ ../bin/main.exe -eval -file manytests/typed/009let_poly.ml

  $ ../bin/main.exe -eval -file manytests/typed/015tuples.ml
  1
  1
  1
  1

  $ ../bin/main.exe -infer -file manytests/typed/001fac.ml
  val fac: int -> int
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/002fac.ml
  val fac_cps: int -> (int -> int) -> int
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/003fib.ml
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/004manyargs.ml
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: '0 -> '0

  $ ../bin/main.exe -infer -file manytests/typed/005fix.ml
  val fac: (int -> int) -> int -> int
  val fix: ((int -> int) -> int -> int) -> int -> int
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/006partial.ml
  val foo: int -> int
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/006partial2.ml
  val foo: int -> int -> int -> int
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/006partial3.ml
  val foo: int -> int -> int -> unit
  val main: int

  $ ../bin/main.exe -infer -file manytests/typed/007order.ml
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit
 

  $ ../bin/main.exe -infer -file manytests/typed/008ascription.ml
  val addi: ('2 -> bool -> int) -> ('2 -> bool) -> '2 -> int
  val main: int
 
 

  $ ../bin/main.exe -infer -file manytests/typed/009let_poly.ml
  val temp: (int * bool)
 

  $ ../bin/main.exe -infer -file manytests/typed/015tuples.ml
  val feven: (int -> int * '28) -> int -> int
  val fix: ((((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)) -> ((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)) -> ((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)
  val fixpoly: ((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)
  val fodd: ('35 * int -> int) -> int -> int
  val main: int
  val map: ('9 -> '11) -> ('9 * '9) -> ('11 * '11)
  val meven: int -> int
  val modd: int -> int
  val tie: (int -> int * int -> int)
 


  $ ../bin/main.exe -infer -file manytests/do_not_type/001.ml
  Inferencing error: unbound variable: 'fac' is not defined in the current scope.

  $ ../bin/main.exe -infer -file manytests/do_not_type/002if.ml
  Inferencing error: type unification failed: cannot unify types int and bool.
 

  $ ../bin/main.exe -infer -file manytests/do_not_type/003occurs.ml
  Inferencing error: occurs check failed: variable '1' cannot appear in its own type '1 -> '3.
 

  $ ../bin/main.exe -infer -file manytests/do_not_type/004let_poly.ml
  Inferencing error: type unification failed: cannot unify types int and bool.
 
  $ ../bin/main.exe -infer -file manytests/do_not_type/015tuples.ml
  Inferencing error: illegal left-hand side: only variables can appear on the left-hand side of a 'let' binding.

  $ ../bin/main.exe -infer -file manytests/do_not_type/016tuples_mismatch.ml
  Inferencing error: type unification failed: cannot unify types ('1 * '0) and (int * int * int).

  $ ../bin/main.exe -infer -file manytests/do_not_type/097fun_vs_list.ml
  Inferencing error: type unification failed: cannot unify types '2 list and '0 -> '0.

  $ ../bin/main.exe -infer -file manytests/do_not_type/097fun_vs_unit.ml
  Inferencing error: type unification failed: cannot unify types unit and '0 -> '0.

  $ ../bin/main.exe -infer -file manytests/do_not_type/098rec_int.ml
  Inferencing error: illegal right-hand side: the right-hand side of a 'let' binding must be an expression, not a pattern.
