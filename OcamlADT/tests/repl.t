(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*TODO: add inferencer*)

  $ ../bin/interpret.exe  manytests/typed/001fac.ml
  Running... 
  24
  val fac = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/002fac.ml
  Running... 

  $ ../bin/interpret.exe  manytests/typed/003fib.ml
  Running... 
  3
  3
  val fib_acc = <fun>
  val fib = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/004manyargs.ml
  Running... 
  1111111111
  1
  10
  100
  val wrap = <fun>
  val test3 = <fun>
  val test10 = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/005fix.ml
  Running... 
  720
  val fix = <fun>
  val fac = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/006partial.ml
  Running... 
  1122
  val foo = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/006partial2.ml
  Running... 
  1
  2
  3
  7
  val foo = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/006partial3.ml
  Running... 
  4
  8
  9
  val foo = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/007order.ml
  Running... 
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start = <fun>
  val main = ""

  $ ../bin/interpret.exe  manytests/typed/008ascription.ml
  Running... 
  8
  val addi = <fun>
  val main = 0

  $ ../bin/interpret.exe  manytests/typed/009let_poly.ml
  Running... 
  val temp = (1, true)

  $ ../bin/interpret.exe manytests/typed/010sukharev.ml
  File manytests/typed/010sukharev.ml not found
  [255]

  $ ../bin/interpret.exe  manytests/typed/015tuples.ml
  File manytests/typed/015tuples.ml not found
  [255]

  $ ../bin/interpret.exe manytests/typed/016lists.ml
  File manytests/typed/016lists.ml not found
  [255]
