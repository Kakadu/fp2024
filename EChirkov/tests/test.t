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
