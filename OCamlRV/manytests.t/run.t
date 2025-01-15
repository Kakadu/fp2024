Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/main.exe typed/001fac.ml -interpret
  24

  $ ../bin/main.exe typed/002fac.ml -interpret
  24

  $ ../bin/main.exe typed/003fib.ml -interpret
  33

  $ ../bin/main.exe typed/004manyargs.ml -interpret
  1111111111110100

  $ ../bin/main.exe typed/005fix.ml -interpret
  720

  $ ../bin/main.exe typed/006partial.ml -interpret
  1122

  $ ../bin/main.exe typed/006partial2.ml -interpret
  1237

  $ ../bin/main.exe typed/006partial3.ml -interpret
  489

Неопределённый порядок аргументов?..
  $ ../bin/main.exe typed/007order.ml -interpret
  124-1103-55555510000

  $ ../bin/main.exe typed/008ascription.ml -interpret
  8

  $ ../bin/main.exe typed/009let_poly.ml -interpret


  $ ../bin/main.exe typed/010sukharev.ml -interpret

  $ ../bin/main.exe typed/015tuples.ml -interpret
  1111

  $ ../bin/main.exe typed/016lists.ml -interpret
  1238
