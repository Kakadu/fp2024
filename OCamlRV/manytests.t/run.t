Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan
SPDX-License-Identifier: LGPL-3.0-or-later

Test 'typed':

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

Test do_not_type:

  $ ../bin/main.exe do_not_type/001.ml -inference
  Infer error: Unbound variable 'fac'

  $ ../bin/main.exe do_not_type/002if.ml -inference
  Infer error: Unification failed on int and bool

  $ ../bin/main.exe do_not_type/003occurs.ml -inference
  Infer error: Occurs check failed

  $ ../bin/main.exe do_not_type/004let_poly.ml -inference
  Infer error: Unification failed on bool and int

  $ ../bin/main.exe do_not_type/005.ml -inference
  Infer error: Unification failed on int and string

  $ ../bin/main.exe do_not_type/015tuples.ml -inference
  Infer error: Only variables are allowed as left-hand side of `let rec'

  $ ../bin/main.exe do_not_type/016tuples_mismatch.ml -inference
  Infer error: Unification failed on '0 * '1 and int * int * int

  $ ../bin/main.exe do_not_type/097fun_vs_list.ml -inference
  Infer error: Unification failed on '0 list and '1 -> '1

  $ ../bin/main.exe do_not_type/097fun_vs_unit.ml -inference
  Infer error: Unification failed on unit and '0 -> '0

  $ ../bin/main.exe do_not_type/098rec_int.ml -inference
  Infer error: This kind of expression is not allowed as right-hand side of `let rec'

  $ ../bin/main.exe do_not_type/099.ml -inference
  Infer error: Only variables are allowed as left-hand side of `let rec'
