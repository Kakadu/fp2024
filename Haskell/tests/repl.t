Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT
  $ ../lib/run_binding.exe -seed 67 -gen 1 -stop
  random seed: 67
  ================================================================================
  success (ran 1 tests)

  $ ../bin/REPL.exe < manytests/do_not_type/001.hs
  Undefined variable 'fac'

  $ ../bin/REPL.exe < manytests/do_not_type/002if.hs
  unification failed on Int and Bool

  $ ../bin/REPL.exe < manytests/do_not_type/003occurs.hs
  Occurs check failed

  $ ../bin/REPL.exe < manytests/do_not_type/004_let_poly.hs
  unification failed on Int and Bool

  $ ../bin/REPL.exe < manytests/do_not_type/099.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/001fac.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/002fac.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/003fib.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/004manyargs.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/005fix.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/006partial.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/006partial2.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/006partial3.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/007order.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/008ascription.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/009let_poly.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/010sukharev.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/015tuples.hs
  Result: [
  fac:  Int -> Maybe Int
   ]

  $ ../bin/REPL.exe < manytests/typed/016lists.hs
  Result: [
  fac:  Int -> Maybe Int
   ]
