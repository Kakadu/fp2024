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
  [ 
  print_int:  Int -> ()
  x:  Int
   ]
  unification failed on Maybe t1 and Ord t4 -> Ord t4 -> Bool
  unification failed on () and t0 -> t0

  $ ../bin/REPL.exe < manytests/typed/001fac.hs
  [ 
  fac:  Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  fac:  Int -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/002fac.hs
  [ 
  fac_cps:  Int -> (Int -> t11) -> t11
  print_int:  Int -> ()
   ]
  [ 
  fac_cps:  Int -> (Int -> Int) -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/003fib.hs
  [ 
  fib_acc:  Int -> Int -> Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  fib:  Int -> Int
  fib_acc:  Int -> Int -> Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  fib:  Int -> Int
  fib_acc:  Int -> Int -> Int -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/004manyargs.hs
  [ 
  print_int:  Int -> ()
  wrap:  t1 -> t1
   ]
  [ 
  print_int:  Int -> ()
  test3:  Int -> Int -> Int -> Int
  wrap:  Int -> Int
   ]
  [ 
  print_int:  Int -> ()
  test10:  Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
  test3:  Int -> Int -> Int -> Int
  wrap:  Int -> Int
   ]

  $ ../bin/REPL.exe < manytests/typed/005fix.hs
  [ 
  fix:  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  print_int:  Int -> ()
   ]
  [ 
  fac:  (Int -> Int) -> Int -> Int
  fix:  ((Int -> Int) -> Int -> Int) -> Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  fac:  (Int -> Int) -> Int -> Int
  fix:  ((Int -> Int) -> Int -> Int) -> Int -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/006partial.hs
  [ 
  foo:  Bool -> Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  foo:  Bool -> Int -> Int
  foo2:  Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  foo:  Bool -> Int -> Int
  foo2:  Int -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/006partial2.hs
  [ 
  foo:  Int -> Int -> Int -> Int
  print_int:  Int -> ()
   ]
  [ 
  foo:  Int -> Int -> Int -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/006partial3.hs
  [ 
  foo:  Int -> Int -> Int -> ()
  print_int:  Int -> ()
   ]
  [ 
  foo:  Int -> Int -> Int -> ()
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/007order.hs
  [ 
  _start:  () -> () -> Int -> () -> Int -> Int -> () -> Int -> Int -> Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/008ascription.hs
  [ 
  addi:  (t4 -> Bool -> Int) -> (t4 -> Bool) -> t4 -> Int
  print_int:  Int -> ()
   ]
  [ 
  addi:  (Int -> Bool -> Int) -> (Int -> Bool) -> Int -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/009let_poly.hs

  $ ../bin/REPL.exe < manytests/typed/010sukharev.hs
  [ 
  _1:  Int -> Int -> (Int, t5) -> Bool
  print_int:  Int -> ()
   ]
  [ 
  _1:  Int -> Int -> (Int, Int -> Int) -> Bool
  _2:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/015tuples.hs
  [ 
  fix:  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  print_int:  Int -> ()
   ]
  [ 
  fix:  (((t6, t6) -> t6) -> (t6, t6) -> t6) -> (t6, t6) -> t6
  map:  (t6 -> t8) -> (t6, t6) -> (t8, t8)
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/016lists.hs
  [ 
  length:  [t3] -> Int
  print_int:  Int -> ()
   ]
  [ 
  length:  [Int] -> Int
  length_tail:  [t6] -> Int
  print_int:  Int -> ()
   ]
  [ 
  length:  [Int] -> Int
  length_tail:  [t4] -> Int
  map:  (t4 -> t5) -> [t4] -> [t5]
  print_int:  Int -> ()
   ]
  [ 
  append:  [t4] -> [t4] -> [t4]
  length:  [Int] -> Int
  length_tail:  [t4] -> Int
  map:  (t4 -> t4) -> [t4] -> [t4]
  print_int:  Int -> ()
   ]
