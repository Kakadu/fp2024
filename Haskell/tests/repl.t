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
  unification failed on Maybe t5 and Ord t8 -> Ord t8 -> Bool
  unification failed on () and t10 -> t10

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
  fac_cps: t11.  Int -> (Int -> t11) -> t11
  print_int:  Int -> ()
   ]
  [ 
  fac_cps: t11.  Int -> (Int -> t11) -> t11
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
  wrap: t1.  t1 -> t1
   ]
  [ 
  print_int:  Int -> ()
  test3:  Int -> Int -> Int -> Int
  wrap: t1.  t1 -> t1
   ]
  [ 
  print_int:  Int -> ()
  test10:  Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
  test3:  Int -> Int -> Int -> Int
  wrap: t1.  t1 -> t1
   ]
  [ 
  main:  Int
  print_int:  Int -> ()
  test10:  Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
  test3:  Int -> Int -> Int -> Int
  wrap: t1.  t1 -> t1
   ]

  $ ../bin/REPL.exe < manytests/typed/005fix.hs
  [ 
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  print_int:  Int -> ()
   ]
  [ 
  fac:  (Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  print_int:  Int -> ()
   ]
  [ 
  fac:  (Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
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
  [ 
  _start:  () -> () -> Int -> () -> Int -> Int -> () -> Int -> Int -> Int
  main:  ()
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/008ascription.hs
  [ 
  addi: t4.  (t4 -> Bool -> Int) -> (t4 -> Bool) -> t4 -> Int
  print_int:  Int -> ()
   ]
  [ 
  addi: t4.  (t4 -> Bool -> Int) -> (t4 -> Bool) -> t4 -> Int
  main:  Int
  print_int:  Int -> ()
   ]

  $ ../bin/REPL.exe < manytests/typed/009let_poly.hs
  [ 
  print_int:  Int -> ()
  temp:  (Int, Bool)
   ]


  $ ../bin/REPL.exe < manytests/typed/010sukharev.hs
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  int_of_option: t36.  Maybe t36 -> t36
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  int_of_option: t38.  Maybe t38 -> Int
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  _5: t44.  Int -> t44
  int_of_option: t38.  Maybe t38 -> Int
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  _42:  Int -> Bool
  _5: t44.  Int -> t44
  int_of_option: t38.  Maybe t38 -> Int
  print_int:  Int -> ()
   ]
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  _42: t47.  t47 -> Bool
  _5: t44.  Int -> t44
  int_of_option: t38.  Maybe t38 -> Int
  print_int:  Int -> ()
   ]
  $ ../bin/REPL.exe < manytests/typed/015tuples.hs
  [ 
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  print_int:  Int -> ()
   ]
  [ 
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  print_int:  Int -> ()
   ]
  [ 
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  print_int:  Int -> ()
   ]
  [ 
  feven: t37.  (t37, Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  print_int:  Int -> ()
   ]
  [ 
  feven: t37.  (t37, Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  fodd: t49.  (Int -> Int, t49) -> Int -> Int
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  print_int:  Int -> ()
   ]
  [ 
  feven: t37.  (t37, Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  fodd: t49.  (Int -> Int, t49) -> Int -> Int
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  print_int:  Int -> ()
  tie:  (Int -> Int, Int -> Int)
   ]
  [ 
  feven: t37.  (t37, Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  fodd: t49.  (Int -> Int, t49) -> Int -> Int
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  meven:  Int -> Int
  modd:  Int -> Int
  print_int:  Int -> ()
  tie:  (Int -> Int, Int -> Int)
   ]
  [ 
  feven: t37.  (t37, Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  fodd: t49.  (Int -> Int, t49) -> Int -> Int
  main:  Int
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  meven:  Int -> Int
  modd:  Int -> Int
  print_int:  Int -> ()
  tie:  (Int -> Int, Int -> Int)
   ]

  $ ../bin/REPL.exe < manytests/typed/016lists.hs
  [ 
  length: t3.  [t3] -> Int
  print_int:  Int -> ()
   ]
  [ 
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  print_int:  Int -> ()
   ]
  [ 
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
  print_int:  Int -> ()
   ]
  [ 
  append: t72.  [t72] -> [t72] -> [t72]
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
  print_int:  Int -> ()
   ]
  [ 
  append: t72.  [t72] -> [t72] -> [t72]
  concat: t94.  [[t94]] -> [t94]
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
  print_int:  Int -> ()
   ]
  [ 
  append: t72.  [t72] -> [t72] -> [t72]
  concat: t94.  [[t94]] -> [t94]
  iter: t99.  (t99 -> ()) -> [t99] -> ()
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
  print_int:  Int -> ()
   ]
  [ 
  append: t72.  [t72] -> [t72] -> [t72]
  cartesian: t110. t117.  [t110] -> [t117] -> [(t110, t117)]
  concat: t94.  [[t94]] -> [t94]
  iter: t99.  (t99 -> ()) -> [t99] -> ()
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
  print_int:  Int -> ()
   ]
  [ 
  append: t72.  [t72] -> [t72] -> [t72]
  cartesian: t110. t117.  [t110] -> [t117] -> [(t110, t117)]
  concat: t94.  [[t94]] -> [t94]
  iter: t99.  (t99 -> ()) -> [t99] -> ()
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  main:  Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
  print_int:  Int -> ()
   ]
