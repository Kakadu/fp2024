Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT
  $ ../lib/run_binding.exe -seed 67 -gen 1 -stop
  random seed: 67
  ================================================================================
  success (ran 1 tests)

  $ ../bin/REPL.exe manytests/do_not_type/001.hs
  Undefined variable 'fac'

  $ ../bin/REPL.exe manytests/do_not_type/002if.hs
  unification failed on Int and Bool

  $ ../bin/REPL.exe manytests/do_not_type/003occurs.hs
  Occurs check failed

  $ ../bin/REPL.exe manytests/do_not_type/004_let_poly.hs
  unification failed on Int and Bool

  $ ../bin/REPL.exe manytests/do_not_type/099.hs
  unification failed on Maybe t5 and Ord t8 -> Ord t8 -> Bool
  unification failed on () and t10 -> t10
  [ 
  x:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/001fac.hs
  [ 
  fac:  Int -> Int
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/002fac.hs
  [ 
  fac_cps: t11.  Int -> (Int -> t11) -> t11
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/003fib.hs
  [ 
  fib:  Int -> Int
  fib_acc:  Int -> Int -> Int -> Int
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/004manyargs.hs
  [ 
  main:  Int
  test10:  Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
  test3:  Int -> Int -> Int -> Int
  wrap: t1.  t1 -> t1
   ]

  $ ../bin/REPL.exe manytests/typed/005fix.hs
  [ 
  fac:  (Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/006partial.hs
  [ 
  foo:  Bool -> Int -> Int
  foo2:  Int -> Int
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/006partial2.hs
  [ 
  foo:  Int -> Int -> Int -> Int
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/006partial3.hs
  [ 
  foo:  Int -> Int -> Int -> ()
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/007order.hs
  [ 
  _start:  () -> () -> Int -> () -> Int -> Int -> () -> Int -> Int -> Int
  main:  ()
   ]

  $ ../bin/REPL.exe manytests/typed/008ascription.hs
  [ 
  addi: t4.  (t4 -> Bool -> Int) -> (t4 -> Bool) -> t4 -> Int
  main:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/009let_poly.hs
  [ 
  temp:  (Int, Bool)
   ]


  $ ../bin/REPL.exe manytests/typed/010sukharev.hs
  [ 
  _1: t5.  Int -> Int -> (Int, t5) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  _42: t47.  t47 -> Bool
  _5: t44.  Int -> t44
  int_of_option: t38.  Maybe t38 -> Int
   ]
  $ ../bin/REPL.exe manytests/typed/015tuples.hs
  [ 
  feven: t37.  (t37, Int -> Int) -> Int -> Int
  fix: t2. t5.  ((t2 -> t5) -> t2 -> t5) -> t2 -> t5
  fixpoly: t24. t27.  ((t24 -> t27, t24 -> t27) -> t24 -> t27, (t24 -> t27, t24 -> t27) -> t24 -> t27) -> (t24 -> t27, t24 -> t27)
  fodd: t49.  (Int -> Int, t49) -> Int -> Int
  main:  Int
  map: t12. t14.  (t12 -> t14) -> (t12, t12) -> (t14, t14)
  meven:  Int -> Int
  modd:  Int -> Int
  tie:  (Int -> Int, Int -> Int)
   ]

  $ ../bin/REPL.exe manytests/typed/016lists.hs
  [ 
  append: t72.  [t72] -> [t72] -> [t72]
  cartesian: t110. t117.  [t110] -> [t117] -> [(t110, t117)]
  concat: t94.  [[t94]] -> [t94]
  iter: t99.  (t99 -> ()) -> [t99] -> ()
  length: t3.  [t3] -> Int
  length_tail: t22.  [t22] -> Int
  main:  Int
  map: t28. t29.  (t28 -> t29) -> [t28] -> [t29]
   ]

  $ ../bin/REPL.exe <<-EOF
  >  fac0 self n = if n<2 then n else n* self (n-1) 
  >  fix f = f (fix f)
  >  fac = fix fac0
  >  main = print_int (fac 3)
  > EOF
  [ 
  fac0:  (Int -> Int) -> Int -> Int
   ]
  [ 
  fix: t11.  (t11 -> t11) -> t11
   ]
  [ 
  fac:  Int -> Int
   ]
  [ 
  main:  ()
   ]

# fibonacci
  $ ../bin/REPL.exe <<-EOF
  > iter f xs = case xs of [] -> (); h:tl -> let () = f h in iter f tl
  > take n xs = case xs of [] -> []; h:tl -> if n>0 then h : (take (n-1) tl) else []
  > tail xs = case xs of h:tl -> tl
  > zip_with f xs ys = case (xs,ys) of ([],[]) -> []; (h:tl, h2:tl2) -> (f h h2) : zip_with f tl tl2
  > fib = 0:1:(zip_with (+) fib (tail fib))
  > main = let () = iter print_int (take 10 fib) in 0
  > EOF
  [ 
  iter: t4.  (t4 -> ()) -> [t4] -> ()
   ]
  [ 
  take: t16.  Int -> [t16] -> [t16]
   ]
  [ 
  tail: t31.  [t31] -> [t31]
   ]
  [ 
  zip_with: t39. t40. t41.  (t39 -> t40 -> t41) -> [t39] -> [t40] -> [t41]
   ]
  [ 
  fib:  [Int]
   ]
  [ 
  main:  Int
   ]


# TODO(Kakadu): It would be great to call read GHCi somewhere in the tests
