Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT
  $ ../lib/run_binding.exe -seed 67 -gen 1 -stop
  random seed: 67
  ================================================================================
  success (ran 1 tests)

  $ cat << EOF | ../bin/REPL.exe
  > fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0  = 1 | True = y * save_fac (y - 1)
  > EOF
  Parsed: fac n = (if n < 0 then Nothing else Just (save_fac n)) where save_fac y | y == 0 = 1 | True = y * save_fac (y - 1)
  Result: [ 
  fac:  Int -> Maybe Int
   ]
