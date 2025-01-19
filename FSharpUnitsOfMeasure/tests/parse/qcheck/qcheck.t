  $ ./qCheckRun.exe -seed 98645678 -gen 2 -stop
  random seed: 98645678
  
  --- Failure --------------------------------------------------------------------
  
  Test anon_test_1 failed (19 shrink steps):
  
  (match a with 0. -> -1312004488025042530 | _ -> "", match a with a -> a | a -> a, a);;
  
  ================================================================================
  failure (1 tests failed, 0 tests errored, ran 1 tests)
