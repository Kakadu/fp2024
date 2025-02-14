  $ ../bin/REPL.exe -fromfile interpreter_tests/rev_list.fs
  val - : int list = [3 ; 2 ; 1 ] 
  val rev_list : '13 list -> '13 list = <fun> 

  $ ../bin/REPL.exe -fromfile interpreter_tests/fmap.fs
  val a : int option = None 
  val b : int option = Some 24  
  val fmap : ('3 -> '6) -> '3 option -> '6 option = <fun> 
