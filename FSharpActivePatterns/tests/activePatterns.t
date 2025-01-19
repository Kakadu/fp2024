  $ ../bin/REPL.exe -fromfile activepatterns/choices.fs
  200
  val - : unit = () 
  val |A|B|C|D| : int * int -> Choice<A (int), B (int), C (int), D (unit)> = <fun> 

  $ ../bin/REPL.exe -fromfile activepatterns/simple.fs
  1
  val - : unit = () 
  val |MinusTwo|Not| : int -> Choice<MinusTwo (int), Not (int)> = <fun> 
