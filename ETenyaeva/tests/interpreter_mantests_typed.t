  $ cat manytests/typed/001fac.ml | ../bin/REPL.exe
  24
  val fac = <fun>
  val main = 0

  $ cat manytests/typed/002fac.ml | ../bin/REPL.exe
  24
  val fac_cps = <fun>
  val main = 0

  $ cat manytests/typed/003fib.ml | ../bin/REPL.exe
  3
  3
  val fib_acc = <fun>
  val fib = <fun>
  val main = 0

  $ cat manytests/typed/004manyargs.ml | ../bin/REPL.exe
  1111111111
  1
  10
  100
  val wrap = <fun>
  val test3 = <fun>
  val test10 = <fun>
  val main = 0

  $ cat manytests/typed/005fix.ml | ../bin/REPL.exe
  720
  val fix = <fun>
  val fac = <fun>
  val main = 0

  $ cat manytests/typed/006partial.ml | ../bin/REPL.exe
  1122
  val foo = <fun>
  val main = 0

  $ cat manytests/typed/006partial2.ml | ../bin/REPL.exe
  1
  2
  3
  7
  val foo = <fun>
  val main = 0

  $ cat manytests/typed/006partial3.ml | ../bin/REPL.exe
  4
  8
  9
  val foo = <fun>
  val main = 0

  $ cat manytests/typed/007order.ml | ../bin/REPL.exe
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start = <fun>
  val main = ()

  $ cat manytests/typed/008ascription.ml | ../bin/REPL.exe
  8
  val addi = <fun>
  val main = 0

  $ cat manytests/typed/009let_poly.ml | ../bin/REPL.exe
  val temp = (1, true)

  $ cat manytests/typed/010sukharev.ml | ../bin/REPL.exe
  val _1 = <fun>
  val _2 = 1
  val _3 = Some (1, "hi")
  val _4 = <fun>
  val _5 = 42
  val _6 = <fun>
  val int_of_option = <function>
  val _42 = <function>
  val id1 = <fun>
  val id2 = <fun>

  $ cat manytests/typed/015tuples.ml | ../bin/REPL.exe
  1
  1
  1
  1
  val fix = <fun>
  val map = <fun>
  val fixpoly = <fun>
  val feven = <fun>
  val fodd = <fun>
  val tie = (<fun>, <fun>)
  val meven = <fun>
  val modd = <fun>
  val main = 0

  $ cat manytests/typed/016lists.ml | ../bin/REPL.exe
  1
  2
  3
  8
  val length = <fun>
  val length_tail = <fun>
  val map = <fun>
  val append = <fun>
  val concat = <fun>
  val iter = <fun>
  val cartesian = <fun>
  val main = 0
