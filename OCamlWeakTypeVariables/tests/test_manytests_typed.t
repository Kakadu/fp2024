  $ ../bin/REPL.exe -dinferprogram < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/003fib.ml
  val fib : int -> int
  val fib_acc : int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/004manyargs.ml
  val main : int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3 : int -> int -> int -> int
  val wrap : 'a -> 'a
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/005fix.ml
  val fac : (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial.ml
  val foo : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/009let_poly.ml
  val temp : int * bool
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/010sukharev.ml
  Error: : end_of_input

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/015tuples.ml
  val feven : 'a * (int -> int) -> int -> int
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val fodd : (int -> int) * 'a -> int -> int
  val main : int
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int) * (int -> int)
  
  $ ../bin/REPL.exe -dinferprogram < manytests/typed/016lists.ml
  Error: : end_of_input
