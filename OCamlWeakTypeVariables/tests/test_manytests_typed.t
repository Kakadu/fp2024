  $ ../bin/REPL.exe -dinferprogram < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/004manyargs.ml
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/005fix.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial.ml
  val foo : int -> int
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
  
  $ cat manytests/typed/010sukharev.ml | ../bin/REPL.exe -dinferprogram
  val _1 : int -> int -> int * 'a -> bool
  val _2 : int
  val _3 : (int * string) option
  val _4 : int -> 'a
  val _5 : int
  val _6 : 'a option -> 'a
  val int_of_option : int option -> int
  val _42 : int -> bool
  val id1 : 'a -> 'a
  val id2 : 'a -> 'a
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/015tuples.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val modd : int -> int
  val meven : int -> int
  val main : int
  
  $ ../bin/REPL.exe -dinferprogram < manytests/typed/016lists.ml
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : ('a list) list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val main : int
  
