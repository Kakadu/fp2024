  $ cat manytests/typed/001fac.ml | ../bin/REPL.exe --dinference
  val fac : int -> int
  val main : int

  $ cat manytests/typed/002fac.ml | ../bin/REPL.exe --dinference
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ cat manytests/typed/003fib.ml | ../bin/REPL.exe --dinference
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ cat manytests/typed/004manyargs.ml | ../bin/REPL.exe --dinference
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ cat manytests/typed/005fix.ml | ../bin/REPL.exe --dinference
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ cat manytests/typed/006partial.ml | ../bin/REPL.exe --dinference
  val foo : int -> int
  val main : int

  $ cat manytests/typed/006partial2.ml | ../bin/REPL.exe --dinference
  val foo : int -> int -> int -> int
  val main : int

  $ cat manytests/typed/006partial3.ml | ../bin/REPL.exe --dinference
  val foo : int -> int -> int -> unit
  val main : int

  $ cat manytests/typed/007order.ml | ../bin/REPL.exe --dinference
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ cat manytests/typed/008ascription.ml | ../bin/REPL.exe --dinference
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ cat manytests/typed/009let_poly.ml | ../bin/REPL.exe --dinference
  val temp : int * bool

  $ cat manytests/typed/010sukharev.ml | ../bin/REPL.exe --dinference
  val _1 : int -> int -> int * 'a -> bool
  val _2 : int
  val _3 : (int * string) option
  val _4 : int -> 'a
  val _5 : int
  val _6 : 'a option -> 'a
  val int_of_option : int option -> int
  val _42 : int -> bool
  val id1 : 'a -> 'a
  val id2 : 'b -> 'b

  $ cat manytests/typed/015tuples.ml | ../bin/REPL.exe --dinference
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('b -> 'a) -> 'b * 'b -> 'a * 'a
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ cat manytests/typed/016lists.ml | ../bin/REPL.exe --dinference
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'b list -> 'b list
  val concat : ('a list) list -> 'b list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'a list -> 'c list -> 'b list
  val main : int
