(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(************************** do_not_type **************************)

  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/001.ml
  val fac : <type> = <fun>
  
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/002if.ml
  val main : <type> = 1
  
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/003occurs.ml
  val fix : <type> = <fun>
  
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/004let_poly.ml
  val _1 : <type> = (1, true)
  
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/005.ml
  val _2 : <type> = <fun>
  
  
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/015tuples.ml
  
  Interpreter error: Type mismatch
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/016tuples_mismatch.ml
  
  Interpreter error: Match failure
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/097fun_vs_list.ml
  
  Interpreter error: Match failure
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/097fun_vs_unit.ml
  
  Interpreter error: Match failure
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/098rec_int.ml
  
  Interpreter error: Unbound identificator: x
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/do_not_type/099.ml
  
  Interpreter error: Type mismatch

(************************** typed **************************)

  $ ../bin/repl.exe --no-hi --file manytests/typed/001fac.ml
  24
  val fac : int -> int = <fun>
  val main : int = 0
  
  

  $ ../bin/repl.exe --no-hi --file manytests/typed/002fac.ml
  24
  val fac_cps : int -> (int -> 'a8) -> 'a8 = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/003fib.ml
  3
  3
  val fib : int -> int = <fun>
  val fib_acc : int -> int -> int -> int = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100
  val main : int = 0
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = <fun>
  val test3 : int -> int -> int -> int = <fun>
  val wrap : 'a0 -> 'a0 = <fun>
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/005fix.ml
  720
  val fac : (int -> int) -> int -> int = <fun>
  val fix : (('a2 -> 'a5) -> 'a2 -> 'a5) -> 'a2 -> 'a5 = <fun>
  val main : int = 0
  
  

  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial.ml
  1122
  val foo : int -> int = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial2.ml
  1
  2
  3
  7
  val foo : int -> int -> int -> int = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial3.ml
  4
  8
  9
  val foo : int -> int -> int -> unit = <fun>
  val main : int = 0
  
  

  $ ../bin/repl.exe --no-hi --file manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int = <fun>
  val main : unit = ()
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/008ascription.ml
  8
  val addi : ('a2 -> bool -> int) -> ('a2 -> bool) -> 'a2 -> int = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/009let_poly.ml
  val temp : int * bool = (1, true)
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/010sukharev.ml
  val _1 : int -> int -> int * 'a3 -> bool = <fun>
  val _2 : int = 1
  val _3 : (int * string) option = Some (1, "hi")
  val _4 : int -> 'a13 = <fun>
  val _42 : int -> bool = <fun>
  val _5 : int = 42
  val _6 : 'a28 option -> 'a28 = <fun>
  val id1 : 'a43 -> 'a43 = <fun>
  val id2 : 'a44 -> 'a44 = <fun>
  val int_of_option : int option -> int = <fun>
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/015tuples.ml
  1
  1
  1
  1
  val feven : 'a32 * int -> int -> int -> int = <fun>
  val fix : (('a2 -> 'a5) -> 'a2 -> 'a5) -> 'a2 -> 'a5 = <fun>
  val fixpoly : 'a21 -> 'a24 * 'a21 -> 'a24 -> 'a21 -> 'a24 * 'a21 -> 'a24 * 'a21 -> 'a24 -> 'a21 -> 'a24 -> 'a21 -> 'a24 * 'a21 -> 'a24 = <fun>
  val fodd : int -> int * 'a40 -> int -> int = <fun>
  val main : int = 0
  val map : ('a9 -> 'a11) -> 'a9 * 'a9 -> 'a11 * 'a11 = <fun>
  val meven : int -> int = <fun>
  val modd : int -> int = <fun>
  val tie : int -> int * int -> int = (<fun>, <fun>)
  
  

  $ ../bin/repl.exe --no-hi --file manytests/typed/016lists.ml
  val append : 'a64 list -> 'a64 list -> 'a64 list = <fun>
  val cartesian : 'a92 list -> 'a99 list -> ('a92 * 'a99) list = <fun>
  val concat : 'a76 list list -> 'a76 list = <fun>
  val iter : ('a82 -> unit) -> 'a82 list -> unit = <fun>
  val length : 'a3 list -> int = <fun>
  val length_tail : 'a16 list -> int = <fun>
  val main : int = 0
  val map : ('a23 -> 'a24) -> 'a23 list -> 'a24 list = <fun>
  
  
