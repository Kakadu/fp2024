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
  val fac : (int -> int) = <fun>
  val main : int = 0
  
  
(* should unify 'a28 'a29 *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/002fac.ml
  24
  val fac_cps : (int -> ((int -> 'a28) -> 'a29)) = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/003fib.ml
  3
  3
  val fib : (int -> int) = <fun>
  val fib_acc : (int -> (int -> (int -> int))) = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100
  val main : int = 0
  val test10 : (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int)))))))))) = <fun>
  val test3 : (int -> (int -> (int -> int))) = <fun>
  val wrap : ('a4 -> 'a4) = <fun>
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/005fix.ml
  720
  val fac : ((int -> int) -> (int -> int)) = <fun>
  val fix : ((('a8 -> 'a16) -> ('a8 -> 'a16)) -> ('a8 -> 'a16)) = <fun>
  val main : int = 0
  
  
(* fix types *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial.ml
  Fatal error: exception Type.Unification.UnificationError("Cannot unify types: int and bool")
  Raised at Type.Unification.unify in file "lib/type/type.ml", line 190, characters 6-176
  Called from Type.Unification.unify in file "lib/type/type.ml", line 167, characters 15-26
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 424, characters 15-56
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 420, characters 29-54
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 331, characters 32-58
  Called from Type.Inference.infer_structure_item in file "lib/type/type.ml", line 584, characters 11-130
  Called from Type.Inference.infer_program.(fun) in file "lib/type/type.ml", line 642, characters 30-65
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Type.Inference.infer in file "lib/type/type.ml", line 662, characters 18-50
  Called from Dune__exe__Repl.run_single.run in file "bin/repl.ml", line 80, characters 23-32
  Called from Dune__exe__Repl.run_single in file "bin/repl.ml", line 107, characters 12-20
  [2]
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial2.ml
  1
  2
  3
  7
  val foo : (int -> (int -> (int -> int))) = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial3.ml
  4
  8
  9
  val foo : (int -> (int -> (int -> unit))) = <fun>
  val main : int = 0
  
  

  $ ../bin/repl.exe --no-hi --file manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start : (unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int))))))))) = <fun>
  val main : unit = ()
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/008ascription.ml
  8
  val addi : (('a9 -> (bool -> int)) -> (('a9 -> bool) -> ('a9 -> int))) = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/009let_poly.ml
  val temp : (int * bool) = (1, true)
  
  
(*  _2 should be int                    problem is in matching
(*  _4 should be int -> 'a
(*  _42 should be int -> bool
(*  int_of_option int option -> int     problem is in matching
(*  id1, id2 should be 'a -> 'a both    problem is in tuples

  $ ../bin/repl.exe --no-hi --file manytests/typed/010sukharev.ml
  val _1 : (int -> (int -> ((int * 'a14) -> bool))) = <fun>
  val _2 : (int * string option) = 1
  val _3 : (int * string) option = Some (1, "hi")
  val _4 : ('a29 -> 'a30) = <fun>
  val _42 : ('a70 -> bool) = <fun>
  val _5 : int = 42
  val _6 : ('a51 option -> 'a51) = <fun>
  val id1 : (('a78 -> 'a78) * ('a80 -> 'a80)) = <fun>
  val id2 : (('a78 -> 'a78) * ('a80 -> 'a80)) = <fun>
  val int_of_option : ('a62 option -> int) = <fun>
  
  

(* fix types *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/015tuples.ml
  Fatal error: exception Type.Unification.UnificationError("Cannot unify types: ((('a41 -> 'a44) * ('a41 -> 'a44)) -> ('a41 -> 'a44)) and (((int -> int) -> (int -> int)) * ((int -> int) -> (int -> int)))")
  Raised at Type.Unification.unify in file "lib/type/type.ml", line 190, characters 6-176
  Called from Type.Unification.unify in file "lib/type/type.ml", line 167, characters 15-26
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 424, characters 15-56
  Called from Type.Inference.infer_structure_item in file "lib/type/type.ml", line 557, characters 11-127
  Called from Type.Inference.infer_program.(fun) in file "lib/type/type.ml", line 642, characters 30-65
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Type.Inference.infer in file "lib/type/type.ml", line 662, characters 18-50
  Called from Dune__exe__Repl.run_single.run in file "bin/repl.ml", line 80, characters 23-32
  Called from Dune__exe__Repl.run_single in file "bin/repl.ml", line 107, characters 12-20
  [2]

(* fix types *)
  $ ../bin/repl.exe --do-not-type --no-hi --file manytests/typed/016lists.ml
  val append : <type> = <fun>
  val cartesian : <type> = <fun>
  val concat : <type> = <fun>
  val iter : <type> = <fun>
  val length : <type> = <fun>
  val length_tail : <type> = <fun>
  val main : <type> = 0
  val map : <type> = <fun>
  
  
