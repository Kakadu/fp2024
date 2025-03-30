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
  
  
(* should unify 'a28 'a29 *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/002fac.ml
  24
  val fac_cps : int -> (int -> 'a28) -> 'a29 = <fun>
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
  val wrap : 'a4 -> 'a4 = <fun>
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/005fix.ml
  720
  val fac : (int -> int) -> int -> int = <fun>
  val fix : ('a8 -> 'a16 -> 'a8 -> 'a16) -> 'a8 -> 'a16 = <fun>
  val main : int = 0
  
  
(* error probably because of name shadowing *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/006partial.ml
  Fatal error: exception Type.Unification.UnificationError("Cannot unify types: int and bool")
  Raised at Type.Unification.unify in file "lib/type/type.ml", line 185, characters 6-176
  Called from Type.Unification.unify in file "lib/type/type.ml", line 166, characters 15-26
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 415, characters 15-56
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 411, characters 29-54
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 326, characters 32-58
  Called from Type.Inference.infer_structure_item in file "lib/type/type.ml", line 567, characters 11-130
  Called from Type.Inference.infer_program.(fun) in file "lib/type/type.ml", line 625, characters 30-65
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Type.Inference.infer in file "lib/type/type.ml", line 640, characters 18-50
  Called from Dune__exe__Repl.run_single.run in file "bin/repl.ml", line 82, characters 22-31
  Called from Dune__exe__Repl.run_single in file "bin/repl.ml", line 111, characters 12-20
  [2]
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
  val addi : ('a9 -> bool -> int) -> ('a9 -> bool) -> 'a9 -> int = <fun>
  val main : int = 0
  
  
  $ ../bin/repl.exe --no-hi --file manytests/typed/009let_poly.ml
  val temp : (int * bool) = (1, true)
  
  
(* fix types *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/010sukharev.ml
  Fatal error: exception Failure("Only simple identifiers are allowed in let-binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 341, characters 11-91
  Called from Type.Inference.infer_structure_item in file "lib/type/type.ml", line 544, characters 11-127
  Called from Type.Inference.infer_program.(fun) in file "lib/type/type.ml", line 625, characters 30-65
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Type.Inference.infer in file "lib/type/type.ml", line 640, characters 18-50
  Called from Dune__exe__Repl.run_single.run in file "bin/repl.ml", line 82, characters 22-31
  Called from Dune__exe__Repl.run_single in file "bin/repl.ml", line 111, characters 12-20
  [2]

(* fix types *)
  $ ../bin/repl.exe --no-hi --file manytests/typed/015tuples.ml
  Fatal error: exception Failure("Only simple identifiers are allowed in let-binding")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 341, characters 11-91
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 326, characters 32-58
  Called from Type.Inference.infer_expr in file "lib/type/type.ml", line 326, characters 32-58
  Called from Type.Inference.infer_structure_item in file "lib/type/type.ml", line 544, characters 11-127
  Called from Type.Inference.infer_program.(fun) in file "lib/type/type.ml", line 625, characters 30-65
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Type.Inference.infer in file "lib/type/type.ml", line 640, characters 18-50
  Called from Dune__exe__Repl.run_single.run in file "bin/repl.ml", line 82, characters 22-31
  Called from Dune__exe__Repl.run_single in file "bin/repl.ml", line 111, characters 12-20
  [2]

(* fix interp *)
$ ../bin/repl.exe --no-hi --file manytests/typed/016lists.ml
