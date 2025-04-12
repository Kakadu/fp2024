  $ ../bin/REPL.exe --dinference <<EOF
  > 
  > let c = 'x'
  > let s = "asv"
  > 
  > let cons2 a b xs = a::b::xs
  > let x = cons2 1 2 [3]
  > let xxx = (1,2,3,(4,5))
  val c : char = 'x'
  val s : string = "asv"
  val cons2 : 'a->'a->'a list->'a list = <fun>
  val x : int list = [1; 2; 3]
  val xxx : int*int*int*(int*int) = (1, 2, 3, (4, 5))

  $ ../bin/REPL.exe --dinference < manytests/typed/001fac.ml
  24
  val fac : int->int = <fun>
  val main : int = 0
  $ ../bin/REPL.exe --dinference < manytests/typed/001fac.ml
  24
  val fac : int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/002fac.ml
  24
  val fac_cps : int->(int->'a)->'a = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/003fib.ml
  3
  3
  val fib_acc : int->int->int->int = <fun>
  val fib : int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100
  val wrap : 'a->'a = <fun>
  val test3 : int->int->int->int = <fun>
  val test10 : int->int->int->int->int->int->int->int->int->int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/005fix.ml
  720
  val fix : (('a->'b)->'a->'b)->'a->'b = <fun>
  val fac : (int->int)->int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/006partial2.ml
  1
  2
  3
  7
  val foo : int->int->int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/006partial3.ml
  4
  8
  9
  val foo : int->int->int->unit = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/006partial.ml
  1122
  val foo : int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000
  val _start : unit->unit->int->unit->int->int->unit->int->int->int = <fun>
  val main : unit = ()

  $ ../bin/REPL.exe --dinference < manytests/typed/008ascription.ml
  8
  val addi : ('a->bool->int)->('a->bool)->'a->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/009let_poly.ml
  val temp : int*bool = (1, true)

  $ ../bin/REPL.exe --dinference < manytests/typed/010sukharev.ml
  val _1 : int->int->int*'a->bool = <fun>
  val _2 : int = 1
  val _3 : (int*string) option = Some (1, "hi")
  val _4 : int->'a = <fun>
  val _5 : int = 42
  val _6 : 'a option->'a = <fun>
  val int_of_option : int option->int = <function>
  val _42 : int->bool = <function>
  val id1 : 'a->'a = <fun>
  val id2 : 'a->'a = <fun>

  $ ../bin/REPL.exe --dinference < manytests/typed/015tuples.ml
  1
  1
  1
  1
  val fix : (('a->'b)->'a->'b)->'a->'b = <fun>
  val map : ('b->'a)->'b*'b->'a*'a = <fun>
  val fixpoly : (('a->'b)*('a->'b)->'a->'b)*(('a->'b)*('a->'b)->'a->'b)->('a->'b)*('a->'b) = <fun>
  val feven : 'a*(int->int)->int->int = <fun>
  val fodd : (int->int)*'a->int->int = <fun>
  val tie : (int->int)*(int->int) = (<fun>, <fun>)
  val meven : int->int = <fun>
  val modd : int->int = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/typed/016lists.ml
  1
  2
  3
  8
  val length : 'a list->int = <fun>
  val length_tail : 'a list->int = <fun>
  val map : ('a->'b)->'a list->'b list = <fun>
  val append : 'a list->'a list->'a list = <fun>
  val concat : 'a list list->'a list = <fun>
  val iter : ('a->unit)->'a list->unit = <fun>
  val cartesian : 'b list->'a list->('b*'a) list = <fun>
  val main : int = 0

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/001.ml
  Type inference error: Undefined variable 'fac'

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/002if.ml
  Type inference error: Unification( if _ then [first type] else [second type] ) failed for following unifiable types: int and bool

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/003occurs.ml
  Type inference error: Occurs check failed: the type variable 'ty2 occurs inside'ty2->'ty4->'ty5

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/004let_poly.ml
  Type inference error: Unification( ([type1] -> [...]) and ([type2] -> [...]) ) failed for following unifiable types: bool and int

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/005.ml
  Type inference error: Unification( ([type1] -> [...]) and ([type2] -> [...]) ) failed for following unifiable types: string and int

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/015tuples.ml
  Type inference error: Recursive binding failed: the LHS of the recursive binding must not be a composed pattern (e.g. tuple, list, etc.). A variable is required.

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/016tuples_mismatch.ml
  Type inference error: Unification( let binding with pattern ) failed for following unifiable types: int*int*int and 'ty1*'ty0

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/097fun_vs_list.ml
  Type inference error: Unification( let binding with pattern ) failed for following unifiable types: 'ty0->'ty0 and 'ty2 list

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/097fun_vs_unit.ml
  Type inference error: Unification( let binding with pattern ) failed for following unifiable types: 'ty0->'ty0 and unit

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/098rec_int.ml
  Type inference error: Recursive binding failed: a function was expected as the RHS. Recursive binding is impossible for that variable. It would lead to infinite recursion.

  $ ../bin/REPL.exe --dinference < manytests/do_not_type/099.ml
  Type inference error: Recursive binding failed: the LHS of the recursive binding must not be a composed pattern (e.g. tuple, list, etc.). A variable is required.
