(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Interpreter
open Ocamladt_lib.Interpreter.PPrinter
open Ocamladt_lib.Parser

let pp_interpret ast =
  match run_interpreter ast with
  | Ok olist ->
    List.iter
      (fun (tag, val') ->
        match tag with
        | Some id -> Format.printf "val %s = %a\n" id pp_value val'
        | None -> if val' <> VString "" then Format.printf "_ = %a\n" pp_value val')
      olist
  | Error e -> print_error e
;;

let pp_parse str =
  let ast = parse str in
  match ast with
  | Ok ast -> pp_interpret ast
  | Error _ -> print_error ParserError
;;

let%expect_test "empty program (fail: EmptyProgram)" =
  pp_parse {||};
  [%expect {| Empty program |}]
;;

let%expect_test "negative int constant" =
  pp_parse {|-1;;|};
  [%expect {|
    _ = -1 |}]
;;

let%expect_test "zero" =
  pp_parse {|0;;|};
  [%expect {| _ = 0 |}]
;;

let%expect_test "x" =
  pp_parse {|x;;|};
  [%expect {| Interpreter error: Unbound value x |}]
;;

let%expect_test "substraction" =
  pp_parse {|5-11;;|};
  [%expect {|
    _ = -6 |}]
;;

let%expect_test "strange move" =
  pp_parse {|5=5;;|};
  [%expect {|
    _ = true |}]
;;

let%expect_test "assignment (fail: UnboundValue - x)" =
  pp_parse {|x = 51;;|};
  [%expect {|
    Interpreter error: Unbound value x |}]
;;

let%expect_test "operators with different priorities" =
  pp_parse {|5-5*1;;|};
  [%expect {| _ = 0 |}]
;;

let%expect_test "just let (int)" =
  pp_parse {|let x = 51;;|};
  [%expect {| val x = 51 |}]
;;

let%expect_test "just let (string)" =
  pp_parse {|let x = "51";;|};
  [%expect {| val x = "51" |}]
;;

let%expect_test "just let (char)" =
  pp_parse {|let x = '5';;|};
  [%expect {| val x = '5' |}]
;;

let%expect_test "int print_endline (fail: TypeMismatch)" =
  pp_parse {|let x = 51 in 
print_endline x;;|};
  [%expect {|
    Interpreter error: Type mismatch |}]
;;

let%expect_test "string print_endline" =
  pp_parse {|let x = "51" in 
print_endline x;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_endline" =
  pp_parse {|print_endline "51";;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_endline as an arg" =
  pp_parse {|let f = print_endline in 
f "Hello";;|};
  [%expect {|
    Hello |}]
;;

let%expect_test "print_endline as an arg (fail: TypeMismatch)" =
  pp_parse {|let f = print_endline in 
f 5;;|};
  [%expect {|
    Interpreter error: Type mismatch |}]
;;

let%expect_test "print_int as an arg" =
  pp_parse {|let f = print_int in 
f 51;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_int let assignment" =
  pp_parse {|let reca = 51 in 
print_int reca;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_char as an arg" =
  pp_parse {|let f = print_char in 
f '5';;|};
  [%expect {|
    5 |}]
;;

let%expect_test "print_char let assignment" =
  pp_parse {|let reca = '5' in 
print_char reca;;|};
  [%expect {|
    5 |}]
;;

let%expect_test "let assignment none (fail: PatternMismatch)" =
  pp_parse {|let Some Some Some Some Some None = 1 in 
print_int None;;|};
  [%expect {|
    Interpreter error: Pattern mismatch |}]
;;

let%expect_test "multiple let assignments" =
  pp_parse {| let x = 3 in let y = 4 in print_int (x + y) ;; |};
  [%expect {|
    7 |}]
;;

let%expect_test "multiple let bool assignments" =
  pp_parse {| let x = 5 = 5 in let y = 4 = 5 in print_bool (x && y) ;; |};
  [%expect {|
    false |}]
;;

let%expect_test "fun assignment with bool operators" =
  pp_parse {| let id = fun x y -> x && y in print_bool (id true false) ;; |};
  [%expect {| Interpreter error: Type mismatch |}]
;;

let%expect_test "fun assignment with bool operators (tuple arg)" =
  pp_parse {| let id = fun (x, y) -> x && y in print_bool (id (true,false)) ;; |};
  [%expect {| Interpreter error: Type mismatch |}]
;;

let%expect_test "too damn simple fun assignment (TC should fail?)" =
  pp_parse {| let id = fun x -> y in print_int (id 7) ;; |};
  [%expect {| Interpreter error: Unbound value y |}]
;;

(*4 am vibes, im sorry*)
let%expect_test "not too damn simple fun assignment" =
  pp_parse {| let id = fun x -> x * x in print_int (id 7) ;; |};
  [%expect {|
    49 |}]
;;

let%expect_test "match case (_ case)" =
  pp_parse
    {|
let classify n = 
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "other"
in
print_endline (classify 2);; |};
  [%expect {|
    other |}]
;;

let%expect_test "match case (specific pattern case)" =
  pp_parse
    {|
let classify n = 
  match n with
  | "0" -> 51
  | "1" -> 811
  | _ -> 0
in
print_int (classify "1");; |};
  [%expect {|
    811 |}]
;;

let%expect_test "if then case" =
  pp_parse
    {| let x = 10 in
if x > 5 then print_endline "> 5"
else print_endline "<= 5";;
 |};
  [%expect {|
    > 5 |}]
;;

let%expect_test "if then case (else)" =
  pp_parse
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_endline "One"
  else
    print_endline "Other"
in 
check_number 5
;; |};
  [%expect {|
    Other |}]
;;

let%expect_test "if then case (then)" =
  pp_parse
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_endline "One"
  else
    print_endline "Other"
in 
check_number 0
;; |};
  [%expect {|
    Zero |}]
;;

let%expect_test "if then case (else if)" =
  pp_parse
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_int 555555555555
  else
    print_endline "Other"
in 
check_number 1
;; |};
  [%expect {|
    555555555555 |}]
;;

let%expect_test "if then case (else if) v2" =
  pp_parse
    {| let check_number n =
  if n >= 0 then
    print_endline "Zero"
  else if n = 1 then
    print_int 555555555555 - n 
  else
    print_endline "Other"
in 
check_number 1
;; |};
  [%expect {|
    Zero |}]
;;

let%expect_test "nested assignments" =
  pp_parse
    {| 
    let x = 
      let y = 
        let z = 
          let w = 1
          in w
        in z
      in y
;; |};
  [%expect {|
    val x = 1 |}]
;;

let%expect_test "factorial" =
  pp_parse
    {| 
let rec fact n = if n = 0 then 1 else n * fact(n-1) in 
print_int (fact 5)
;; |};
  [%expect {|
    120 |}]
;;

(*i just wanna km*)
(*upd: dont mind. fixed :\ .*)
let%expect_test "recursive function (nested apply - multiple args)" =
  pp_parse
    {| let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) in print_int (pow 5 6);;|};
  [%expect {|
    15625 |}]
;;

let%expect_test "factorial (multiple structure items)" =
  pp_parse
    {| 
let rec fact n = if n = 0 then 1 else n * fact(n-1);; 
let x = fact 6 in print_int x ;; |};
  [%expect {|
    720
    val fact = <fun> |}]
;;

let%expect_test "not y.dev" =
  pp_parse
    {| let arith x y = (x * y, x / y, x + y, x - y);;
    let prod x y = 
        let fst (a, _, _, _) = a in
        fst (arith x y)
    ;;
    let p = prod 3 1;;
  |};
  [%expect {|
    val arith = <fun>
    val prod = <fun>
    val p = 3 |}]
;;

let%expect_test "wrong input (fail: ParserError)" =
  pp_parse {|let = ;;|};
  [%expect {| Parser Error |}]
;;

let%expect_test "eval simple let binding" =
  pp_parse {| let a = -(4 + 4) and b = true;; |};
  [%expect {|
  val a = -8
  val b = true
  |}]
;;

let%expect_test "multiple nested let's" =
  pp_parse {|
  let f =
    let x = "fowl" in
    let y = "51" in
    x <> y
  ;;
  |};
  [%expect {| val f = true |}]
;;

let%expect_test "tuple assignment" =
  pp_parse {| let test1 = (1, "hello", 314);; |};
  [%expect {|
  val test1 = (1, "hello", 314)
  |}]
;;

let%expect_test "tuple (no assignment)" =
  pp_parse {| (1, "hello", 314);; |};
  [%expect {|
  _ = (1, "hello", 314)
  |}]
;;

let%expect_test "tuple assignment v2" =
  pp_parse {| let swap (x, y) = (y, x);;
let test = swap (1, "ocaml");; |};
  [%expect {|
  val swap = <fun>
  val test = ("ocaml", 1)
  |}]
;;

let%expect_test "()" =
  pp_parse
    {|
    let a =
      let b = 
        let rec f = (let x = 3 in x) + 1 
        in f
      in ();;
    let s = "string";;
    |};
  [%expect {|
    val a = unit
    val s = "string" |}]
;;

let%expect_test "multiple funs (+ nested)" =
  pp_parse
    {| let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f)) ;;
 |};
  [%expect {|
  val fix = <fun>
  |}]
;;

let%expect_test "option type match" =
  pp_parse {|
let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1
;;
 |};
  [%expect {|
  val _2 = <function>
  |}]
;;

let%expect_test "tuples mismatch (fail: PatternMismatch)" =
  pp_parse {|
let a, _ = 1, 2, 3 ;;
 |};
  [%expect {|
  Interpreter error: Pattern mismatch
  |}]
;;

let%expect_test "just fun assignment" =
  pp_parse {|
let a = (fun x -> x) ;; 
 |};
  [%expect {|
  val a = <fun>
  |}]
;;

let%expect_test "list (shouldn't work, see tests below)" =
  pp_parse {|
let [a] = [42] ;; 
 |};
  [%expect {|
    val a = 42 |}]
;;

(* -------- ADT --------*)

let%expect_test "adt" =
  pp_parse
    {|
type shape = Point of int 
  | Circle of int * int 
  | Rect of int * int * int 
;;
|};
  [%expect {||}]
;;

(*we dont support regular types like float*)
let%expect_test "adt (infer should fail)" =
  pp_parse
    {|
type point = float * float;;
type shape = Point of point
  | Circle of point * float
  | Rect of point * point 
;;|};
  [%expect {| |}]
;;

let%expect_test "simple adt with pattern matching + printing" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int 
  | Square of int
;;
let area s = 
    match s with
    | Circle c -> 3 
    | Square c -> 0
    | _ -> 10
;;
let x = Circle 5 in
let y = area x in
print_int y
;;
  |};
  [%expect {|
    3
    val area = <fun> |}]
;;

let%expect_test "simple adt with pattern matching function (else case) + printing" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int 
  | Square of int
;;
let area s = 
    match s with
    | Square c -> 0
    | Circle c -> 3 
    | _ -> 10
;;
let x = Rectangle 5 in
let y = area x in
print_int y
;;
  |};
  [%expect {|
    10
    val area = <fun> |}]
;;

let%expect_test "simple adt with pattern matching + printing v2" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let area s = 
    match s with
    | Circle c -> 3 
    | Square c -> 0
    | _ -> 10
;;
let x = Rectangle (5, 10) in
let y = area x in
print_int y
;;
  |};
  [%expect {|
    10
    val area = <fun> |}]
;;

let%expect_test "simple adt with pattern matching + printing v3" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let area s = 
    match s with
    | Circle c -> 3 
    | Square c -> 0
    | Rectangle (c1, c2) -> c1 * c2
;;
let x = Rectangle (5, 10) in
let y = area x in
print_int y
;;
  |};
  [%expect {|
    50
    val area = <fun> |}]
;;

let%expect_test "simple adt NORM (infer should fail)" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;

let x = Chto 5
;;
  |};
  [%expect {| val x = Chto 5 |}]
;;

let%expect_test "simple adt with pattern matching (fail: PatternMismatch)" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let area s = 
    match s with
    | Circle c -> 3 
    | Chto c -> 0
;;
let x = Square 5 in
let y = area x in
print_int y
;;
  |};
  [%expect {| Interpreter error: Pattern mismatch |}]
;;

let%expect_test "simple adt (fail: UnboundValue area)" =
  pp_parse
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let x = Cir 5 in 
print_int area x;;
  |};
  [%expect {| Interpreter error: Unbound value area |}]
;;

(* good, needs a initialization check + infer print(see next tests)*)
let%expect_test "poly adt tree" =
  pp_parse {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;
  |};
  [%expect {| |}]
;;

let%expect_test "poly adt tree (dumb insert)" =
  pp_parse
    {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;

let rec insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | _ -> Node (x, Leaf, Leaf)
;;

let tree1 = 
 insert 6 Leaf
;;

let rec tree_size t =
  match t with
  | Leaf -> 0
  | Node (a, left, right) -> 1 + tree_size left + tree_size right
;;

let () = print_int (tree_size tree1)

  |};
  [%expect
    {|
    1
    val insert = <fun>
    val tree1 = Node (6, Leaf, Leaf)
    val tree_size = <fun> |}]
;;

let%expect_test "empty poly adt tree (dumb insert)" =
  pp_parse
    {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;

let rec insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | _ -> Node (x, Leaf, Leaf)
;;

let tree2 = Leaf;;

let rec tree_size t =
  match t with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + tree_size left + tree_size right
;;

let () = print_int (tree_size tree2)

  |};
  [%expect
    {|
    0
    val insert = <fun>
    val tree2 = Leaf
    val tree_size = <fun> |}]
;;

let%expect_test "poly adt tree v2" =
  pp_parse
    {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;

let rec insert x = function
  | Leaf -> Node (x, Leaf, Leaf)  
  | Node (value, left, right) ->
      if x < value then
        Node (value, insert x left, right)
    else
        Node (value, left, insert x right)
;;

let tree =
  insert 5 (insert 8 (insert 3 (insert 6 Leaf)));;

let rec tree_size t =
  match t with
  | Leaf -> 0
  | Node (a, left, right) -> 1 + tree_size left + tree_size right
;;

let () = print_int (tree_size tree)

  |};
  [%expect
    {|
    4
    val insert = <fun>
    val tree = Node (6, Node (3, Leaf, Node (5, Leaf, Leaf)), Node (8, Leaf, Leaf))
    val tree_size = <fun> |}]
;;

let%expect_test "poly adt tree v2 (constructs)" =
  pp_parse
    {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;

let tree =
  Node (6,
    Node (3, Leaf, Node (5, Leaf, Leaf)),
    Node (8, Leaf, Leaf)
  );;

let rec tree_size t =
  match t with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + tree_size left + tree_size right
;;

let () = print_int (tree_size tree)

  |};
  [%expect
    {|
    4
    val tree = Node (6, Node (3, Leaf, Node (5, Leaf, Leaf)), Node (8, Leaf, Leaf))
    val tree_size = <fun> |}]
;;

(*good*)
let%expect_test "simple print" =
  pp_parse {|
let () = print_int 5;;
  |};
  [%expect {| 5 |}]
;;

let%expect_test "empty program (no ;;) (fail: emptyprogram)" =
  pp_parse {||};
  [%expect {| Empty program |}]
;;

let%expect_test "function" =
  pp_parse
    {|
     let f = function
        | 5 -> 5
        | _ -> 0
      in
      f 5, f 42
  |};
  [%expect {| _ = (5, 0) |}]
;;

let%expect_test "pattern matching function with print_int" =
  pp_parse {| let f = function 0 -> 42 | _ -> 99 in 
print_int (f 0)|};
  [%expect {| 42 |}]
;;

let%expect_test "nested function as apply with print_int" =
  pp_parse {| print_int ((function x -> function y -> x + y) 3 4);; |};
  [%expect {| 7 |}]
;;

let%expect_test "tuple pattern function with print_string (fail: TypeMismatch)" =
  pp_parse {|  print_endline ((function (x, y) -> x + y) ("Hello", " World")) |};
  [%expect {| Interpreter error: Type mismatch |}]
;;

let%expect_test "function inside let binding with print_int" =
  pp_parse {| let f = function x -> x * 2 in print_int (f 10) |};
  [%expect {| 20 |}]
;;

let%expect_test "some" =
  pp_parse
    {|
 let f = function
        | Some x -> x
        | None -> 0
      in
      f None, f (Some 42)
  |};
  [%expect {| _ = (None, 42) |}]
;;

(*aka manytests*)

let%expect_test "001fac" =
  pp_parse
    {| 
let rec fac n = if n<=1 then 1 else n * fac (n-1)

let main =
  let () = print_int (fac 4) in
  0
|};
  [%expect {|
    24
    val fac = <fun>
    val main = 0 |}]
;;

let%expect_test "002fac" =
  pp_parse
    {| 
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n))

let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0
|};
  [%expect {|
    24
    val fac_cps = <fun>
    val main = 0 |}]
;;

let%expect_test "003fac" =
  pp_parse
    {| 
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1

let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2) 

let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0
|};
  [%expect {|
    3
    3
    val fib_acc = <fun>
    val fib = <fun>
    val main = 0 |}]
;;

let%expect_test "004manyargs" =
  pp_parse
    {|

let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0
|};
  [%expect
    {|
    1111111111
    1
    10
    100
    val wrap = <fun>
    val test3 = <fun>
    val test10 = <fun>
    val main = 0 |}]
;;

let%expect_test "005fix" =
  pp_parse
    {|
let rec fix f x = f (fix f) x

let fac self n = if n<=1 then 1 else n * self (n-1)

let main =
  let () = print_int (fix fac 6) in
  0
|};
  [%expect {|
    720
    val fix = <fun>
    val fac = <fun>
    val main = 0 |}]
;;

let%expect_test "006partial" =
  pp_parse
    {|
let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + b * c

let main =
  let foo = foo 1 in
  let foo = foo 2 in
  let foo = foo 3 in
  let () = print_int foo in
  0
|};
  [%expect {|
    1
    2
    3
    7
    val foo = <fun>
    val main = 0 |}]
;;

let%expect_test "006partial2" =
  pp_parse
    {|
let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

let foo x = foo true (foo false (foo true (foo false x)))
let main =
  let () = print_int (foo 11) in
  0
|};
  [%expect {|
    1122
    val foo = <fun>
    val main = 0 |}]
;;

let%expect_test "006partial3" =
  pp_parse
    {|

let foo a =
  let () = print_int a in fun b ->
  let () = print_int b in fun c ->
  print_int c

let main =
  let () = foo 4 8 9 in
  0
|};
  [%expect {|
    4
    8
    9
    val foo = <fun>
    val main = 0 |}]
;;

let%expect_test "007order" =
  pp_parse
    {|
let _start () () a () b _c () d __ =
  let () = print_int (a+b) in
  let () = print_int __ in
  a*b / _c + d


let main =
  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
|};
  [%expect
    {|
    1
    2
    4
    -1
    103
    -555555
    10000
    val _start = <fun>
    val main = "" |}]
;;

let%expect_test "008ascription" =
  pp_parse
    {|
let addi = fun f g x -> (f x (g x: bool) : int)

let main =
  let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
  0
|};
  [%expect {|
    8
    val addi = <fun>
    val main = 0 |}]
;;

let%expect_test "009let_poly" =
  pp_parse {|
let temp =
  let f = fun x -> x in
  (f 1, f true)
|};
  [%expect {| val temp = (1, true) |}]
;;

let%expect_test "010sukharev" =
  pp_parse
    {|
  let _1 = fun x y (a, _) -> (x + y - a) = 1

let _2 =
    let x, Some f = 1, Some ( "p1onerka was here" )
    in x

let _3 =  Some (1, "hi")

let _4 = let rec f x = f 5 in f

let _5 =
    let id x = x in
    match Some id with
      | Some f -> let _ = f "42" in f 42
      | None -> 0

let int_of_option = function Some x -> x | None -> 0
|};
  [%expect
    {|
    val _1 = <fun>
    val _2 = 1
    val _3 = Some (1, "hi")
    val _4 = <fun>
    val _5 = 42
    val int_of_option = <function> |}]
;;

let%expect_test "011sukharev" =
  pp_parse {|
let id1, id2 = let id x = x in (id, id)

|};
  [%expect {|
    val id1 = <fun>
    val id2 = <fun>
   |}]
;;

let%expect_test "012sukharev" =
  pp_parse {|
let _6 = fun arg -> match arg with Some x -> let y = x in y

|};
  [%expect {| val _6 = <fun> |}]
;;

let%expect_test "013sukharev" =
  pp_parse {|

let _42 = function 42 -> true | _ -> false
|};
  [%expect {| val _42 = <function> |}]
;;

let%expect_test "015tuples" =
  pp_parse
    {|

let rec fix f x = f (fix f) x
let map f p = let (a,b) = p in (f a, f b)
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l
let feven p n =
  let (e, o) = p in
  if n = 0 then 1 else o (n - 1)
let fodd p n =
  let (e, o) = p in
  if n = 0 then 0 else e (n - 1)
let tie = fixpoly (feven, fodd)

let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)
let main =
  let () = print_int (modd 1) in
  let () = print_int (meven 2) in
  let (even,odd) = tie in
  0
|};
  [%expect
    {|
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
    val main = 0 |}]
;;

(*bad**)
let%expect_test "016lists" =
  pp_parse
    {|
let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper

let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0
|};
  [%expect
    {|
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
    val main = 0 |}]
;;

let%expect_test "debug_length" =
  pp_parse
    {|
    let rec map f xs = match xs with | [] -> [] | h::t -> (f h)::map f t in
     let rec append xs ys = match xs with [] -> ys | h::t -> h::append t ys in
     let rec cartesian xs ys =
       match xs with
       | [] -> []
       | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)
     in
    let rec length xs =
       match xs with
       | [] -> 0
       | h::t -> 1 + length t
     in
     let result = cartesian [1;2] [1;2;3;4] in
     let () = result in 
     length result|};
  [%expect {|
    _ = 8 |}]
;;

let%expect_test "empty_list" =
  pp_parse {|match [] with | [] -> 1 | _ -> 0|};
  [%expect {|
    _ = 1 |}]
;;

let%expect_test "cons_head" =
  pp_parse {| match [1;2;3] with | h::t -> h |};
  [%expect {|
    _ = 1 |}]
;;

let%expect_test "cons_tail" =
  pp_parse "match [1;2;3] with | h::t -> t";
  [%expect {|
    _ = [2; 3] |}]
;;

let%expect_test "tuple_cons" =
  pp_parse {| match [1;2;3] with | h::t -> (h, t) |};
  [%expect {|
    _ = (1, [2; 3]) |}]
;;

let%expect_test "length_function" =
  pp_parse
    {|let rec length xs = match xs with | [] -> 0 | h::t -> 1 + length t in length [1;2;3]|};
  [%expect {|
    _ = 3 |}]
;;

let%expect_test "length_tail_function" =
  pp_parse
    {| let rec helper acc xs = match xs with | [] -> acc | h::t -> helper (acc+1) t in
     helper 0 [1;2;3] |};
  [%expect {|
    _ = 3 |}]
;;

let%expect_test "map_function" =
  pp_parse
    {| let rec map f xs = match xs with | [] -> [] | h::t -> (f h)::map f t in map (fun x
     -> x+1) [1;2;3] |};
  [%expect {|
    _ = [2; 3; 4] |}]
;;

let%expect_test "append_function" =
  pp_parse
    {|let rec append xs ys = match xs with | [] -> ys | h::t -> h::append t ys in append
     [1;2] [3;4] |};
  [%expect {|
    _ = [1; 2; 3; 4] |}]
;;

let%expect_test "concat_function" =
  pp_parse
    {|let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)
in 
    let rec concat xs =
      let rec helper xs =
        match xs with
        | [] -> []
        | h::tl -> append h (helper tl)
      in helper xs
    in 
    concat [[1;2]; [3; 4]]|};
  [%expect {|
    _ = [1; 2; 3; 4] |}]
;;

let%expect_test "iter_function" =
  pp_parse
    {| let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter
     print_int [1;2;3]|};
  [%expect {|
val iter = <fun>
|}]
;;

let%expect_test "list_basic" =
  pp_parse {|let lst = 1 :: 2 :: 3 :: [] in lst|};
  [%expect {|
    _ = [1; 2; 3] |}]
;;

let%expect_test "list_match" =
  pp_parse {|match 1 :: 2 :: 3 :: [] with | [] -> 0 | h :: _ -> h|};
  [%expect {|
    _ = 1 |}]
;;

let%expect_test "list_append" =
  pp_parse
    {|let append xs ys = match xs with | [] -> ys | h :: t -> h :: append t ys in append
     [1; 2] [3; 4]|};
  [%expect {|
    _ = [1; 2; 3; 4] |}]
;;

let%expect_test "debug_cartesian" =
  pp_parse
    {|let rec map f xs = match xs with | [] -> [] | h::t -> (f h)::map f t in
     let rec append xs ys = match xs with | [] -> ys | h::t -> h::append t ys in
     let rec cartesian xs ys =
       match xs with
       | [] -> []
       | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)
     in
     cartesian [1;2] [1;2;3;4]|};
  [%expect {|
    _ = [(1, 1); (1, 2); (1, 3); (1, 4); (2, 1); (2, 2); (2, 3); (2, 4)] |}]
;;

let%expect_test "fix_factorial" =
  pp_parse
    {|
let rec fix f x = f (fix f) x in
let factorial f n =
  if n = 0 then 1 else n * f (n - 1)
in
let factorial_fn = fix factorial in
factorial_fn 5
|};
  [%expect {| _ = 120 |}]
;;

let%expect_test "map_increment" =
  pp_parse
    {|
let map f p = let (a,b) = p in (f a, f b) in
let pair = (1, 2) in
map (fun x -> x + 1) pair
|};
  [%expect {| _ = (2, 3) |}]
;;

let%expect_test "meven_modd" =
  pp_parse
    {|
let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)
in
(meven 2, modd 1)
|};
  [%expect {| _ = (1, 1) |}]
;;

let%expect_test "rec calls" =
  pp_parse {|
let rec f () = y in 
let y = 42 in 
f ()
;;
|};
  [%expect {| _ = 42 |}]
;;
