(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Interpreter
open Ocamladt_lib.Interpreter.PPrinter
open Ocamladt_lib.Parser

(* Full verison with TC
   let pp_interpret ast =
   match run_inference ast with
   | Ok _ ->
   (match run_interpreter ast with
   | Ok value -> print_value
   | Error e -> print_error e)
   | Error e -> print_type_error e (* to be implemented in TC*)
   ;;
*)

let pp_interpret_demo ast =
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

let pp_parse_demo str =
  let ast = parse str in
  match ast with
  | Ok ast -> pp_interpret_demo ast
  | Error _ -> print_error ParserError
;;

let%expect_test "empty program (fail: EmptyProgram)" =
  pp_parse_demo {||};
  [%expect {| Empty program |}]
;;

let%expect_test "negative int constant" =
  pp_parse_demo {|-1;;|};
  [%expect {|
    _ = -1 |}]
;;

let%expect_test "zero" =
  pp_parse_demo {|0;;|};
  [%expect {| _ = 0 |}]
;;

let%expect_test "x" =
  pp_parse_demo {|x;;|};
  [%expect {| Intepreter error: Unbound value x |}]
;;

let%expect_test "substraction" =
  pp_parse_demo {|5-11;;|};
  [%expect {|
    _ = -6 |}]
;;

let%expect_test "strange move" =
  pp_parse_demo {|5=5;;|};
  [%expect {|
    _ = true |}]
;;

let%expect_test "assignment (fail: UnboundValue - x)" =
  pp_parse_demo {|x = 51;;|};
  [%expect {|
    Intepreter error: Unbound value x |}]
;;

let%expect_test "operators with different priorities" =
  pp_parse_demo {|5-5*1;;|};
  [%expect {| _ = 0 |}]
;;

let%expect_test "just let (int)" =
  pp_parse_demo {|let x = 51;;|};
  [%expect {| val x = 51 |}]
;;

let%expect_test "just let (string)" =
  pp_parse_demo {|let x = "51";;|};
  [%expect {| val x = "51" |}]
;;

let%expect_test "just let (char)" =
  pp_parse_demo {|let x = '5';;|};
  [%expect {| val x = '5' |}]
;;

let%expect_test "int print_endline (fail: TypeMismatch)" =
  pp_parse_demo {|let x = 51 in 
print_endline x;;|};
  [%expect {|
    Intepreter error: Type mismatch |}]
;;

let%expect_test "string print_endline" =
  pp_parse_demo {|let x = "51" in 
print_endline x;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_endline" =
  pp_parse_demo {|print_endline "51";;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_endline as an arg" =
  pp_parse_demo {|let f = print_endline in 
f "Hello";;|};
  [%expect {|
    Hello |}]
;;

let%expect_test "print_endline as an arg (fail: TypeMismatch)" =
  pp_parse_demo {|let f = print_endline in 
f 5;;|};
  [%expect {|
    Intepreter error: Type mismatch |}]
;;

let%expect_test "print_int as an arg" =
  pp_parse_demo {|let f = print_int in 
f 51;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_int let assignment" =
  pp_parse_demo {|let reca = 51 in 
print_int reca;;|};
  [%expect {|
    51 |}]
;;

let%expect_test "print_char as an arg" =
  pp_parse_demo {|let f = print_char in 
f '5';;|};
  [%expect {|
    5 |}]
;;

let%expect_test "print_char let assignment" =
  pp_parse_demo {|let reca = '5' in 
print_char reca;;|};
  [%expect {|
    5 |}]
;;

let%expect_test "let assignment none (fail: PatternMismatch)" =
  pp_parse_demo {|let Some Some Some Some Some None = 1 in 
print_int None;;|};
  [%expect {|
    Intepreter error: Pattern mismatch |}]
;;

let%expect_test "multiple let assignments" =
  pp_parse_demo {| let x = 3 in let y = 4 in print_int (x + y) ;; |};
  [%expect {|
    7 |}]
;;

let%expect_test "multiple let bool assignments" =
  pp_parse_demo {| let x = 5 = 5 in let y = 4 = 5 in print_bool (x && y) ;; |};
  [%expect {|
    false |}]
;;

let%expect_test "fun assignment with bool operators" =
  pp_parse_demo {| let id = fun x y -> x && y in print_bool (id true false) ;; |};
  [%expect {| false |}]
;;

let%expect_test "fun assignment with bool operators (tuple arg)" =
  pp_parse_demo {| let id = fun (x, y) -> x && y in print_bool (id (true,false)) ;; |};
  [%expect {| false |}]
;;

let%expect_test "too damn simple fun assignment (TC should fail?)" =
  pp_parse_demo {| let id = fun x -> y in print_int (id 7) ;; |};
  [%expect {| Intepreter error: Unbound value y |}]
;;

(*4 am vibes, im sorry*)
let%expect_test "not too damn simple fun assignment" =
  pp_parse_demo {| let id = fun x -> x * x in print_int (id 7) ;; |};
  [%expect {|
    49 |}]
;;

let%expect_test "match case (_ case)" =
  pp_parse_demo
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
  pp_parse_demo
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
  pp_parse_demo
    {| let x = 10 in
if x > 5 then print_endline "> 5"
else print_endline "<= 5";;
 |};
  [%expect {|
    > 5 |}]
;;

let%expect_test "if then case (else)" =
  pp_parse_demo
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
  pp_parse_demo
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
  pp_parse_demo
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
  pp_parse_demo
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
  pp_parse_demo
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
  pp_parse_demo
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
  pp_parse_demo
    {| let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) in print_int (pow 5 6);;|};
  [%expect {|
    15625 |}]
;;

let%expect_test "factorial (multiple structure items)" =
  pp_parse_demo
    {| 
let rec fact n = if n = 0 then 1 else n * fact(n-1);; 
let x = fact 6 in print_int x ;; |};
  [%expect {|
    720
    val fact = <fun> |}]
;;

let%expect_test "not y.dev" =
  pp_parse_demo
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
  pp_parse_demo {|let = ;;|};
  [%expect {| Parser Error |}]
;;

let%expect_test "eval simple let binding" =
  pp_parse_demo {| let a = -(4 + 4) and b = true;; |};
  [%expect {|
  val a = -8
  val b = true
  |}]
;;

let%expect_test "multiple nested let's" =
  pp_parse_demo
    {|
  let f =
    let x = "fowl" in
    let y = "51" in
    x <> y
  ;;
  |};
  [%expect {| val f = true |}]
;;

let%expect_test "tuple assignment" =
  pp_parse_demo {| let test1 = (1, "hello", 314);; |};
  [%expect {|
  val test1 = (1, "hello", 314)
  |}]
;;

let%expect_test "tuple (no assignment)" =
  pp_parse_demo {| (1, "hello", 314);; |};
  [%expect {|
  _ = (1, "hello", 314)
  |}]
;;

let%expect_test "tuple assignment v2" =
  pp_parse_demo {| let swap (x, y) = (y, x);;
let test = swap (1, "ocaml");; |};
  [%expect {|
  val swap = <fun>
  val test = ("ocaml", 1)
  |}]
;;

let%expect_test "()" =
  pp_parse_demo
    {|
    let a =
      let b = 
        let rec f = (let x = 3 in x) + 1 
        in f
      in ();;
    let s = "string";;
    |};
  [%expect {|
    Intepreter error: Unbound value () |}]
;;

let%expect_test "multiple funs (+ nested)" =
  pp_parse_demo
    {| let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f)) ;;
 |};
  [%expect {|
  val fix = <fun>
  |}]
;;

let%expect_test "option type match" =
  pp_parse_demo
    {|
let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1
;;
 |};
  [%expect {|
  val _2 = <function>
  |}]
;;

(*good*)
let%expect_test "tuples" =
  pp_parse_demo {|
let rec (a, b) = (a, b) ;;
 |};
  [%expect {|
  Intepreter error: Unbound value a
  |}]
;;

let%expect_test "tuples mismatch (fail: PatternMismatch)" =
  pp_parse_demo {|
let a, _ = 1, 2, 3 ;;
 |};
  [%expect {|
  Intepreter error: Pattern mismatch
  |}]
;;

let%expect_test "just fun assignment" =
  pp_parse_demo {|
let a = (fun x -> x) ;; 
 |};
  [%expect {|
  val a = <fun>
  |}]
;;

let%expect_test "list (shouldn't work, see tests below)" =
  pp_parse_demo {|
let [a] = [42] ;; 
 |};
  [%expect {|
  Parser Error
  |}]
;;

(* -------- ADT --------*)

let%expect_test "adt" =
  pp_parse_demo
    {|
type shape = Point of int 
  | Circle of int * int 
  | Rect of int * int * int 
;;
|};
  [%expect {| |}]
;;

(*we dont support regular custom types*)
let%expect_test "adt (fail: ParserError)" =
  pp_parse_demo
    {|
type point = float * float ;;
type shape = Point of point
  | Circle of point * float
  | Rect of point * point 
;;|};
  [%expect {|
  Parser Error
  |}]
;;

let%expect_test "simple adt with pattern matching + printing" =
  pp_parse_demo
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
    val area = <fun>|}]
;;

let%expect_test "simple adt with pattern matching function (else case) + printing" =
  pp_parse_demo
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
    val area = <fun>|}]
;;

let%expect_test "simple adt with pattern matching + printing v2" =
  pp_parse_demo
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
    val area = <fun>|}]
;;

let%expect_test "simple adt with pattern matching + printing v3" =
  pp_parse_demo
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
    val area = <fun>|}]
;;

let%expect_test "simple adt (fail: UnboundValue)" =
  pp_parse_demo
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;

let x = Chto 5
;;
  |};
  [%expect {|
    Intepreter error: Unbound value Chto|}]
;;

let%expect_test "simple adt with pattern matching (fail: PatternMismatch)" =
  pp_parse_demo
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let area s = 
    match s with
    | Circle c -> 3 
    | Chto c -> 0
    | _ -> 10
;;
let x = Square 5 in
let y = area x in
print_int y
;;
  |};
  [%expect {|
    10
    val area = <fun>|}]
;;

let%expect_test "simple adt (fail: UnboundValue Cir)" =
  pp_parse_demo
    {|
type shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let x = Cir 5 in 
print_int area x;;
  |};
  [%expect {| Intepreter error: Unbound value Cir|}]
;;

(* good, needs a initialization check + infer print(see next tests)*)
let%expect_test "poly adt tree" =
  pp_parse_demo {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;
  |};
  [%expect {| |}]
;;

let%expect_test "poly adt tree" =
  pp_parse_demo
    {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;

let rec insert x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | _ -> Node (x, Leaf, Leaf)
;;

let tree = 
 insert 6 Leaf
;;
  |};
  [%expect {|
    val insert = <fun>
    val tree = <ADT>: Node |}]
;;

(*bad, idk, haven;t thought*)
let%expect_test "poly adt with pattern matching + printing" =
  pp_parse_demo
    {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;

let rec tree_size t =
  match t with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + tree_size left + tree_size right
;;

let my_tree = Node (42, Node (1, Leaf, Leaf), Node (2, Leaf, Leaf)) in
let size = tree_size my_tree in
let () = print_int size;;

  |};
  [%expect {|
    Parser Error|}]
;;

(*good*)
let%expect_test "poly adt" =
  pp_parse_demo {|
let () = print_int 5;;
  |};
  [%expect {| 5 |}]
;;

let%expect_test "empty program (no ;;) (fail: emptyprogram)" =
  pp_parse_demo {||};
  [%expect {| Empty program |}]
;;

let%expect_test "function" =
  pp_parse_demo
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
  pp_parse_demo {| let f = function 0 -> 42 | _ -> 99 in 
print_int (f 0)|};
  [%expect {| 42 |}]
;;

let%expect_test "nested function as apply with print_int" =
  pp_parse_demo {| print_int ((function x -> function y -> x + y) 3 4);; |};
  [%expect {| 7 |}]
;;

let%expect_test "tuple pattern function with print_string (fail: TypeMismatch)" =
  pp_parse_demo {|  print_endline ((function (x, y) -> x + y) ("Hello", " World")) |};
  [%expect {| Intepreter error: Type mismatch |}]
;;

let%expect_test "function inside let binding with print_int" =
  pp_parse_demo {| let f = function x -> x * 2 in print_int (f 10) |};
  [%expect {| 20 |}]
;;
