(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Interpreter

let test_interpret s =
  let open Stdlib.Format in
  match Parser.parse_expr s with
  | Ok parsed ->
    (match Inferencer.Infer.infer_program parsed with
     | Ok env_inf ->
       (match Interpreter.eval_structure parsed with
        | Ok env_int -> pp_env env_inf env_int
        | Error e -> printf "Interpreter error: %a\n" Values.pp_error e)
     | Error e -> printf "Infer error: %a\n" Typedtree.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;

let%expect_test "interpret pattern-matching" =
  let _ =
    test_interpret
      {| 
    let a x = match x with 1 -> true | _ -> false
    let b = a 3
  |}
  in
  [%expect {|
    {
    val a : int -> bool = <fun>
    val b : bool = false
    } |}]
;;

let%expect_test "interpet non-exhaustive pattern-matching" =
  let _ = test_interpret {| 
    let (0, b) = (4, 3)
  |} in
  [%expect
    {| Interpreter error: Ill left-hand side Pattern not acceptable for variable name |}]
;;

let%expect_test "interpret tuple pattern" =
  let _ = test_interpret {| 
    let (a, b) = (4, 3)
  |} in
  [%expect {|
    {
    val a : int = 4
    val b : int = 3
    } |}]
;;

let%expect_test "interpret simple pattern-matching" =
  let _ =
    test_interpret
      {| 
      let f = true
      let g = 
        match f with
        | true -> true
        | false -> false
      let n = not g
  |}
  in
  [%expect
    {|
    {
    val f : bool = true
    val g : bool = true
    val n : bool = false
    } |}]
;;

let%expect_test "interpret non-exhaustive match" =
  let _ =
    test_interpret
      {| 
      let f x = 
        (match x with
        | [] -> ""
        | hd :: snd :: tl -> hd)
      in
      f ["oops"]
  |}
  in
  [%expect {| Interpreter error: Pattern-matching failure |}]
;;

let%expect_test "interpret correct match" =
  let _ =
    test_interpret
      {| 
      let f x = 
        (match x with
        | [] -> 0
        | h::tl -> 1
        | hd :: snd :: tl -> hd)
      in
      print_int (f [1])
  |}
  in
  [%expect {|
    1
    {
    } |}]
;;

let%expect_test "interpret values using cons" =
  let _ =
    test_interpret
      {| 
      let a = 1 :: 2 :: 3 :: []
      let b = (1, "one") :: (2, "two") :: [(3, "three")]
      let c = [1; 2] :: [3; 4] :: []
  |}
  in
  [%expect
    {|
    {
    val a : int list = [1; 2; 3]
    val b : (int * string) list = [(1, "one"); (2, "two"); (3, "three")]
    val c : int list list = [[1; 2]; [3; 4]]
    } |}]
;;

let%expect_test "interpret mutual recursion" =
  let _ =
    test_interpret
      {| 
      let rec is_even n =
        if n = 0 then true else is_odd (n - 1)
      and is_odd n =
        if n = 0 then false else is_even (n - 1)
      let a  = is_even 4
  |}
  in
  [%expect
    {|
    {
    val a : bool = true
    val is_even : int -> bool = <fun>
    val is_odd : int -> bool = <fun>
    } |}]
;;

let%expect_test "interpret simple function with fun" =
  let _ = test_interpret {| 
      let f = fun x -> x + 3
      let a = f 3
  |} in
  [%expect {|
    {
    val a : int = 6
    val f : int -> int = <fun>
    } |}]
;;

let%expect_test "interpret simple function" =
  let _ = test_interpret {| 
      let f x y = x + y
      let a = f 3 4
  |} in
  [%expect {|
    {
    val a : int = 7
    val f : int -> (int -> int) = <fun>
    } |}]
;;

let%expect_test "interpret division by zero" =
  let _ = test_interpret {| 
      let a = 0
      let b = 30 / a
  |} in
  [%expect {| Interpreter error: Division by zero |}]
;;

let%expect_test "interpret multiple strucuture items" =
  let _ =
    test_interpret
      {| 
      let g m = m*m
      let f x = if x > 0 then h x else g x in
      f 5
  |}
  in
  [%expect {| Infer error: Undefined variable "h" |}]
;;

let%expect_test "001" =
  let _ = test_interpret {| 
    let recfac n = if n<=1 then 1 else n * fac (n-1)
  |} in
  [%expect {| Infer error: Undefined variable "fac" |}]
;;

let%expect_test "002" =
  let _ = test_interpret {| 
    let main = if true then 1 else false
  |} in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test "003" =
  let _ =
    test_interpret
      {| 
    let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))
  |}
  in
  [%expect {| Infer error: Occurs check failed: type variable 1 inside type '1 -> '3 |}]
;;

let%expect_test "004" =
  let _ =
    test_interpret
      {| 
    let _1 =
      (fun f -> (f 1, f true)) (fun x -> x)

    let _2 = function
      | Some f -> let _ = f "42" in f 42
      | None -> 1
      |}
  in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test "015" =
  let _ = test_interpret {| 
    let rec (a,b) = (a,b)

    let a, _ = 1, 2, 3
  |} in
  [%expect {| Infer error: Ill left-hand side : only variables are allowed |}]
;;

let%expect_test "099" =
  let _ =
    test_interpret
      {| 
    let rec Some x = Some 1

    let rec x = x + 1

    let Some a = (fun x -> x)

    let () = (fun x -> x)
  |}
  in
  [%expect {| Infer error: Ill left-hand side : only variables are allowed |}]
;;

let%expect_test "001fac" =
  let _ =
    test_interpret
      {| 
      let rec fac n = if n<=1 then 1 else n * fac (n-1)

      let main =
        let () = print_int (fac 4) in
        0

  |}
  in
  [%expect
    {|
    24
    {
    val fac : int -> int = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "002fac" =
  let _ =
    test_interpret
      {| 
      let rec fac_cps n k =
      if n=1 then k 1 else
      fac_cps (n-1) (fun p -> k (p*n))

      let main =
      let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
      0
    
  |}
  in
  [%expect
    {|
    24
    {
    val fac_cps : int -> ((int -> int) -> int) = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "003fib" =
  let _ =
    test_interpret
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
    
  |}
  in
  [%expect
    {|
    33
    {
    val fib : int -> int = <fun>
    val fib_acc : int -> (int -> (int -> int)) = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "004manyargs" =
  let _ =
    test_interpret
      {| 
      let wrap f = if 1 = 1 then f else f
      let test3 a b c =
      let a = print_int a in
      let b = print_int b in
      let c = print_int c in
      0
      let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j
      let rez =
          (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
            1000000000)
      let main =
      let rez =
          (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
            1000000000)
      in
      let () = print_int rez in
      let temp2 = wrap test3 1 10 100 in
      0
    
  |}
  in
  [%expect
    {|
    1111111111110100
    {
    val main : int = 0
    val rez : int = 1111111111
    val test10 : int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int))))))))) = <fun>
    val test3 : int -> (int -> (int -> int)) = <fun>
    val wrap : '0 -> '0 = <fun>
    } |}]
;;

let%expect_test "005fix" =
  let _ =
    test_interpret
      {| 
      let rec fix f x = f (fix f) x

      let fac self n = if n<=1 then 1 else n * self (n-1)

      let main =
        let () = print_int (fix fac 6) in
        0
  |}
  in
  [%expect
    {|
    720
    {
    val fac : (int -> int) -> (int -> int) = <fun>
    val fix : ((int -> int) -> (int -> int)) -> (int -> int) = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "006partial" =
  let _ =
    test_interpret
      {| 
      let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

      let foo x = foo true (foo false (foo true (foo false x)))
      let main =
        let () = print_int (foo 11) in
        0
  |}
  in
  [%expect
    {|
    1122
    {
    val foo : int -> int = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "006partial2" =
  let _ =
    test_interpret
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
      
  |}
  in
  [%expect
    {|
    1237
    {
    val foo : int -> (int -> (int -> int)) = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "006partial3" =
  let _ =
    test_interpret
      {| 
      let foo a =
        let () = print_int a in fun b ->
        let () = print_int b in fun c ->
        print_int c

      let main =
        let () = foo 4 8 9 in
        0
      
  |}
  in
  [%expect
    {|
    489
    {
    val foo : int -> (int -> (int -> unit)) = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "007order" =
  let _ =
    test_interpret
      {| 
      let _start () () a () b _c () d __ =
        let () = print_int (a+b) in
        let () = print_int __ in
        a*b / _c + d


      let main =
        print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
            
  |}
  in
  [%expect
    {|
    124-1103-55555510000
    {
    val _start : unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int)))))))) = <fun>
    val main : unit = ()
    } |}]
;;

let%expect_test "008ascription" =
  let _ =
    test_interpret
      {| 
    let addi = fun f g x -> (f x (g x: bool) : int)

    let main =
      let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
      0
  |}
  in
  [%expect
    {|
    8
    {
    val addi : ('2 -> (bool -> int)) -> (('2 -> bool) -> ('2 -> int)) = <fun>
    val main : int = 0
    } |}]
;;

let%expect_test "009letpoly" =
  let _ =
    test_interpret {| 
    let temp =
      let f = fun x -> x in
      (f 1, f true)
  |}
  in
  [%expect {|
    {
    val temp : (int * bool) = (1, true)
    } |}]
;;

let%expect_test "010" =
  let _ =
    test_interpret
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
      let _6 = fun arg -> match arg with Some x -> let y = x in y
      let int_of_option = function Some x -> x | None -> 0
      let _42 = function 42 -> true | _ -> false
      let id1, id2 = let id x = x in (id, id)
  |}
  in
  [%expect
    {|
    {
    val _1 : int -> (int -> ((int * '3) -> bool)) = <fun>
    val _2 : int = 1
    val _3 : ((int * string)) option = Some (1, "hi")
    val _4 : int -> '10 = <fun>
    val _42 : int -> bool = <fun>
    val _5 : int = 42
    val _6 : ('23) option -> '23 = <fun>
    val id1 : '32 -> '32 = <fun>
    val id2 : '33 -> '33 = <fun>
    val int_of_option : (int) option -> int = <fun>
    } |}]
;;

let%expect_test "015tuples" =
  let _ =
    test_interpret
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
        let (even, odd) = tie in
        let () = print_int (odd 3) in
        let () = print_int (even 4) in
        0
  |}
  in
  [%expect
    {|
    1111
    {
    val feven : ('29 * int -> int) -> (int -> int) = <fun>
    val fix : ((((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int))) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)) = <fun>
    val fixpoly : ((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int) = <fun>
    val fodd : (int -> int * '36) -> (int -> int) = <fun>
    val main : int = 0
    val map : ('9 -> '11) -> (('9 * '9) -> ('10 * '11)) = <fun>
    val meven : int -> int = <fun>
    val modd : int -> int = <fun>
    val tie : (int -> int * int -> int) = (<fun>, <fun>)
    } |}]
;;

let%expect_test "016lists" =
  let _ =
    test_interpret
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
  |}
  in
  [%expect
    {|
    1238
    {
    val append : (int * int) list -> ((int * int) list -> (int * int) list) = <fun>
    val cartesian : int list -> (int list -> (int * int) list) = <fun>
    val concat : (int * int) list list -> (int * int) list = <fun>
    val iter : (int -> unit) -> (int list -> unit) = <fun>
    val length : (int * int) list -> int = <fun>
    val length_tail : '16 list -> int = <fun>
    val main : int = 0
    val map : (int -> (int * int)) -> (int list -> (int * int) list) = <fun>
    } |}]
;;
