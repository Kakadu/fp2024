let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

(*~/Documents/repositories/fp2024/EMigunova/_build/default/bin/REPL.exe --dinference < /home/anastasia/Documents/repositories/fp2024/manytests/do_not_type/001.ml*)
let main =
  let () = print_int (fac 4) in
  0
;;

let rec fac_cps n k = if n = 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))

let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0
;;

let rec fib_acc a b n =
  if n = 1
  then b
  else (
    let n1 = n - 1 in
    let ab = a + b in
    fib_acc b ab n1)
;;

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0
;;

let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0
;;

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
    wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0
;;

(*5*)
let rec fix f x = f (fix f) x
let fac self n = if n <= 1 then 1 else n * self (n - 1)

let main =
  let () = print_int (fix fac 6) in
  0
;;

(*06partial*)
let foo b = if b then fun foo -> foo + 2 else fun foo -> foo * 10
let foo x = foo true (foo false (foo true (foo false x)))

let main =
  let () = print_int (foo 11) in
  0
;;

(*06partial2*)

let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + (b * c)
;;

let main =
  let foo = foo 1 in
  let foo = foo 2 in
  let foo = foo 3 in
  let () = print_int foo in
  0
;;

(*06partial3*)
let foo a =
  let () = print_int a in
  fun b ->
    let () = print_int b in
    fun c -> print_int c
;;

let main =
  let () = foo 4 8 9 in
  0
;;

(*007order*)
let _start () () a () b _c () d __ =
  let () = print_int (a + b) in
  let () = print_int __ in
  (a * b / _c) + d
;;

let main =
  print_int
    (_start
       (print_int 1)
       (print_int 2)
       3
       (print_int 4)
       100
       1000
       (print_int (-1))
       10000
       (-555555))
;;

(*008*)

let addi = fun f g x -> (f x (g x : bool) : int)

let main =
  let () =
    print_int
      (addi (fun x b -> if b then x + 1 else x * 2) (fun _start -> _start / 2 = 0) 4)
  in
  0
;;

(*009*)

let temp =
  let f = fun x -> x in
  f 1, f true
;;

(*_____________all tests until 010 are done_______*)

(*010*)

let _1 = fun x y (a, _) -> x + y - a = 1

let _2 =
  let x, Some f = 1, Some "p1onerka was here" in
  x
;;

let _3 = Some (1, "hi")

let _4 =
  let rec f x = f 5 in
  f
;;

let _5 =
  let id x = x in
  match Some id with
  | Some f ->
    let _ = f "42" in
    f 42
  | None -> 0
;;

let _6 =
  fun arg ->
  match arg with
  | Some x ->
    let y = x in
    y
;;

let int_of_option = function
  | Some x -> x
  | None -> 0
;;

let _42 = function
  | 42 -> true
  | _ -> false
;;

let id1, id2 =
  let id x = x in
  id, id
;;

(*015*)
let rec fix f x = f (fix f) x

let map f p =
  let a, b = p in
  f a, f b
;;

let fixpoly l = fix (fun self l -> map (fun li x -> li (self l) x) l) l

let feven p n =
  let e, o = p in
  if n = 0 then 1 else o (n - 1)
;;

let fodd p n =
  let e, o = p in
  if n = 0 then 0 else e (n - 1)
;;

let tie = fixpoly (feven, fodd)

let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)

let main =
  let () = print_int (modd 1) in
  let () = print_int (meven 2) in
  let even, odd = tie in
  let () = print_int (odd 3) in
  let () = print_int (even 4) in
  0
;;

(*016*)

let rec length xs =
  match xs with
  | [] -> 0
  | h :: tl -> 1 + length tl
;;

let length_tail =
  let rec helper acc xs =
    match xs with
    | [] -> acc
    | h :: tl -> helper (acc + 1) tl
  in
  helper 0
;;

let rec map f xs =
  match xs with
  | [] -> []
  | a :: [] -> [ f a ]
  | [ a; b ] -> [ f a; f b ]
  | [ a; b; c ] -> [ f a; f b; f c ]
  | a :: b :: c :: d :: tl -> f a :: f b :: f c :: f d :: map f tl
;;

let rec append xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> x :: append xs ys
;;

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h :: tl -> append h (helper tl)
  in
  helper
;;

let rec iter f xs =
  match xs with
  | [] -> ()
  | h :: tl ->
    let () = f h in
    iter f tl
;;

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h :: tl -> append (map (fun a -> h, a) ys) (cartesian tl ys)
;;

let main =
  let () = iter print_int [ 1; 2; 3 ] in
  let () = print_int (length (cartesian [ 1; 2 ] [ 1; 2; 3; 4 ])) in
  0
;;
