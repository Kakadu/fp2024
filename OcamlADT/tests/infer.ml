(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes
open Base

let filter names =
  let rev_names = List.rev names in
  List.fold_left rev_names ~init:[] ~f:(fun acc name ->
    match List.find acc ~f:(fun a -> String.equal a name) with
    | None -> acc @ [ name ]
    | Some _ -> acc)
;;

let pprint_result env (names : string list) =
  let trash = [] in
  List.iter
    (List.rev @@ filter names)
    ~f:(fun key ->
      match Map.find env key with
      | None -> printf ""
      | Some typ ->
        (match List.find trash ~f:(fun a -> String.equal a key) with
         | Some _ -> printf ""
         | None ->
           (match typ with
            | Forall (args, typ) ->
              let m, _, _ = minimize (binder_to_list args) in
              (match key with
               | x when Stdlib.Char.code x.[0] >= 65 && Stdlib.Char.code x.[0] <= 90 ->
                 printf ""
               | "-" -> printf "%s : %a\n" key (pprint_type ~poly_names_map:m) typ
               | _ -> printf "val %s : %a\n" key (pprint_type ~poly_names_map:m) typ))))
;;

let parse_and_infer_result program =
  match parse_str program with
  | str ->
    (match run_infer_program str env_with_things with
     | Ok (env, names) -> pprint_result env names
     (* | Ok env -> printf "%a\n" TypeEnv.pp_env env *)
     | Error err -> printf "%a" pp_inf_err err)
;;

(*
   |  ____      U  ___ u      _   _     U  ___ u _____        _____   __   __  ____   U _____ u
   | |  _"\      \/"_ \/     | \ |"|     \/"_ \/|_ " _|      |_ " _|  \ \ / /U|  _"\ u\| ___"|/
   |/| | | |     | | | |    <|  \| |>    | | | |  | |          | |     \ V / \| |_) |/ |  _|'
   |U| |_| |\.-,_| |_| |    U| |\  |u.-,_| |_| | /| |\        /| |\   U_|"|_u |  __/   | |___
   | |____/ u \_)-\___/      |_| \_|  \_)-\___/ u |_|U       u |_|U     |_|   |_|      |_____|
   |  |||_         \\        ||   \\,-.    \\   _// \\_      _// \\_.-,//|(_  ||>>_    <<   >>
   | (__)_)       (__)       (_")  (_/    (__) (__) (__)    (__) (__)\_) (__)(__)__)  (__) (__)
*)

(*PASSED*)
let%expect_test "001" =
  parse_and_infer_result {|
let recfac n = if n<=1 then 1 else n * fac (n-1);;|};
  [%expect {| Unbound_variable: "fac" |}]
;;

(*PASSED*)
let%expect_test "002" =
  parse_and_infer_result {|
let main = if true then 1 else false;;|};
  [%expect {| Unification_failed: int # bool |}]
;;

(*PASSED*)
let%expect_test "003  " =
  parse_and_infer_result
    {|
let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f));;|};
  [%expect {| Occurs_check: 'c and 'c -> 'b |}]
;;

(*PASSED*)
let%expect_test "004  " =
  parse_and_infer_result {|
let _1 =
  (fun f -> (f 1, f true)) (fun x -> x);;|};
  [%expect {| Unification_failed: int # bool |}]
;;

(*PASSED*)
let%expect_test "005  " =
  parse_and_infer_result
    {|
let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1;;|};
  [%expect {| Unification_failed: string # int |}]
;;

(*PASSED*)
let%expect_test "015" =
  parse_and_infer_result {|let rec (a,b) = (a,b);;|};
  [%expect {| Wrong right value in rec |}]
;;

(*PASSED*)
let%expect_test "016" =
  parse_and_infer_result {|let a, _ = 1, 2, 3;;|};
  [%expect {| Unification_failed: int * int * int # 'b * 'a |}]
;;

(*PASSED*)
let%expect_test "091.1" =
  parse_and_infer_result {|let [a] = (fun x -> x);;|};
  [%expect {| Unification_failed: 'b -> 'b # 'c list |}]
;;

(*PASSED*)
let%expect_test "097.2" =
  parse_and_infer_result {|let () = (fun x -> x);;|};
  [%expect {| Unification_failed: 'b -> 'b # unit |}]
;;

(*PASSED*)
let%expect_test "098" =
  parse_and_infer_result {|let rec x = x + 1;;|};
  [%expect {| Wrong right value in rec |}]
;;

(*PASSED*)
let%expect_test "098" =
  parse_and_infer_result {|let rec x::[] = [1];;|};
  [%expect {| Wrong right value in rec |}]
;;

(*
   |  _____   __   __  ____   U _____ u ____
   | |_ " _|  \ \ / /U|  _"\ u\| ___"|/|  _"\
   |   | |     \ V / \| |_) |/ |  _|" /| | | |
   |  /| |\   U_|"|_u |  __/   | |___ U| |_| |\
   | u |_|U     |_|   |_|      |_____| |____/ u
   | _// \\_.-,//|(_  ||>>_    <<   >>  |||_
   |(__) (__)\_) (__)(__)__)  (__) (__)(__)_)
*)

(*PASSED*)
let%expect_test "001fact without builtin" =
  parse_and_infer_result {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);;|};
  [%expect {|
    val fac : int -> int |}]
;;

(*passed*)
let%expect_test "001fact with builtin" =
  parse_and_infer_result
    {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);;
 let main =
  let () = print_int (fac 4) in
  0;;|};
  [%expect {|
    val fac : int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "002fact without builtin" =
  parse_and_infer_result
    {|
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n));;|};
  [%expect {|
    val fac_cps : int -> (int -> 'a) -> 'a |}]
;;

(*passed*)
let%expect_test "002fact builtin" =
  parse_and_infer_result
    {|
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n));; let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0;;|};
  [%expect {|
    val fac_cps : int -> (int -> 'a) -> 'a
    val main : int |}]
;;

(*PASSED*)
let%expect_test "003fib without builtin" =
  parse_and_infer_result
    {|
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1;;
    let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2);;|};
  [%expect {|
    val fib_acc : int -> int -> int -> int
    val fib : int -> int |}]
;;

(*passed*)
let%expect_test "003fib builtin" =
  parse_and_infer_result
    {|
let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1;;
    let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2);;
  let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0;;|};
  [%expect
    {|
    val fib_acc : int -> int -> int -> int
    val fib : int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "004" =
  parse_and_infer_result
    {|
let wrap f = if 1 = 1 then f else f;;

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0;;

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j;;

let main =
  let rez =
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0;;|};
  [%expect
    {|
    val wrap : 'a -> 'a
    val test3 : int -> int -> int -> int
    val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "005" =
  parse_and_infer_result
    {|
let rec fix f x = f (fix f) x;;

let fac self n = if n<=1 then 1 else n * self (n-1);;|};
  [%expect
    {|
    val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    val fac : (int -> int) -> int -> int |}]
;;

(*PASSED*)
let%expect_test "005" =
  parse_and_infer_result
    {|
let rec fix f x = f (fix f) x;;

let fac self n = if n<=1 then 1 else n * self (n-1);;

let main =
  let () = print_int (fix fac 6) in
  0;;|};
  [%expect
    {|
    val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    val fac : (int -> int) -> int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "006" =
  parse_and_infer_result
    {|let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10);;
    let foo x = foo true (foo false (foo true (foo false x)));;
    let main =
  let () = print_int (foo 11) in
  0;;|};
  [%expect {|
    val foo : int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "006.2" =
  parse_and_infer_result
    {|let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + b * c;;

let main =
  let foo = foo 1 in
  let foo = foo 2 in
  let foo = foo 3 in
  let () = print_int foo in
  0;;|};
  [%expect {|
    val foo : int -> int -> int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "006.3" =
  parse_and_infer_result
    {|let foo a =
  let () = print_int a in fun b ->
  let () = print_int b in fun c ->
  print_int c;;

let main =
  let () = foo 4 8 9 in
  0;;|};
  [%expect {|
    val foo : int -> int -> int -> unit
    val main : int |}]
;;

(*PASSED*)
let%expect_test "007" =
  parse_and_infer_result
    {|let _start () () a () b _c () d __ =
  let () = print_int (a+b) in
  let () = print_int __ in
  a*b / _c + d;;

let main =
  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555));;|};
  [%expect
    {|
    val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
    val main : unit |}]
;;

(*PASSED*)
let%expect_test "008" =
  parse_and_infer_result
    {|
let addi = fun f g x -> (f x (g x: bool) : int);;
let main =
  let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> _start/2 = 0) 4) in
  0;;|};
  [%expect
    {|
    val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "009" =
  parse_and_infer_result {|
let temp =
  let f = fun x -> x in
  (f 1, f true);;|};
  [%expect {|
    val temp : int * bool |}]
;;

(*PASSED*)
let%expect_test "010" =
  parse_and_infer_result
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
    |};
  [%expect
    {|
    val _1 : int -> int -> int * 'a -> bool
    val _2 : int
    val _3 : (int * string) option
    val _4 : int -> 'a
    val _5 : int
    val _6 : 'a option -> 'a
    val int_of_option : int option -> int
    val _42 : int -> bool
    val id2 : 'b -> 'b
    val id1 : 'a -> 'a |}]
;;

(*PASSED*)
let%expect_test "015" =
  parse_and_infer_result
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
  let () = print_int (odd 3) in
  let () = print_int (even 4) in
  0
|};
  [%expect
    {|
    val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
    val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
    val feven : 'a * (int -> int) -> int -> int
    val fodd : (int -> int) * 'a -> int -> int
    val tie : (int -> int) * (int -> int)
    val meven : int -> int
    val modd : int -> int
    val main : int |}]
;;

(*PASSED*)
let%expect_test "016" =
  parse_and_infer_result
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
    val length : 'a list -> int
    val length_tail : 'a list -> int
    val map : ('a -> 'b) -> 'a list -> 'b list
    val append : 'a list -> 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val iter : ('a -> unit) -> 'a list -> unit
    val cartesian : 'a list -> 'b list -> ('a * 'b) list
    val main : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5+5;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5+5;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5/5;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5-5;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5*5;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5>=5;;|};
  [%expect {| - : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5<=5;;|};
  [%expect {| - : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5>5;;|};
  [%expect {| - : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5<5;;|};
  [%expect {| - : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|true || false;;|};
  [%expect {| - : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|false && false;;|};
  [%expect {| - : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|let id x = x in
let homka = Some id in
match homka with
| Some f -> f 42, f "42";;|};
  [%expect {| - : int * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|function 5 -> 'c';;|};
  [%expect {| - : int -> char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|function 5 -> 'c' | 22 -> 'k';;|};
  [%expect {| - : int -> char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|function 5 -> 'c' | 22 -> 23;;|};
  [%expect {| Unification_failed: char # int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|function 5 -> 'c' | 'c' -> 23;;|};
  [%expect {| Unification_failed: int # char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|let id x = x in
let homkaOBOLTUS = id in
match homkaOBOLTUS with
|  f -> f 42, f "42";;|};
  [%expect {| - : int * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5=5 then 1 else 5;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5=5 then "aboba";;|};
  [%expect {| - : string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5 then "aboba";;|};
  [%expect {| Unification_failed: int # bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if true then "andreichik" else 7;;|};
  [%expect {| Unification_failed: string # int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|(5,6,7);;|};
  [%expect {| - : int * int * int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|(5,7);;|};
  [%expect {| - : int * int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|('c',7,false,"andreichik");;|};
  [%expect {| - : char * int * bool * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|function 
5 -> 'c' 
| 67 -> 'b' 
| 68 -> 'h' 
| 69 -> 's' 
| 89 -> 'a';;|};
  [%expect {| - : int -> char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|match 9 with
|5 -> 5 
|6 -> 5
|7 -> 7
|7 -> 1
|7 -> 1
|7 -> 1
| _ -> 3
;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> fun y -> y+x;;|};
  [%expect {| - : int -> int -> int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> fun y -> fun z -> fun w -> y + x * z / w;;|};
  [%expect {| - : int -> int -> int -> int -> int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 1
let y = 2
let z = 3|};
  [%expect {|
    val x : int
    val y : int
    val z : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let y = 5|};
  [%expect {|
    val y : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let _ = (2,5) and y = ("a","b")|};
  [%expect {|
    val y : string * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = (2,5) and y = ("a","b")|};
  [%expect {|
    val y : string * string
    val x : int * int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let (x,y) = (2,5) and z = ("a","b")
   let f = x|};
  [%expect
    {|
    val z : string * string
    val y : int
    val x : int
    val f : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let (x,y) = (2,'c');;|};
  [%expect {|
    val y : char
    val x : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 5=5;;|};
  [%expect {|
    val x : bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let rec g () = g ();;|};
  [%expect {|
    val g : unit -> 'a |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 6 and y = 6 in x + y;;|};
  [%expect {| - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let rec f x = f x;;|};
  [%expect {|
    val f : 'a -> 'b |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let f = fun x -> x;;|};
  [%expect {|
    val f : 'a -> 'a |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = 5;;
let 5 = x;;|};
  [%expect {|
    val x : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = (6: char);;|};
  [%expect {| Unification_failed: int # char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = ()|};
  [%expect {|
    val x : unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|
let square x = x * x;;
let id = fun x -> x in (id square) (id 123);;|};
  [%expect {|
    val square : int -> int
    - : int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|
let rec meven n = if n = 0 then 1 else modd (n - 1)
   and modd n = if n = 0 then 1 else meven (n - 1)
;;|};
  [%expect {|
    val meven : int -> int
    val modd : int -> int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = x + x;;|};
  [%expect {|
    val f : int -> int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int)(y: int) = x + y;;|};
  [%expect {|
    val f : int -> int -> int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = x || x;;|};
  [%expect {| Unification_failed: int # bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = function 5 -> true | 6 -> false;;|};
  [%expect {|
    val f : int -> int -> bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let (f: int -> bool) = function 5 -> true | 6 -> false;;|};
  [%expect {|
    val f : int -> bool |}]
;;

let%expect_test "Simplest ADT" =
  parse_and_infer_result {|
  type shape = Circle 
  let x = Circle
|};
  [%expect {|
    val x : shape |}]
;;

let%expect_test "ADT of" =
  parse_and_infer_result
    {|
  type shape = Circle of int ;;
  type 'a koka = Circle of int ;;
  let x = Circle 5
|};
  [%expect {|
    val x : 'a koka |}]
;;

let%expect_test "ADT of few" =
  parse_and_infer_result
    {|
  type shape = 
  Circle of int
| Rectangle of char 
| Triangle of int*int
;;
let x = 10;;
let Circle (5,5) = Circle x;;
|};
  [%expect {| Unification_failed: int # int * int |}]
;;

let%expect_test "ADT with poly" =
  parse_and_infer_result
    {|
  type 'a shape = Circle of int
  | Rectangle of int * int
  | Square of int
;;
let x = 10
let y = Circle x
let (z: int shape) = Rectangle (2,5)
let q = Square 34985734895
|};
  [%expect
    {|
      val x : int
      val y : 'a shape
      val z : int shape
      val q : 'a shape |}]
;;

let%expect_test "ADT with poly constraint" =
  parse_and_infer_result
    {|
  type 'a shape = Circle of int
  | Rectangle of char * int
  | Square of int * 'a * 'a
;;
let (x: shape) = Circle 5;;
|};
  [%expect {| Unification_failed: 'a shape # shape |}]
;;

let%expect_test "ADT with constraint" =
  parse_and_infer_result
    {|
  type 'a shape = Circle of int
  | Rectangle of char * int
  | Square of int * 'a * 'a
;;
let (x: (int,int) shape) = Circle 5;;
|};
  [%expect {| Unification_failed: 'a shape # int int shape |}]
;;

let%expect_test "ADT with constraint exp" =
  parse_and_infer_result
    {|
  type 'a shape = Circle of int
  | Rectangle of char * int
  | Square of int * 'a * 'a
;;
let y = Circle 5;;
let (x: char shape) = y;;
|};
  [%expect {|
      val y : 'a shape
      val x : char shape |}]
;;

let%expect_test "ADT arity" =
  parse_and_infer_result {|
type 'a foo = Foo
type bar = Bar of foo

|};
  [%expect {|
    Arity_mismatch |}]
;;

let%expect_test "ADT arity" =
  parse_and_infer_result
    {|
type 'a foo = Foo
type 'a bar = Bar of 'a foo
let x = Bar Foo
|};
  [%expect {|
    val x : 'a bar |}]
;;

let%expect_test "alot" =
  parse_and_infer_result
    {|
let f q w e r t y u i o p a s d g h j k l z x c v b n m qq ww ee rr tt yy uu ii oo pp aa ss dd ff gg hh jj kk ll zz xx cc vv = 5;;|};
  [%expect
    {|
      val f : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> 'aa -> 'bb -> 'cc -> 'dd -> 'ee -> 'ff -> 'gg -> 'hh -> 'ii -> 'jj -> 'kk -> 'll -> 'mm -> 'nn -> 'oo -> 'pp -> 'qq -> 'rr -> 'ss -> 'tt -> 'uu -> 'vv -> int |}]
;;
