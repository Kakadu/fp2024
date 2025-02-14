open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes
open Ocamladt_lib.Ast.TypeExpr
(*
   let infer_result exp  =
    (match run_infer_expr exp TypeEnv.empty with
     | Ok (_, env) -> printf "%a" pprint_type env
     | Error err -> printf "%a" pp_inf_err err)
;;


let%expect_test _ =
  let _ = infer_result 
  (Exp_let (Nonrecursive,
     ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) },
      []),
     (Exp_constant (Const_integer 1))))
 in
  [%expect{| int |}]
;;

let%expect_test _ =
  let _ = infer_result 
  (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_string "a")) }, []),
           (Exp_ident "x")))
 in
  [%expect{| string |}]
;;


let%expect_test _ =
  let _ = infer_result 
  (Exp_constant (Const_integer 1))
 in
  [%expect{| int |}]
;;

let%expect_test _ =
  let _ = infer_result 
  (Exp_constant (Const_char 'a'))
 in
  [%expect{| char |}]
;;

let%expect_test _ =
  let _ = infer_result 
  (Exp_constant (Const_string "str"))
 in
  [%expect{| string |}]
;;
*)

(*str item tetst*)
let parse_and_infer_result program =
  match parse_str program with
  | str ->
    (match run_infer_program str env_with_print_funs with
     | Ok env -> printf "\nres:\n%a" TypeEnv.pp_env env
     (* | Ok (_,[]) -> failwith "abibi" *)
     | Error err -> printf "%a" pp_inf_err err)
  | _ -> failwith "aboba"
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> x;;|};
  [%expect
    {|
    res:
    "-": '0 -> '0
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = x+x;;|};
  [%expect {|
    Unbound_variable: "x" |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let f x = x+x;;|};
  [%expect
    {|
    res:
    "f": int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5+5;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5/5;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5-5;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5*5;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5>=5;;|};
  [%expect
    {|
    res:
    "-": bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5<=5;;|};
  [%expect
    {|
    res:
    "-": bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5>5;;|};
  [%expect
    {|
    res:
    "-": bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|5<5;;|};
  [%expect
    {|
    res:
    "-": bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|let id x = x in
let homka = Some id in
match homka with
| Some f -> f 42, f "42";;|};
  [%expect{|
    res:
    "-": int * string
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|let id x = x in
let homkaOBOLTUS = id in
match homkaOBOLTUS with
|  f -> f 42, f "42";;|};
  [%expect{|
    res:
    "-": int * string
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|fun id ->
  let homkaOBOLTIMUSPRIME = id in
  match homkaOBOLTIMUSPRIME with
  |  f -> f 42, f "42";;|};
  [%expect{|
    Unification_failed: int # string |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result {|let x = 1;;
let y = 2;;
let z = 3;;
(x,y,z) = (5,6,7);;|};
  [%expect
    {|
    res:
    "-": bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int
    "y": int
    "z": int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5=5 then 1 else 5;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|if 5=5 then "aboba";;|};
  [%expect
    {|
    res:
    "-": string
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|(5,6,7);;|};
  [%expect
    {|
    res:
    "-": int * int * int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|function 
5 -> 'c' 
| 67 -> 'b' 
| 68 -> 'h' 
| 69 -> 's' 
| 89 -> 'a';;|};
  [%expect{|
    res:
    "-": char
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
  [%expect{|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> fun y -> y+x;;|};
  [%expect
    {|
    res:
    "-": int -> int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*ALL "LET" ITEMS*)

let%expect_test "zero" =
  parse_and_infer_result {|let y = 5;;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "y": int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let _ = (2,5) and y = ("a","b");;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "y": string * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = (2,5) and y = ("a","b");;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int * int
    "y": string * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let (x,y) = (2,5) and z = ("a","b");; let f = x;;|};
  [%expect
    {|
    res:
    "f": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int
    "y": int
    "z": string * string |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let (x,y) = (2,'c');;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int
    "y": char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 5=5;;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 6 and y = 6 in x + y;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let rec f x = f x;;|};
  [%expect
    {|
    res:
    "f": [ 1; 2; ]. '1 -> '2
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let f = fun x -> x;;|};
  [%expect
    {|
    res:
    "f": [ 0; ]. '0 -> '0
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = 5;;
let 5 = x;;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = (6: char);;|};
  [%expect {|
    Unification_failed: int # char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
() = print_int 5;;|};
  [%expect
    {|
    res:
    "-": bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|
let square x = x * x;;
let id = fun x -> x in (id square) (id 123);;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "square": int -> int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result
    {|
let rec meven n = if n = 0 then 1 else modd (n - 1)
   and modd n = if n = 0 then 1 else meven (n - 1)
;;|};
  [%expect
    {|
    res:
    "meven": int -> int
    "modd": int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = x + x;;|};
  [%expect
    {|
    res:
    "f": int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int)(y: int) = x + y;;|};
  [%expect
    {|
    res:
    "f": int -> int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = x || x;;|};
  [%expect {|
    Unification_failed: int # bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = function 5 -> true | 6 -> false;;|};
  [%expect{|
    res:
    "f": int -> bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f = f "a" in f 5;;|};
  [%expect {|
    Unbound_variable: "f" |}]
;;
(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result {| let (f: int -> bool) = function 5 -> true | 6 -> false;;|};
  [%expect{|
    Unification_failed: bool # int -> bool |}]
;;

(*KAKADU TYPE BEAT*)
(*PASSED*)
let%expect_test "001fact without builtin" =
  parse_and_infer_result {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);;|};
  [%expect
    {|
    res:
    "fac": int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*passed*)
let%expect_test "001fact with builtin" =
  parse_and_infer_result
    {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);;
 let main =
  let () = print_int (fac 4) in
  0;;|};
  [%expect
    {|
    res:
    "fac": int -> int
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*PASSED*)
let%expect_test "002fact without builtin" =
  parse_and_infer_result
    {|
let rec fac_cps n k =
  if n=1 then k 1 else
  fac_cps (n-1) (fun p -> k (p*n));;|};
  [%expect
    {|
    Occurs_check |}]
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
  [%expect
    {|
    Occurs_check |}]
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
  [%expect
    {|
    res:
    "fib": int -> int
    "fib_acc": int -> int -> int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    res:
    "fib": int -> int
    "fib_acc": int -> int -> int -> int
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    res:
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "test10": int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    "test3": int -> int -> int -> int
    "wrap": [ 0; ]. '0 -> '0 |}]
;;

(*PASSED*)
let%expect_test "005" =
  parse_and_infer_result
    {|
let rec fix f x = f (fix f) x;;

let fac self n = if n<=1 then 1 else n * self (n-1);;|};
  [%expect
    {|
    res:
    "fac": (int -> int) -> int -> int
    "fix": [ 2; 5; ]. (('2 -> '5) -> '2 -> '5) -> '2 -> '5
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    res:
    "fac": (int -> int) -> int -> int
    "fix": [ 2; 5; ]. (('2 -> '5) -> '2 -> '5) -> '2 -> '5
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*PASSED?*)
let%expect_test "006" =
  parse_and_infer_result
    {|let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10);;
    let foo x = foo true (foo false (foo true (foo false x)));;
    let main =
  let () = print_int (foo 11) in
  0;;|};
  [%expect
    {|
    res:
    "foo": int -> int
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
  [%expect
    {|
    res:
    "foo": int -> int -> int -> int
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
  [%expect
    {|
    res:
    "foo": int -> int -> int -> unit
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    res:
    "_start": unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
    "main": unit
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    res:
    "addi": [ 2; ]. ('2 -> bool -> int) -> ('2 -> bool) -> '2 -> int
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*PASSED*)
let%expect_test "009" =
  parse_and_infer_result {|
let temp =
  let f = fun x -> x in
  (f 1, f true);;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "temp": int * bool |}]
;;

(*FAILED _5*)
let%expect_test "010" =
  parse_and_infer_result
    {|
let _1 = fun x y (a, _) -> (x + y - a) = 1;;
let _2 =
    let x, Some f = 1, Some ( "p1onerka was here" )
    in x;;
    let _3 =  Some (1, "hi");;
    let _4 = let rec f x = f 5 in f;;
 let id1, id2 = let id x = x in (id, id);;
 let a_42 = function
  | 42 -> true
  | _ -> false
;;
let int_of_option = function
  | Some x -> x
  | None -> 0
;;
let k_6 arg =
  match arg with
  | Some x -> let y = x in y
;;

let aaa_5 =
  let id x = x in
  match Some id with
  | Some f ->
    let _ = f "42" in
    f 42
  | None -> 0
;;
    |};
  [%expect{|
    res:
    "_1": [ 3; ]. int -> int -> int * '3 -> bool
    "_2": int
    "_3": int * stringoption
    "_4": [ 13; ]. int -> '13
    "a_42": bool
    "aaa_5": int
    "id1": [ 17; 18; ]. '17 -> '17
    "id2": [ 17; 18; ]. '18 -> '18
    "int_of_option": int
    "k_6": [ 31; ]. '31option -> '31
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;


(*FAILED *)
let%expect_test "015" =
  parse_and_infer_result
    {|
let rec fix f x = f (fix f) x;;
let map f p = let (a,b) = p in (f a, f b);;
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l;;
let feven p n =
  let (e, o) = p in
  if n = 0 then 1 else o (n - 1);;
let fodd p n =
  let (e, o) = p in
  if n = 0 then 0 else e (n - 1);;
  let tie = fixpoly (feven, fodd);; 
  let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1);;
let main =
  let () = print_int (modd 1) in
  let () = print_int (meven 2) in
  let (even,odd) = tie in
  let () = print_int (odd 3) in
  let () = print_int (even 4) in
  0;;
|};
  [%expect{|
    res:
    "feven": [ 32; ]. '32 * (int -> int) -> int -> int
    "fix": [ 2; 5; ]. (('2 -> '5) -> '2 -> '5) -> '2 -> '5
    "fixpoly": [ 21; 24; ]. (('21 -> '24) * ('21 -> '24) -> '21 -> '24) * (('21 -> '24) * ('21 -> '24) -> '21 -> '24) -> ('21 -> '24) * ('21 -> '24)
    "fodd": [ 40; ]. (int -> int) * '40 -> int -> int
    "main": int
    "map": [ 11; 9; ]. ('9 -> '11) -> '9 * '9 -> '11 * '11
    "meven": int -> int
    "modd": int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "tie": (int -> int) * (int -> int) |}]
;;

  
(*KAKADU DO NOT TYPE BEAT*)

(*PASSED*)
let%expect_test "001" =
  parse_and_infer_result {|
let recfac n = if n<=1 then 1 else n * fac (n-1);;|};
  [%expect {|
    Unbound_variable: "fac" |}]
;;

(*PASSED*)
let%expect_test "002" =
  parse_and_infer_result {|
let main = if true then 1 else false;;|};
  [%expect {|
    Unification_failed: int # bool |}]
;;

(*PASSED*)
let%expect_test "003  " =
  parse_and_infer_result
    {|
let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f));;|};
  [%expect {|
    Occurs_check |}]
;;

(*PASSED*)
let%expect_test "004  " =
  parse_and_infer_result {|
let _1 =
  (fun f -> (f 1, f true)) (fun x -> x);;|};
  [%expect {|
    Unification_failed: int # bool |}]
;;

(*PASSED*)
let%expect_test "005  " =
  parse_and_infer_result
    {|
let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1;;|};
  [%expect{|
    Unification_failed: string # int |}]
;;

(*PASSED*)
let%expect_test "015" =
  parse_and_infer_result {|let rec (a,b) = (a,b);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure abobi)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 60, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 821, characters 2-40
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-52
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 1030, characters 2-52
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*PASSED*)
let%expect_test "016" =
  parse_and_infer_result {|let a, _ = 1, 2, 3;;|};
  [%expect {|
    Unification_failed: int * int * int # '0 * '1 |}]
;;

(*FAILED*)
let%expect_test "091.1" =
  parse_and_infer_result {|let [a] = (fun x -> x);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 58, characters 8-25
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 1058, characters 2-53
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

(*PASSED*)
let%expect_test "097.2" =
  parse_and_infer_result {|let () = (fun x -> x);;|};
  [%expect {|
    Unification_failed: '0 -> '0 # unit |}]
;;

(*PASSED*)
let%expect_test "098" =
  parse_and_infer_result {|let rec x = x + 1;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "wrong rec")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 60, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 821, characters 2-40
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-52
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 1082, characters 2-48
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "098" =
  parse_and_infer_result {|let rec x::[] = [1];;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 58, characters 8-25
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 1102, characters 2-50
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
