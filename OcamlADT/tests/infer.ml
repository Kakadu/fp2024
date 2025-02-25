open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes

let parse_and_infer_result program =
  match parse_str program with
  | str ->
    (match run_infer_program str env_with_print_funs with
     | Ok env -> printf "\nres:\n%a" TypeEnv.pp_env env
     | Error err -> printf "%a" pp_inf_err err)
;;

let%expect_test "zero" =
  parse_and_infer_result {|fun x -> x;;|};
  [%expect
    {|
    res:
    "-": '0 -> '0
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = x+x;;|};
  [%expect {| Unbound_variable: "x" |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let f x = x+x;;|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
  [%expect
    {|
    res:
    "-": int * string
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|function 5 -> 'c';;|};
  [%expect
    {|
    res:
    "-": int -> char
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
  [%expect
    {|
    res:
    "-": int * string
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|let x = 1;;
let y = 2;;
let z = 3;;
(x,y,z) = (5,6,7);;|};
  [%expect
    {|
    res:
    "-": bool
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
  [%expect
    {|
    res:
    "-": int -> char
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
  [%expect
    {|
    res:
    "-": int
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
let x = (6: char);;|};
  [%expect {| Unification_failed: int # char |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {|
() = print_int 5;;|};
  [%expect
    {|
    res:
    "-": bool
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "f": int -> int -> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = x || x;;|};
  [%expect {| Unification_failed: int # bool |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f (x: int) = function 5 -> true | 6 -> false;;|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "f": int -> int -> bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "zero" =
  parse_and_infer_result {| let f = f "a" in f 5;;|};
  [%expect {| Unbound_variable: "f" |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result {| let (f: int -> bool) = function 5 -> true | 6 -> false;;|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "f": int -> bool
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*KAKADU TYPE BEAT*)
(*PASSED*)
let%expect_test "001fact without builtin" =
  parse_and_infer_result {|
let rec fac n = if n<=1 then 1 else n * fac (n-1);;|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "fac_cps": [ 8; ]. int -> (int -> '8) -> '8
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "fac_cps": [ 8; ]. int -> (int -> '8) -> '8
    "main": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
      let x, Some f = 1, Some "p1onerka was here" in
      x
    ;;
    
    let _3 = Some (1, "hi");;
    let _4 =
      let rec f x = f 5 in
      f
    ;;
    
    let id1, id2 =
      let id x = x in
      id, id
    ;;
    let a_42 = function
      | 42 -> true
      | _ -> false
    ;;
    
    let k_6 arg =
      match arg with
      | Some x ->
        let y = x in
        y
    ;;
    let int_of_option = function
      | Some x -> x
      | None -> 0
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
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "_1": [ 3; ]. int -> int -> int * '3 -> bool
    "_2": int
    "_3": (int * string) option
    "_4": [ 18; ]. int -> '18
    "a_42": int -> bool
    "aaa_5": int
    "id1": [ 22; 23; ]. '22 -> '22
    "id2": [ 22; 23; ]. '23 -> '23
    "int_of_option": int option -> int
    "k_6": [ 31; ]. '31 option -> '31
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(* *)
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
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
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
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "append": [ 90; ]. '90 list -> '90 list -> '90 list
    "cartesian": [ 127; 135; ]. '127 list -> '135 list -> ('127 * '135) list
    "concat": [ 110; ]. '110 list list -> '110 list
    "iter": [ 116; ]. ('116 -> unit) -> '116 list -> unit
    "length": [ 3; ]. '3 list -> int
    "length_tail": [ 18; ]. '18 list -> int
    "main": int
    "map": [ 25; 26; ]. ('25 -> '26) -> '25 list -> '26 list
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*KAKADU DO NOT TYPE BEAT*)

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
  [%expect {| Occurs_check: 1 and '1 -> '3 |}]
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

(*FAILED*)
let%expect_test "015" =
  parse_and_infer_result {|let rec (a,b) = (a,b);;|};
  [%expect {| Not supported syntax |}]
;;

(*PASSED*)
let%expect_test "016" =
  parse_and_infer_result {|let a, _ = 1, 2, 3;;|};
  [%expect {| Unification_failed: int * int * int # '0 * '1 |}]
;;

(*FAILED*)
let%expect_test "091.1" =
  parse_and_infer_result {|let [a] = (fun x -> x);;|};
  [%expect {| Unification_failed: '0 -> '0 # '3 list |}]
;;

(*PASSED*)
let%expect_test "097.2" =
  parse_and_infer_result {|let () = (fun x -> x);;|};
  [%expect {| Unification_failed: '0 -> '0 # unit |}]
;;

(*PASSED*)
let%expect_test "098" =
  parse_and_infer_result {|let rec x = x + 1;;|};
  [%expect {| Wrong rec |}]
;;

let%expect_test "098" =
  parse_and_infer_result {|let rec x::[] = [1];;|};
  [%expect {| Not supported syntax |}]
;;

(*ADT*)
let%expect_test "Simplest ADT" =
  parse_and_infer_result {|
  type shape = Circle ;;
|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "Circle": [ 0; ]. shape
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "ADT of" =
  parse_and_infer_result
    {|
  type shape = Circle of int ;;
  type ('a,'b) koka = Circle of int ;;
|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "Circle": [ 3; ]. int -> '2 '1 koka
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "a": '2
    "b": '1
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
let x = 10;;
let Circle 5 = Circle 5;;
|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "Circle": [ 1; ]. int -> '0 shape
    "None": [ a; ]. 'a option
    "Rectangle": [ 2; ]. int * int -> '0 shape
    "Some": [ a; ]. 'a -> 'a option
    "Square": [ 3; ]. int -> '0 shape
    "[]": [ a; ]. 'a list
    "a": '0
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int |}]
;;

let%expect_test "ADT with poly2" =
  parse_and_infer_result {|
  type 'a shape = 
   Square of int;;
|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "Square": [ 1; ]. int -> '0 shape
    "[]": [ a; ]. 'a list
    "a": '0
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

let%expect_test "ADT with poly3" =
  parse_and_infer_result
    {|
  type 'a shape = Circle of int
  | Rectangle of char * int
  | Square of int * 'a * 'a
;;
|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "Circle": [ 1; ]. int -> '0 shape
    "None": [ a; ]. 'a option
    "Rectangle": [ 2; ]. char * int -> '0 shape
    "Some": [ a; ]. 'a -> 'a option
    "Square": [ 3; ]. int * 'a * 'a -> '0 shape
    "[]": [ a; ]. 'a list
    "a": '0
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
  [%expect {| Unification_failed: '0 shape # shape |}]
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
  [%expect {| Unification_failed: '0 shape # int int shape |}]
;;

let%expect_test "ADT with constraint exp" =
  parse_and_infer_result
    {|
  type 'a shape = Circle of int
  | Rectangle of char * int
  | Square of int * 'a * 'a
;;
let y = Circle 5;;
let (x: (int) shape) = y;;
|};
  [%expect
    {|
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "Circle": [ 1; ]. int -> int shape
    "None": [ a; ]. 'a option
    "Rectangle": [ 2; ]. char * int -> int shape
    "Some": [ a; ]. 'a -> 'a option
    "Square": [ 3; ]. int * 'a * 'a -> int shape
    "[]": [ a; ]. 'a list
    "a": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit
    "x": int shape
    "y": int shape |}]
;;

(*BUG*)
let%expect_test "ADT arity" =
  parse_and_infer_result {|
type 'a foo = Foo
type bar = Bar of foo

|};
  [%expect {|
    Arity_mismatch |}]
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
    res:
    "::": [ a; ]. 'a * 'a list -> 'a list
    "None": [ a; ]. 'a option
    "Some": [ a; ]. 'a -> 'a option
    "[]": [ a; ]. 'a list
    "fac_cps": [ 8; ]. int -> (int -> '8) -> '8
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;
