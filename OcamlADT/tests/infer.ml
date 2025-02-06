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
    "-": 'a-> 'a
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

(*BUG*)
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

(*BUG*)
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
  parse_and_infer_result {|let x = 5 in x;;|};
  [%expect
    {|
    res:
    "-": int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
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
    "-": int  * int  * int
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
    "y": string  * string |}]
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
    "x": int  * int
    "y": string  * string |}]
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
    "z": string  * string |}]
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
    "f": 'b-> 'c
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
    "f": [ 0; ]. 'a-> 'a
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
  [%expect
    {|
    Unification_failed: int  # char |}]
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

(*bug*)
let%expect_test "zero" =
  parse_and_infer_result {|
let () = print_int 5;;|};
  [%expect
    {|
    res:
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;

(*BUG*)
let%expect_test "zero" =
  parse_and_infer_result
    {|
let rec meven n = if n = 0 then 1 else modd (n - 1)
   and modd n = if n = 0 then 1 else meven (n - 1)
   and homka n = damir 4
   and damir n = homka 5
;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure abobiks)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.(>>=) in file "lib/infer.ml", line 11, characters 18-22
  Called from Ocamladt_lib__Infer.MInfer.run in file "lib/infer.ml" (inlined), line 60, characters 18-23
  Called from Ocamladt_lib__Infer.run_infer_program.(fun) in file "lib/infer.ml", line 720, characters 2-40
  Called from Ocamladt_tests__Infer.parse_and_infer_result in file "tests/infer.ml", line 60, characters 11-52
  Called from Ocamladt_tests__Infer.(fun) in file "tests/infer.ml", line 478, characters 2-189
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
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
    res:
    "fac_cps": int -> (int -> 'i) -> 'i
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
    "fac_cps": int -> (int -> int ) -> int
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
    "wrap": [ 0; ]. 'a-> 'a |}]
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
    "fac": (int -> int ) -> int -> int
    "fix": (('c-> 'f) -> 'c-> 'f) -> 'c-> 'f
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
    "fac": (int -> int ) -> int -> int
    "fix": ((int -> int ) -> int -> int ) -> int -> int
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

(*FAILED*)
let%expect_test "008" =
  parse_and_infer_result {|
let addi = fun f g x -> (f x (g x: bool) : int);;|};
  [%expect
    {|
    res:
    "addi": [ 2; ]. ('c-> bool -> int ) -> ('c-> bool ) -> 'c-> int
    "print_bool": bool -> unit
    "print_char": char -> unit
    "print_endline": string -> unit
    "print_int": int -> unit |}]
;;
