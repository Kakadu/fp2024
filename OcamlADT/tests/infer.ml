open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes

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


(*str item tetst*)
let parse_and_infer_result program  =
  match parse_str program with
  | str ->
    (match run_infer_program str TypeEnv.empty with
     | Ok env -> printf "\nres:\n %a" TypeEnv.pp_env env
     (* | Ok (_,[]) -> failwith "abibi" *)
     | Error err -> printf "%a" pp_inf_err err)
  | _ -> failwith "aboba"
;;

let%expect_test "zero" =
parse_and_infer_result {|fun x -> x;;|};
  [%expect{|
    res:
     "-": '0 -> '0 |}]
;;


(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|let x = x+x;;|};
  [%expect{|
    Unbound_variable: "x" |}]
;;

(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|let f x = x+x;;|};
  [%expect{|
    res:
     "f": int -> int |}]
;;
(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|5+5;;|};
  [%expect{|
    res:
     "-": int |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5/5;;|};
  [%expect{|
    res:
     "-": int |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5-5;;|};
  [%expect{|
    res:
     "-": int |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5*5;;|};
  [%expect{|
    res:
     "-": int |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5>=5;;|};
  [%expect{|
    res:
     "-": bool |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5<=5;;|};
  [%expect{|
    res:
     "-": bool |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5>5;;|};
  [%expect{|
    res:
     "-": bool |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|5<5;;|};
  [%expect{|
    res:
     "-": bool |}]
;;


let%expect_test "zero" =
parse_and_infer_result {|let x = 5 in x;;|};
  [%expect{|
    res:
     "-": int |}]
;;

(*BUG*)
let%expect_test "zero" =
parse_and_infer_result {|let x = 1;;
let y = 2;;
let z = 3;;
(x,y,z) = (5,6,7);;|};
  [%expect{|
    res:
     "-": bool
    "x": int
    "y": int
    "z": int |}]
;;



let%expect_test "zero" =
parse_and_infer_result {|if 5=5 then 1 else 5;;|};
  [%expect{|
    res:
     "-": int |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|if 5=5 then "aboba";;|};
  [%expect{|
    res:
     "-": string |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|(5,6,7);;|};
  [%expect{|
    res:
     "-": int * int * int |}]
;;
let%expect_test "zero" =
parse_and_infer_result {|function 
5 -> 'c' 
| 67 -> 'b' 
| 68 -> 'h' 
| 69 -> 's' 
| 89 -> 'a';;|};
  [%expect{|
    res:
     "-": char |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|match 9 with
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
     "-": int |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|fun x -> fun y -> y+x;;|};
  [%expect{|
    res:
     "-": int -> int -> int |}]
;;


(*ALL "LET" ITEMS*)


let%expect_test "zero" =
parse_and_infer_result {|let x = "a" and y = 5 and z = 6;; let x = z;;|};
  [%expect{|
    res:
     "x": int
    "y": int
    "z": int |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|let _ = (2,5) and y = ("a","b");;|};
  [%expect{|
    res:
     "y": string * string |}]
;;


let%expect_test "zero" =
parse_and_infer_result {|let x = (2,5) and y = ("a","b");;|};
  [%expect{|
    res:
     "x": int * int
    "y": string * string |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|let (x,y) = (2,5) and z = ("a","b");; let f = x;;|};
  [%expect{|
    res:
     "f": int
    "x": int
    "y": int
    "z": string * string |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|let (x,y) = (2,'c') and z = 10;;|};
  [%expect{|
    res:
     "x": int
    "y": char
    "z": int |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|let x = 5=5;;|};
  [%expect{|
    res:
     "x": bool |}]
;;

let%expect_test "zero" =
parse_and_infer_result {|let rec f x = f x;;|};
  [%expect{|
    res:
     "f": '1 -> '2 |}]
;;



let%expect_test "zero" =
parse_and_infer_result {|
let x = 5;;
let 5 = x;;|};
  [%expect{|
    res:
     "x": int |}]
;;