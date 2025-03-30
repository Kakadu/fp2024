(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Type.Unification
open Type.Inference
open Type.Types
open Parse.Structure
open Parse.Expressions

let env_to_str env =
  List.map
    (fun (name, Scheme (_, ty)) ->
      if (not (Checks.is_builtin_op name)) && not (Checks.is_builtin_fun name)
      then Printf.sprintf "%s : %s\n" name (string_of_ty ty)
      else "")
    env
  |> String.concat ""
;;

let test_infer s =
  let open Format in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pprog s with
  | Ok parsed ->
    print_endline
      (try
         let new_env = infer parsed in
         env_to_str new_env
       with
       | TypeError s -> s
       | UnificationError s -> s
       | Failure s -> s)
  | Error e -> printf "Parse error: %s\n" e
;;

let test_infer_expr s =
  let open Format in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pexpr s with
  | Ok parsed ->
    print_endline
      (try
         let _, t, _ = infer_expr initial_env parsed 1 in
         string_of_ty t
       with
       | TypeError s -> s
       | UnificationError s -> s
       | Failure s -> s)
  | Error e -> printf "Parse error: %s\n" e
;;

let _, _ = test_infer, test_infer_expr

(************************** Expressions **************************)

let%expect_test _ =
  test_infer_expr "fun x -> x";
  [%expect {| 'a1 -> 'a1 |}]
;;

let%expect_test _ =
  test_infer_expr "fun x -> 1";
  [%expect {|
    'a1 -> int
  |}]
;;

let%expect_test _ =
  test_infer_expr "fun x y -> x";
  [%expect {| 'a1 -> 'a2 -> 'a1 |}]
;;

let%expect_test _ =
  test_infer_expr "let rec fact n = if n <= 1 then 1 else n * fact (n-1) in fact";
  [%expect {|
    int -> int
  |}]
;;

let%expect_test _ =
  test_infer_expr "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib";
  [%expect {| int -> int
    |}]
;;

let%expect_test _ =
  test_infer_expr "5.0<cm>";
  [%expect {|
    float<cm>
  |}]
;;

let%expect_test "List literal" =
  test_infer_expr "[1; 2; 3]";
  [%expect {|
    int list
  |}]
;;

let%expect_test _ =
  test_infer_expr "let x = 1 and y = 2 in x";
  [%expect {|
    int
  |}]
;;

let%expect_test _ =
  test_infer_expr "fun (x : int) -> x + 1";
  [%expect {|
  int -> int
|}]
;;

let%expect_test _ =
  test_infer_expr
    {| fun xs ->
       match xs with
       | [] -> 0
       | [x] -> x
       | [x; y] -> x + y |};
  [%expect {|
      int list -> int
    |}]
;;

let%expect_test _ =
  test_infer_expr "let x = 5<cm> in x + 3";
  [%expect {| Cannot unify types: int and int<cm> |}]
;;

let%expect_test _ =
  test_infer_expr
    "let rec sumList = fun xs -> match xs with [] -> 0 | [x] -> x | [x; y] -> x + y in \
     sumList";
  [%expect {| int list -> int |}]
;;

let%expect_test _ =
  test_infer_expr "let a = () in a";
  [%expect {| unit |}]
;;

let%expect_test _ =
  test_infer_expr "let a = 5 in let b = 5 in a + b";
  [%expect {| int |}]
;;

(************************** Programs **************************)

let%expect_test _ =
  test_infer {| let b = 5.0 |};
  [%expect {|
    b : float |}]
;;

(* 'a23 ? ? ? *)
let%expect_test _ =
  test_infer
    {| let a = 5.0
     let b = true
     let c = if true then 1 else 0
     let d = [1; 2; 3]
     let e = 0 :: d
     let f x y z = x + y + z
     let o x = x

     let e' x y = x :: y
      |};
  [%expect
    {|
    e' : 'a30 -> 'a30 list -> 'a30 list
    o : 'a24 -> 'a24
    f : int -> int -> int -> int
    e : int list
    d : int list
    c : int
    b : bool
    a : float
  |}]
;;

let%expect_test _ =
  test_infer
    {|
    let not = function
    | true -> false
    | false -> true
    
    let a = not false|};
  [%expect {|
    a : bool
    not : bool -> bool |}]
;;

let%expect_test _ =
  test_infer
    {|
    let a = "string"
    let b = print_string a
    let c = print_endline a
    let d = print_int (1 + 2)
    let e = print_char 'c'
    let f = print_bool (if true then true else false)
    let r = print_float 1.0|};
  [%expect
    {|
    r : unit
    f : unit
    e : unit
    d : unit
    c : unit
    b : unit
    a : string |}]
;;

(* Match is not typed as expected.
   Gets type of last rule's expression.
   Argument is also not typed correctly. *)
let%expect_test _ =
  test_infer {|
     let p x = match x with
     | hd :: tl -> hd + 1
     | _ -> -1
  |};
  [%expect {|
    p : 'a14 -> int |}]
;;
