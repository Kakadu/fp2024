(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Typedtree
open OCamlRV_lib.Infer
open OCamlRV_lib.InferCore

let test_infer s =
  let open OCamlRV_lib.Parser in
  match parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         Format.printf "val %s : %a\n" key pp_type ty)
     | Error e -> Format.printf "Infer error: %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error: %s\n" e
;;

(*---------------- Simple Expressions  -----------------*)

let%expect_test _ =
  test_infer {|
      let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;
     |};
  [%expect {| val fact : int -> int |}]
;;

let%expect_test _ =
  test_infer
    {|
    let rec fib_loop m n i = if i = 0 then m else fib_loop n (n + m) (i - 1);;
  |};
  [%expect {| val fib_loop : int -> int -> int -> int |}]
;;

let%expect_test _ =
  test_infer
    {|
    let rec fibo n = if n <= 1 then n else fibo (n - 1) + fibo (n - 2);;
  |};
  [%expect {| val fibo : int -> int |}]
;;

let%expect_test _ =
  test_infer {|let a b = if b then 1 else 2;;|};
  [%expect {| val a : bool -> int |}]
;;

let%expect_test _ =
  test_infer
    {|
  let res cond = match cond with | "firstMatch" -> 1 | "SecondMatch" -> 2| _ -> 3;;
  |};
  [%expect {| val res : string -> int |}]
;;

(*------------------List Test--------------------*)
let%expect_test _ =
  test_infer {| let list = [];;|};
  [%expect {| val list : '0 list |}]
;;

let%expect_test _ =
  test_infer {| 
     let someCons = 1::2::3::[];;
     |};
  [%expect {| val someCons : int list |}]
;;

let%expect_test _ =
  test_infer
    {| 
    let rec sum_list lst = match lst with | [] -> 0 | x::xs -> x + sum_list xs;;
     |};
  [%expect {| val sum_list : int list -> int |}]
;;

let%expect_test _ =
  test_infer
    {|let rec double_list lst = match lst with 
  | [] -> [] 
  | x::xs -> (2 * x)::double_list xs;;|};
  [%expect {| val double_list : int list -> int list|}]
;;

(*-------------------Primitves---------------------*)
let%expect_test _ =
  test_infer {| 
    let f = false;;|};
  [%expect {| val f : bool |}]
;;

let%expect_test _ =
  test_infer {| 
    let t = true;;|};
  [%expect {| val t : bool |}]
;;

let%expect_test _ =
  test_infer {| 
    let stroka = "this is string";;|};
  [%expect {| val stroka : string |}]
;;

(*ERROR*)
(*слаувик хз че тут никак*)
let%expect_test _ =
  test_infer {| 
    let l = [1;2];;|};
  [%expect {| Infer error: Not implemented |}]
;;

let%expect_test _ =
  test_infer {| 
    let l = 1::2::[];;|};
  [%expect {| val l : int list |}]
;;

let%expect_test _ =
  test_infer {| 
    let l = "1"::"2"::[];;|};
  [%expect {| val l : string list |}]
;;

let%expect_test _ =
  test_infer {| 
    let a = ();;|};
  [%expect {| val a : unit |}]
;;

let%expect_test _ =
  test_infer {| 
    let idk (fs : int) (sn : int) = fs + sn * fs;;|};
  [%expect {| val idk : int -> int -> int |}]
;;

(*----------------------------------------------*)

(*Тесты ниже нужны скорее убедиться чтобы мы всякие ошибки детектили, тк считаю что в тайпчекере важнее находить not well typed случаи*)

let%expect_test _ =
  test_infer {| 
    let a  = "hello" + 5;;|};
  [%expect {| Infer error: Unification failed on string and int |}]
;;

let%expect_test _ =
  test_infer {| 
    let a  = b + 5;;|};
  [%expect {| Infer error: Unbound variable 'b' |}]
;;

(*ERROR*)
(* Слаувик тут должно быть что-то по типу что нельзя лист сложить но почему то какой то прикол с парсером у нас*)
let%expect_test _ =
  test_infer {| 
    let b = 1::2 in let a  = b + 5|};
  [%expect {| Parsing error: : end_of_input |}]
;;

let%expect_test _ =
  test_infer {| 
    let a  = "hello" + 5;;|};
  [%expect {| Infer error: Unification failed on string and int |}]
;;

let%expect_test _ =
  test_infer {|
    let a = true + 5;;|};
  [%expect {| Infer error: Unification failed on bool and int |}]
;;

let%expect_test _ =
  test_infer
    {|
  let res cond = match cond with | 1 -> "firstMatch" | 2 -> "SecondMatch" | "ERRORCASE" -> 3;;
  |};
  [%expect {| Infer error: Unification failed on int and string |}]
;;

let%expect_test _ =
  test_infer {|let a b = if b then 1 else "two";;|};
  [%expect {| Infer error: Unification failed on int and string |}]
;;

let%expect_test _ =
  test_infer {|
    let c (a : int) (b : string list) = a + b;;|};
  [%expect {| Infer error: Unification failed on string list and int |}]
;;

let%expect_test _ =
  test_infer {|
    let c (a : int) (b : int list) = a + b;;|};
  [%expect {| Infer error: Unification failed on int list and int |}]
;;