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

let%expect_test _ =
  test_infer {| 
      let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;
     |};
  [%expect {| val fact : int -> int |}]
;;
