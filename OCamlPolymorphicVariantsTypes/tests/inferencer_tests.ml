(** Copyright 2024-2027, Ilia Suponev, Chirkov Dmitri *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Inferencer
open Miniml.Parser_utility
open Miniml.Parser

let code_infer s =
  let open Stdlib.Format in
  match parse program_parser s with
  | ParseSuccess (p, _) ->
    (match run_infer p with
     | Ok env -> printf "%a\n" TypeEnv.pp_env env
     | Error e -> printf "Inferencer error: %a\n" pp_error e)
  | ParseError (e, _) -> printf "Parser error: %s\n" e
  | ParseFail -> printf "Parser failed" (*TODO*)
;;

let%expect_test _ =
  code_infer {| let x = 5 ;;|};
  [%expect {| "x" : int |}]
;;

let%expect_test _ =
  code_infer {| let x = if (23 < true) then false else true;;|};
  [%expect {| Inferencer error: unification failed on bool and int |}]
;;

let%expect_test _ =
  code_infer {| let x = if (23 < 34) then false else true;;|};
  [%expect {| "x" : bool |}]
;;

let%expect_test _ =
  code_infer {| let x = [];;|};
  [%expect {| "x" : [ 0; ]. '0 list |}]
;;

let%expect_test _ =
  code_infer {| let x = [2; 34];;|};
  [%expect {| "x" : int list |}]
;;

let%expect_test _ =
  code_infer {| let x f = f;;|};
  [%expect {| "x" : [ 0; ]. ('0 -> '0) |}]
;;

let%expect_test _ =
  code_infer {| let a b = match b with | int -> 22 | bool -> false;; |};
  [%expect {| Inferencer error: unification failed on bool and int |}]
;;

let%expect_test _ =
  code_infer {| let f a b = a b;; |};
  [%expect {| "f" : [ 0; 2; ]. (('0 -> '2) -> ('0 -> '2)) |}]
;;

let%expect_test _ =
  code_infer {| let a = [(2, 5), (7, 8)];; |};
  [%expect {| "a" : ((int * int) * (int * int)) list |}]
;;

let%expect_test _ =
  code_infer {| let a b = Some(b);; |};
  [%expect {| "a" : [ 0; ]. ('0 -> '0 Some) |}]
;;

let%expect_test _ =
  code_infer {| let a = None;; |};
  [%expect {| "a" : [ 0; ]. '0 None |}]
;;

let%expect_test _ =
  code_infer {| let a = function | int -> true | bool -> false;; |};
  [%expect {| "a" : [ 0; ]. ('0 -> bool) |}]
;;

let%expect_test _ =
  code_infer {| let a b = match b with | int -> true | bool -> false;; |};
  [%expect {| "a" : [ 2; ]. ('2 -> bool) |}]
;;
