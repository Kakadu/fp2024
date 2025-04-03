(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Inference
open Inference.Scheme
open Inference.TypeEnv
open Inference.Infer
open Parse.Structure
open Parse.Patterns

let print_env env =
  let open Format in
  let () =
    Base.Map.iteri
      ~f:(fun ~key:name ~data:(Scheme (_, ty)) ->
        if (not (Checks.is_builtin_op name)) && not (Checks.is_builtin_fun name)
        then printf "%s : %s\n" name (State.pp_core_type ty))
      env
  in
  ()
;;

let run_pat s =
  let open Format in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All ppat s with
  | Error e -> printf "Parse error: %s\n" e
  | Ok parsed ->
    let open Inference.State in
    (match run (infer_pat builtin_env parsed) with
     | Error e -> printf "%s" (pp_error e)
     | Ok (res, _) -> print_env res)
;;

let run s =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pprog s with
  | Ok ast ->
    (match infer empty ast with
     | Ok (type_env, _) -> print_env type_env
     | Error e -> Format.printf "Inferencer error: %s\n" (State.pp_error e))
  | Error e -> Format.printf "Parse error: %s\n" e
;;

let _ = run, run_pat

(************************** Patterns **************************)

let%expect_test _ =
  run_pat {|_|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|a|};
  [%expect {| a : 'a0 |}]
;;

let%expect_test _ =
  run_pat {|1|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|1.0|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|true|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|"str"|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|'a'|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|()|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|1.0<bip>|};
  [%expect {| Not implemented |}]
;;

let%expect_test _ =
  run_pat {|(a, b)|};
  [%expect {|
  a : 'a0
  b : 'a1 |}]
;;

let%expect_test _ =
  run_pat {|( (a, b), (c, d) )|};
  [%expect {|
  a : 'a0
  b : 'a1
  c : 'a2
  d : 'a3 |}]
;;

let%expect_test _ =
  run_pat {|(a : int)|};
  [%expect {| a : int |}]
;;

let%expect_test _ =
  run_pat {|(1 : string)|};
  [%expect {| Unification failed for types string and int |}]
;;

let%expect_test _ =
  run_pat {|[]|};
  [%expect {| |}]
;;

let%expect_test _ =
  run_pat {|[a; b]|};
  [%expect {|
  a : 'a2
  b : 'a2 |}]
;;

let%expect_test _ =
  run_pat {|a | b|};
  [%expect {|
  a : 'a0
  b : 'a0 |}]
;;

let%expect_test _ =
  run_pat {|a | b | c|};
  [%expect {|
  a : 'a0
  b : 'a0
  c : 'a0 |}]
;;

let%expect_test _ =
  run_pat {|a :: b|};
  [%expect {|
  a : 'a0
  b : 'a0 list |}]
;;

let%expect_test _ =
  run_pat {|a :: []|};
  [%expect {|
    a : 'a0 |}]
;;

let%expect_test _ =
  run_pat {|a :: b :: c|};
  [%expect {|
  a : 'a0
  b : 'a0
  c : 'a0 list |}]
;;

let%expect_test _ =
  run_pat {|Some a|};
  [%expect {| a : 'a0 |}]
;;

let%expect_test _ =
  run_pat {|None|};
  [%expect {| |}]
;;

(************************** Programs **************************)

let%expect_test _ =
  run {| let hello = "world" |};
  [%expect {| hello : string |}]
;;
