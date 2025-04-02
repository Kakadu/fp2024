(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Inference.Infer
open Inference.Scheme
open Inference.Subst
open Inference.Type
open Inference.TypeEnv
open Parse.Structure
open Parse.Patterns
open Ast

(* todo: pprint_type, pprint_error *)

let print_env (env) =
  let open Format in
  let () = Base.Map.iteri
  ~f:(fun ~key:name ~data:(Scheme (_, ty)) ->
    if (not (Checks.is_builtin_op name) && not (Checks.is_builtin_fun name))
    then (printf "%s : " name; pp_core_type std_formatter ty; printf "\n"))
  env in ()

let run_pat s =
  let open Format in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All ppat s with
  | Error e -> printf "Parse error: %s\n" e
  | Ok parsed ->
    let open Inference.State in
    match run (infer_pat Inference.Infer.initial_env parsed) with
    | Error e -> printf "Inference error\n"
    | Ok (res, _) -> print_env res
;;

(************************** Patterns **************************)

let%expect_test _ =
run_pat {|_|};
[%expect {| |}]

let%expect_test _ =
run_pat {|a|};
[%expect {| a : (Type_var "0") |}]

let%expect_test _ =
run_pat {|1|};
[%expect {| |}]

let%expect_test _ =
run_pat {|1.0|};
[%expect {| |}]

let%expect_test _ =
run_pat {|true|};
[%expect {| |}]

let%expect_test _ =
run_pat {|"killme"|};
[%expect {| |}]

let%expect_test _ =
run_pat {|'a'|};
[%expect {| |}]

let%expect_test _ =
run_pat {|()|};
[%expect {| |}]

let%expect_test _ =
run_pat {|1.0<bip>|};
[%expect {| Inference error |}]

let%expect_test _ =
run_pat {|(a, b)|};
[%expect {|
  a : (Type_var "0")
  b : (Type_var "1") |}]

let%expect_test _ =
run_pat {|( (a, b), (c, d) )|};
[%expect {|
  a : (Type_var "0")
  b : (Type_var "1")
  c : (Type_var "2")
  d : (Type_var "3") |}]

let%expect_test _ =
run_pat {|(a : int)|};
[%expect {| a : Type_int |}]

let%expect_test _ =
run_pat {|[]|};
[%expect {| |}]

let%expect_test _ =
run_pat {|[a; b]|};
[%expect {|
  a : (Type_var "2")
  b : (Type_var "2") |}]

let%expect_test _ =
run_pat {|a | b|};
[%expect {|
  a : (Type_var "0")
  b : (Type_var "0") |}]

let%expect_test _ =
run_pat {|a | b | c|};
[%expect {|
  a : (Type_var "0")
  b : (Type_var "0")
  c : (Type_var "0") |}]

let%expect_test _ =
run_pat {|a :: b|};
[%expect {|
  a : (Type_var "0")
  b : (Type_list (Type_var "0")) |}]

  let%expect_test _ =
  run_pat {|a :: []|};
  [%expect {|
    a : (Type_var "0") |}]
  

let%expect_test _ =
run_pat {|a :: b :: c|};
[%expect {|
  a : (Type_var "0")
  b : (Type_var "0")
  c : (Type_list (Type_var "0")) |}]

let%expect_test _ =
run_pat {|Some a|};
[%expect {| a : (Type_var "0") |}]

let%expect_test _ =
run_pat {|None|};
[%expect {| |}]

(************************** Expressions **************************)
