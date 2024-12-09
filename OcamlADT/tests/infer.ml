open Ocamladt_lib.Parser
open Format
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes

let parse_and_infer_result str =
  match parse str with
  | Ok parse_result ->
    (match run_infer_program parse_result with
     | Ok (_, env) -> printf "%a" TypeEnv.pp_env env
     | Error err -> printf "%a" pp_inf_err err)
  | Error err -> printf "Parsing error: %s\n" err
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| 55;; |} in
  [%expect {| "0": int |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let x = 5;; |} in
  [%expect {| "0": int |}]
;;
