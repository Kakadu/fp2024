open Miniml.Inferencer
open Miniml.Typedtree
open Miniml.Parser_utility
open Miniml.Parser
open Miniml.Ast
open Format

let code_infer s =
  let open Stdlib.Format in
  match parse program_parser s with
  | ParseSuccess (p, parser_state) ->
    (match run_infer p with
     | Ok env -> printf "%a\n" TypeEnv.pp_env env
     | Error e -> printf "Inferencer error: %a\n" pp_error e)
  | ParseError (e, parser_state) -> printf "Parser error: %s\n" e
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
