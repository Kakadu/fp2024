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
