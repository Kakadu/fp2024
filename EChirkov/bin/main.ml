(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Stdio
open EChirkov.Ast
open EChirkov.Parser
open EChirkov.Interpreter
open EChirkov.Inferencer

let usage () =
  printf "Usage: miniML [--interpret | --dump-ast | --infer] <file.ml>\n";
  exit 1
;;

let read_file filename =
  try In_channel.read_all filename with
  | Sys_error err ->
    printf "Error: %s\n" err;
    exit 1
;;

let () =
  if Array.length Sys.argv <> 3 then usage ();
  let mode = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let source_code = read_file filename in
  match parse source_code with
  | Error msg -> printf "Parsing error: %s\n" msg
  | Ok ast ->
    (match mode with
     | "--dump-ast" -> printf "AST:\n%s\n" (show_program ast)
     | "--interpret" ->
       (match interpret ast with
        | Ok _ -> ()
        | Error err ->
          printf "Interpretation error: %s\n" (EChirkov.Interpreter.pp_error err))
     | "--infer" ->
       (match inference ast with
        | Ok env -> EChirkov.Inferencer.print_env env
        | Error msg ->
          Format.printf "Type inference error: %a\n" EChirkov.Inferencer.pp_error msg)
     | _ -> usage ())
;;
