(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest
open Parser
open Inferencer.InfAuxilary
open Inferencer.Inference
open Interpreter.Interpret
open Stdlib.Format

let run_infer input =
  match parse input with
  | Ok program ->
    (match inference program with
     | Ok (_, env) ->
       (* Getting the environment without builtin *)
       let remove_builtins env =
         let env = TypeEnv.remove env "print_int" in
         TypeEnv.remove env "print_endline"
       in
       Base.Map.iteri (remove_builtins env) ~f:(fun ~key ~data:(Scheme (_, ty)) ->
         printf "val %s: %a\n" key TypesTree.pp_typ ty)
     | Error err -> printf "Inferencing error: %a.\n" TypesTree.pp_error err)
  | Error err -> printf "Parsing error: %s.\n" err
;;

let run_eval input =
  match parse input with
  | Ok program ->
    (match Interpreter.IntAuxilary.Res.run (interpret program) with
     | Ok _ -> ()
     | Error err -> printf "Interpretation error: %a.\n" ValuesTree.pp_error err)
  | Error err -> printf "Parsing error: %s.\n" err
;;

let get_input filename =
  let channel = open_in filename in
  let input = really_input_string channel (in_channel_length channel) in
  close_in channel;
  input
;;

type config =
  { is_infer : bool
  ; is_eval : bool
  ; file : string option
  ; input : string option
  }

let parse_args () =
  let rec prs_args args flags =
    match args with
    | [] -> flags
    | "-infer" :: rest -> prs_args rest { flags with is_infer = true }
    | "-eval" :: rest -> prs_args rest { flags with is_eval = true }
    | "-file" :: filename :: rest -> prs_args rest { flags with file = Some filename }
    | arg :: rest -> prs_args rest { flags with input = Some arg }
  in
  let default_flags = { is_infer = false; is_eval = false; file = None; input = None } in
  let args =
    match Array.to_list Sys.argv with
    | _ :: list -> list
    | _ -> []
  in
  prs_args args default_flags
;;

let main () =
  let config = parse_args () in
  let input =
    match config.file with
    | Some filename -> get_input filename
    | None ->
      (match config.input with
       | Some s -> s
       | None -> "")
  in
  if config.is_infer
  then run_infer input
  else if config.is_eval
  then run_eval input
  else if config.is_infer && config.is_eval
  then (
    let _ = run_infer input in
    let _ = run_eval input in
    ())
  else
    printf
      "Error: Could not parse arguments: Please restart program and put -infer or -eval \
       flag.\n"
;;

let () = main ()
