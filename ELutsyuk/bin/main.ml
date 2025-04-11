(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

(* open *)
open Inferencer
open Interpreter
open Forest
open Parser
open Printf

let run_inferencer str =
  match parse_program str with
  | Ok parsed ->
    (match inference_program parsed with
     | Ok env ->
       let filtered_env =
         Base.Map.filter_keys env ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline" ]))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key ~data:(Scheme (_, ty)) ->
         Format.printf "val %s: %a\n" key TypesTree.pp_typ ty)
     | Error err -> Format.printf "Infer error. %a\n" TypesTree.pp_error err)
  | Error err -> Format.printf "Parsing error. %s\n" err
;;

let run_interpreter str =
  let open Stdlib.Format in
  match parse_program str with
  | Ok parsed ->
    (match interpret_program parsed with
     | Ok _ -> ()
     | Error err -> printf "Interpreter error: %a\n" ValuesTree.pp_error err)
  | Error err -> printf "Parsing error: %s\n" err
;;

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content
;;

type config =
  { infer_flag : bool
  ; interpret_flag : bool
  ; file : string option
  ; input : string option
  }

let parse_arguments () =
  let rec parse_args args config =
    match args with
    | [] -> config
    | "-infer" :: rest -> parse_args rest { config with infer_flag = true }
    | "-interpret" :: rest -> parse_args rest { config with interpret_flag = true }
    | "-file" :: filename :: rest -> parse_args rest { config with file = Some filename }
    | arg :: rest -> parse_args rest { config with input = Some arg }
  in
  parse_args
    (Array.to_list Sys.argv |> List.tl)
    { infer_flag = false; interpret_flag = false; file = None; input = None }
;;

let main () =
  let config = parse_arguments () in
  let input_content =
    match config.file with
    | Some filename -> read_file filename
    | None ->
      (match config.input with
       | Some s -> s
       | None -> "")
  in
  if config.infer_flag
  then run_inferencer input_content
  else if config.interpret_flag
  then run_interpreter input_content
  else Format.printf "Please specify either -infer or -interpret flag.\n"
;;

let () = main ()
