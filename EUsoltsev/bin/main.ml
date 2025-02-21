(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Inferencer
open Interpreter
open Parser
open Printf
open Ast

let run_inference input =
  match parse input with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       let filtered_env =
         Base.Map.filter_keys env ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key ~data:(S (_, ty)) ->
         Format.printf "val %s: %a\n" key pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

let run_interpreter s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match Inter.eval_structure parsed with
     | Ok _ -> ()
     | Error e -> printf "Interpreter error: %a\n" pp_value_error e)
  | Error e -> printf "Parsing error: %s\n" e
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
  then run_inference input_content
  else if config.interpret_flag
  then run_interpreter input_content
  else printf "Please specify either -infer or -interpret flag.\n"
;;

let () = main ()
