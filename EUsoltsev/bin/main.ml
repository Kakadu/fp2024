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
       Base.Map.iteri filtered_env ~f:(fun ~key:_ ~data:(S (_, ty)) ->
         Format.printf "%a\n" pp_ty ty)
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

let main () =
  let input = ref "" in
  let infer_flag = ref false in
  let interpret_flag = ref false in
  let file_flag = ref "" in
  let args =
    [ "-infer", Arg.Set infer_flag, "Run type inference"
    ; "-interpret", Arg.Set interpret_flag, "Run interpretation"
    ; "-file", Arg.Set_string file_flag, "Specify the file to process"
    ]
  in
  let anon_fun s = input := s in
  Arg.parse
    args
    anon_fun
    "Usage: program [-infer | -interpret] [-file <filename>] <input>";
  let input_content = if !file_flag <> "" then read_file !file_flag else !input in
  if !infer_flag
  then run_inference input_content
  else if !interpret_flag
  then run_interpreter input_content
  else printf "Please specify either -infer or -interpret flag.\n"
;;

let () = main ()
