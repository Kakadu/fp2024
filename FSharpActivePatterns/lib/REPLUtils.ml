(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AstPrinter
open Parser
open Inferencer
open TypesPp
open Ast
open Interpreter
open Stdlib

type input =
  | Input of string
  | EOF

type run_result =
  | Result of (construction, string) result
  | Empty
  | End

let input_upto_sep sep ic =
  let sep_len = String.length sep in
  let take_line () = In_channel.input_line ic in
  let rec fill_buffer b =
    let line = take_line () in
    match line with
    | None -> EOF
    | Some line ->
      let line = String.trim line in
      let len = String.length line in
      if String.ends_with ~suffix:sep line
      then (
        Buffer.add_substring b line 0 (len - sep_len);
        Buffer.add_string b "\n";
        Input (Buffer.contents b))
      else (
        Buffer.add_string b line;
        Buffer.add_string b "\n";
        fill_buffer b)
  in
  let buffer = Buffer.create 1024 in
  fill_buffer buffer
;;

let input_with_indents ic =
  let take_line () = In_channel.input_line ic in
  let rec fill_buffer b =
    let start_pos = pos_in ic in
    let line = take_line () in
    match line with
    | None -> Input (Buffer.contents b)
    | Some line ->
      let is_empty = String.length line = 0 in
      let is_continue =
        List.exists (fun pref -> String.starts_with ~prefix:pref line) [ " "; "\t"; "\n" ]
        || is_empty
        || String.starts_with ~prefix:"and" (String.trim line)
      in
      if is_continue
      then (
        Buffer.add_string b (line ^ "\n");
        fill_buffer b)
      else (
        seek_in ic start_pos;
        Buffer.add_string b "\n";
        Input (Buffer.contents b))
  in
  let buffer = Buffer.create 1024 in
  let first_line = take_line () in
  match first_line with
  | None -> EOF
  | Some first_line ->
    Buffer.add_string buffer (first_line ^ "\n");
    fill_buffer buffer
;;

type in_channel =
  | File of Stdlib.in_channel
  | Stdin

let run_single ic =
  let input =
    match ic with
    | Stdin -> input_upto_sep ";;" Stdlib.stdin
    | File ic -> input_with_indents ic
  in
  match input with
  | EOF -> End
  | Input input -> if String.trim input = "" then Empty else Result (parse input)
;;

let run_repl dump_parsetree input_file =
  let ic =
    match input_file with
    | Some n -> File (open_in n)
    | None -> Stdin
  in
  let rec run_repl_helper run type_env value_env state values_acc =
    let open Format in
    match run ic with
    | Result (Error _) ->
      fprintf err_formatter "Parsing error\n";
      run_repl_helper run type_env value_env state values_acc
    | Empty ->
      fprintf std_formatter "\n";
      print_flush ();
      run_repl_helper run type_env value_env state values_acc
    | End -> type_env, value_env, values_acc
    | Result (Ok ast) ->
      if dump_parsetree
      then (
        print_construction std_formatter ast;
        run_repl_helper run type_env value_env state values_acc)
      else (
        let result = run_interpreter type_env value_env state ast in
        match result with
        | new_state, Error err ->
          fprintf err_formatter "Error occured: %a\n" pp_global_error err;
          print_flush ();
          run_repl_helper run type_env value_env new_state values_acc
        | new_state, Ok (new_type_env, new_value_env, evaled_names) ->
          (match ic with
           | Stdin ->
             Base.Map.iteri
               ~f:(fun ~key ~data ->
                 let t, v = data in
                 fprintf
                   std_formatter
                   "val %s : %a = %a\n"
                   key
                   pp_typ
                   t
                   ValueEnv.pp_value
                   v)
               evaled_names;
             print_flush ();
             run_repl_helper run new_type_env new_value_env new_state values_acc
           | File _ ->
             let overwrite map1 map2 =
               Base.Map.fold
                 ~init:map1
                 ~f:(fun ~key ~data map1 -> Base.Map.set map1 ~key ~data)
                 map2
             in
             let values_acc = overwrite values_acc evaled_names in
             run_repl_helper run new_type_env new_value_env new_state values_acc))
  in
  let type_env = TypeEnv.default in
  let value_env = ValueEnv.default in
  let _, _, evaled_values =
    run_repl_helper run_single type_env value_env 0 (Base.Map.empty (module Base.String))
  in
  Base.Map.iteri evaled_values ~f:(fun ~key ~data:(typ, value) ->
    Format.fprintf
      Format.std_formatter
      "val %s : %a = %a\n"
      key
      pp_typ
      typ
      ValueEnv.pp_value
      value)
;;
