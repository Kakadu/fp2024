(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.AstPrinter
open FSharpActivePatterns.Parser
open FSharpActivePatterns.Inferencer
open FSharpActivePatterns.TypesPp
open FSharpActivePatterns.Ast
open FSharpActivePatterns.Interpreter
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
      (match String.ends_with ~suffix:sep line with
       | true ->
         Buffer.add_substring b line 0 (len - sep_len);
         Buffer.add_string b "\n";
         Input (Buffer.contents b)
       | false ->
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
      (match is_continue with
       | true ->
         Buffer.add_string b (line ^ "\n");
         fill_buffer b
       | false ->
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

let run_single ic =
  let input =
    match ic with
    | None -> input_upto_sep ";;" stdin
    | Some ic -> input_with_indents ic
  in
  match input with
  | EOF -> End
  | Input input -> if String.trim input = "" then Empty else Result (parse input)
;;

let run_repl dump_parsetree input_file =
  let ic =
    match input_file with
    | None -> None
    | Some n -> Some (open_in n)
  in
  let rec run_repl_helper run type_env value_env state =
    let open Format in
    match run ic with
    | Result (Error _) ->
      fprintf err_formatter "Parsing error\n";
      run_repl_helper run type_env value_env state
    | Empty ->
      fprintf std_formatter "\n";
      print_flush ();
      run_repl_helper run type_env value_env state
    | End -> type_env, value_env
    | Result (Ok ast) ->
      (match dump_parsetree with
       | true ->
         print_construction std_formatter ast;
         run_repl_helper run type_env value_env state
       | false ->
         let result = run_interpreter type_env value_env state ast in
         (match result with
          | new_state, Error err ->
            fprintf err_formatter "Error occured: %a\n" pp_global_error err;
            print_flush ();
            run_repl_helper run type_env value_env new_state
          | new_state, Ok (new_type_env, new_value_env, evaled_names) ->
            (match ic with
             | None ->
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
               run_repl_helper run new_type_env new_value_env new_state
             | Some _ -> run_repl_helper run new_type_env new_value_env new_state)))
  in
  let type_env = TypeEnv.default in
  let value_env = ValueEnv.default in
  let type_env, value_env = run_repl_helper run_single type_env value_env 0 in
  let type_env = TypeEnv.remove type_env "print_int" in
  let value_env = ValueEnv.remove value_env "print_int" in
  match ic with
  | Some _ ->
    TypeEnv.iteri type_env ~f:(fun ~name ~typ ->
      let open Format in
      let value = ValueEnv.find_exn value_env name in
      fprintf std_formatter "val %s : %a = %a\n" name pp_typ typ ValueEnv.pp_value value)
  | None -> ()
;;

type opts =
  { mutable dump_parsetree : bool
  ; mutable input_file : string option
  }

let () =
  let opts = { dump_parsetree = false; input_file = None } in
  let open Stdlib.Arg in
  let speclist =
    [ ( "-dparsetree"
      , Unit (fun _ -> opts.dump_parsetree <- true)
      , "Dump parse tree, don't evaluate anything" )
    ; ( "-fromfile"
      , String (fun filename -> opts.input_file <- Some filename)
      , "Input file name" )
    ]
  in
  let anon_func _ =
    Stdlib.Format.eprintf "Positioned arguments are not supported\n";
    Stdlib.exit 1
  in
  let usage_msg = "Read-Eval-Print-Loop for F# with Active Patterns" in
  let () = parse speclist anon_func usage_msg in
  run_repl opts.dump_parsetree opts.input_file
;;
