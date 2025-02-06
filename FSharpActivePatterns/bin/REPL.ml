(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.AstPrinter
open FSharpActivePatterns.Parser
open FSharpActivePatterns.Inferencer
open FSharpActivePatterns.TypedTree
open FSharpActivePatterns.TypesPp
open FSharpActivePatterns.Ast
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
  let rec run_repl_helper run env state =
    let open Format in
    match run ic with
    | Result (Error _) ->
      fprintf err_formatter "Parsing error\n";
      run_repl_helper run env state
    | Empty ->
      fprintf std_formatter "\n";
      print_flush ();
      run_repl_helper run env state
    | End -> env
    | Result (Ok ast) ->
      (match dump_parsetree with
       | true ->
         print_construction std_formatter ast;
         run_repl_helper run env state
       | false ->
         let result = infer ast env state in
         (match result with
          | new_state, Error err ->
            fprintf err_formatter "Type checking failed: %a\n" pp_error err;
            print_flush ();
            run_repl_helper run env new_state
          | new_state, Ok (env, names_and_types) ->
            (match ic with
             | None ->
               List.iter
                 (fun (n, t) -> fprintf std_formatter "%s : %a\n" n pp_typ t)
                 names_and_types;
               print_flush ();
               run_repl_helper run env new_state
             | Some _ -> run_repl_helper run env new_state)))
  in
  let env =
    TypeEnvironment.extend
      TypeEnvironment.empty
      "print_int"
      (Scheme (VarSet.empty, Arrow (int_typ, unit_typ)))
  in
  let env = run_repl_helper run_single env 0 in
  let env = TypeEnvironment.remove env "print_int" in
  match ic with
  | Some _ -> TypeEnvironment.pp_without_freevars Format.std_formatter env
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
