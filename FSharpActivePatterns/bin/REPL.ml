(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Parser
open Stdlib

let input_upto_sep sep ic =
  let sep_len = String.length sep in
  let take_line () = In_channel.input_line ic in
  let rec fill_buffer b =
    let line = take_line () in
    match line with
    | None -> ()
    | Some line ->
      let len = String.length line in
      (match String.ends_with ~suffix:sep (String.trim line) with
       | true ->
         Buffer.add_substring b line 0 (len - sep_len);
         Buffer.add_string b "\n"
       | false ->
         Buffer.add_string b line;
         Buffer.add_string b "\n";
         fill_buffer b)
  in
  let buffer = Buffer.create 1024 in
  let () = fill_buffer buffer in
  Buffer.contents buffer
;;

let run_single ic =
  let input = input_upto_sep ";;" ic in
  parse input
;;

let run_repl dump_parsetree input_file =
  let ic =
    match input_file with
    | None -> stdin
    | Some n -> open_in n
  in
  let rec run_repl_helper run =
    match run ic with
    | None -> Stdlib.Format.eprintf "Error occured\n"
    | Some ast ->
      if dump_parsetree
      then (
        print_construction ast;
        flush stdout);
      run_repl_helper run
  in
  run_repl_helper run_single
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
