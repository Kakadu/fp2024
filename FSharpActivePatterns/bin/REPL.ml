(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.AstPrinter
open FSharpActivePatterns.Parser
open FSharpActivePatterns.Inferencer
open FSharpActivePatterns.TypedTree
open FSharpActivePatterns.TypesPp
open Stdlib

type input =
  | Input of string
  | EOF

type 'a run_result =
  | Result of 'a
  | Fail
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

let run_single input =
  match input with
  | EOF -> End
  | Input input ->
    let trimmed_input = String.trim input in
    if trimmed_input = ""
    then Empty
    else (
      match parse trimmed_input with
      | Some ast -> Result ast
      | None -> Fail)
;;

let run_repl dump_parsetree input_file =
  (* TODO: refactor repl runners *)
  let run_repl_helper input env state =
    let open Format in
    match input with
    | Fail ->
      fprintf err_formatter "Error occured\n";
      None
    | Empty ->
      fprintf std_formatter "\n";
      print_flush ();
      Some (env, state)
    | End -> None
    | Result ast ->
      (match dump_parsetree with
       | true ->
         print_construction std_formatter ast;
         Some (env, state)
       | false ->
         let result = infer ast env state in
         (match result with
          | new_state, Error err ->
            fprintf err_formatter "Type checking failed: %a\n" pp_error err;
            print_flush ();
            Some (env, new_state)
          | new_state, Ok (env, types) ->
            List.iter
              (fun t ->
                fprintf std_formatter "- : ";
                pp_typ std_formatter t)
              types;
            print_flush ();
            Some (env, new_state)))
  in
  let env =
    TypeEnvironment.extend
      TypeEnvironment.empty
      "print_int"
      (Scheme (VarSet.empty, Arrow (int_typ, unit_typ)))
  in
  let rec run_file inputs env state =
    match inputs with
    | [] -> ()
    | hd :: tl ->
      (match run_repl_helper hd env state with
       | Some (env, state) -> run_file tl env state
       | None -> ())
  in
  let rec run_repl env state ic =
    let input = run_single (input_upto_sep ";;" ic) in
    match run_repl_helper input env state with
    | Some (env, state) -> run_repl env state ic
    | None -> ()
  in
  match input_file with
  | None -> run_repl env 0 stdin
  | Some n ->
    let content = In_channel.input_all (open_in n) in
    let re = Str.regexp "\n\n" in
    let splitted = Str.split re content in
    let splitted = List.map (fun s -> run_single (Input s)) splitted in
    run_file splitted env 0
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
