(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast
open Ocamladt_lib.Parser
open Ocamladt_lib.Interpreter
open Ocamladt_lib.Interpreter.PPrinter
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes
open Angstrom

(* ------------------------------- *)
(*       Command-line Options      *)
(* ------------------------------- *)

type options =
  { mutable show_ast : bool
  ; mutable run_typecheck : bool
  ; mutable file_string : string option
  }

let usage_msg =
  "\n\
   Ocaml+ADT interpreter\n\n\
   Usage (file mode): dune exec ./bin/interpret.exe <options> <filepath>\n\
   Usage (repl mode): dune exec ./bin/interpret.exe <options>\n\n\
   Options:\n\
   --ast        Dump abstract syntax tree of a program\n\
   --typecheck  Typecheck the program and print result\n\n\
   REPL commands:\n\
   help         Display usage message\n\
   quit         Quit the REPL mode\n"
;;

(* ------------------------------- *)
(*       REPL and File Modes       *)
(* ------------------------------- *)

(* A helper that parses a fixed word (like "help" or "quit") with surrounding whitespace. *)
let parse_word word =
  let ws = skip_while Base.Char.is_whitespace in
  ws *> string word *> ws
;;

let parse_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  parse_str content
;;

let rec read_repl_input inp_chan =
  match In_channel.input_line inp_chan with
  | None -> None
  | Some input ->
    if input = "help"
    then (
      print_endline usage_msg;
      flush stdout;
      read_repl_input inp_chan)
    else if input = "quit"
    then None
    else (
      match parse input with
      | Error _ ->
        print_endline "Syntax error";
        read_repl_input inp_chan
      | Ok ast ->
        if ast = []
        then read_repl_input inp_chan (* empty input is handled by interpreter/parser *)
        else Some ast)
;;

(* Read an entire input and process it *)
let process_input options ast =
  print_endline "Running... ";
  flush stdout;
  if options.show_ast
  then (
    print_endline "\nAST dump:";
    print_endline (show_program ast);
    print_newline ());
  let typecheck_result =
    match run_infer_program ast env_with_print_funs with
    | Ok _ -> "passed"
    | Error err -> "error - " ^ Format.asprintf "%a" pp_inf_err err
  in
  if options.run_typecheck then print_endline ("Typecheck: " ^ typecheck_result);
  (match typecheck_result with
   | "passed" ->
     (match run_interpreter ast with
      | Ok olist ->
        List.iter
          (fun (tag, v) ->
            match tag with
            | Some id -> Format.printf "val %s = %a\n" id PPrinter.pp_value v
            | None -> if v <> VString "" then Format.printf "_ = %a\n" PPrinter.pp_value v)
          olist
      | Error e -> pp_error Format.std_formatter e)
   | _ -> if options.run_typecheck then print_endline ("Typecheck: " ^ typecheck_result));
  flush stdout;
  Format.pp_print_flush Format.std_formatter ()
;;

let run_repl options =
  let inp_chan = stdin in
  let rec helper () =
    match read_repl_input inp_chan with
    | None -> () (* Exit the loop if no input is provided (i.e., "quit" command) *)
    | Some ast ->
      process_input options ast;
      helper ()
  in
  helper ()
;;

let run_file options string =
  match parse string with
  | Error _ -> print_endline "Syntax Error"
  | Ok ast -> process_input options ast
;;

(* ------------------------------- *)
(*           Main Entry            *)
(* ------------------------------- *)

let () =
  let options = { show_ast = false; run_typecheck = false; file_string = None } in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> options.show_ast <- true), "Dump AST"
    ; "--typecheck", Arg.Unit (fun () -> options.run_typecheck <- true), "Run typecheck"
    ]
  in
  let read_file path =
    if Sys.file_exists path
    then (
      let ch = open_in_bin path in
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      options.file_string <- Some s)
    else (
      Printf.eprintf "File %s not found\n" path;
      exit 255)
  in
  Arg.parse arg_list read_file usage_msg;
  match options.file_string with
  | Some s -> run_file options s
  | None -> run_repl options
;;
