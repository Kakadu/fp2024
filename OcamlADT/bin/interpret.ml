(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast
open Ocamladt_lib.Parser
open Ocamladt_lib.Interpreter
open Ocamladt_lib.Interpreter.PPrinter
open Ocamladt_lib.Infer
open Ocamladt_lib.InferTypes
open Format

(* ------------------------------- *)
(*       Command-line Options      *)
(* ------------------------------- *)

type options =
  { mutable show_ast : bool
  ; mutable file_string : string option
  }

let usage_msg =
  "\n\
   Ocaml+ADT interpreter\n\n\
   Usage (file mode): dune exec ./bin/interpret.exe <options> <filepath>\n\
   Usage (repl mode): dune exec ./bin/interpret.exe <options>\n\n\
   Options:\n\
   --ast        Dump abstract syntax tree of a program\n\
   REPL commands:\n\
   help         Display usage message\n\
   quit         Quit the REPL mode\n"
;;

(* ------------------------------- *)
(*       REPL and File Modes       *)
(* ------------------------------- *)

(* A helper that parses a fixed word (like "help" or "quit") with surrounding whitespace. *)

let rec read_repl_input inp_chan =
  match In_channel.input_line inp_chan with
  | None -> None
  | Some input ->
    (match input with
     | "help" ->
       print_endline usage_msg;
       flush stdout;
       read_repl_input inp_chan
     | "quit" -> None
     | _ ->
       (match parse input with
        | Error _ ->
          print_endline "Syntax error";
          read_repl_input inp_chan
        | Ok ast -> if ast = [] then read_repl_input inp_chan else Some ast))
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
  let tcr = run_infer_program ast env_with_things in
  match tcr with
  | Error err -> Format.printf "Type error: %a\n" pp_inf_err err
  | Ok (env, _) ->
    (match run_interpreter ast with
     | Error e -> pp_error Format.std_formatter e
     | Ok olist ->
       List.iter
         (fun (tag, v) ->
           match tag with
           | Some id ->
             (match Base.Map.find env id with
              | Some (Forall (args, typ)) ->
                let m, _, _ = minimize (binder_to_list args) in
                let type_str = Format.asprintf "%a" (pprint_type ~poly_names_map:m) typ in
                Format.printf "val %s : %s = %a\n" id type_str PPrinter.pp_value v
              | None -> Format.printf "val %s = %a\n" id PPrinter.pp_value v)
           | None -> if v <> VString "" then Format.printf "_ = %a\n" PPrinter.pp_value v)
         olist);
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
  let options = { show_ast = false; file_string = None } in
  let arg_list = [ "--ast", Arg.Unit (fun () -> options.show_ast <- true), "Dump AST" ] in
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
