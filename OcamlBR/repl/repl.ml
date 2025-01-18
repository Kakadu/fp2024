(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlBR.Ast
open OCamlBR.Parser
open OCamlBR.Interpreter_tests
open OCamlBR.Inferencer_tests
open Base
open Stdio

type stop_after =
  | SA_parsing
  | SA_never

type opts =
  { mutable dump_parsetree : bool
  ; mutable stop_after : stop_after
  ; mutable input_file : string option
  ; mutable interpret : bool
  ; mutable inference : bool
  }

let eval ast =
  ignore (show_structure ast);
  (* eval will be here soon *)
  ()
;;

let run_single dump_parsetree stop_after interpret inference eval input_source =
  let text =
    match input_source with
    | Some file_name -> In_channel.read_all file_name |> Stdlib.String.trim
    | None -> In_channel.input_all stdin |> Stdlib.String.trim
  in
  match parse_expr text with
  | Error e -> Stdlib.Format.printf "Parsing error: %s\n%!" e
  | Ok ast ->
    if dump_parsetree then print_endline (show_structure ast);
    (match stop_after with
     | SA_parsing -> ()
     | SA_never ->
       if inference then infer_program_test text;
       if interpret then test_interpret text;
       eval ast)
;;

let () =
  let opts =
    { dump_parsetree = false
    ; stop_after = SA_never
    ; input_file = None
    ; interpret = false
    ; inference = false
    }
  in
  let () =
    Stdlib.Arg.parse
      [ ( "-dparsetree"
        , Stdlib.Arg.Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-stop-after"
        , Stdlib.Arg.String
            (function
              | "parsing" -> opts.stop_after <- SA_parsing
              | _ -> failwith "Bad argument for -stop-after")
        , "Stop after parsing" )
      ; ( "-fromfile"
        , Stdlib.Arg.String (fun filename -> opts.input_file <- Some filename)
        , "Read code from the specified file" )
      ; ( "-interpret"
        , Stdlib.Arg.Unit (fun () -> opts.interpret <- true)
        , "Interpret the parsed code" )
      ; ( "-inference"
        , Stdlib.Arg.Unit (fun () -> opts.inference <- true)
        , "Perform type inference on the parsed code" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Positional arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for custom language"
  in
  run_single
    opts.dump_parsetree
    opts.stop_after
    opts.interpret
    opts.inference
    (fun ast -> eval ast)
    opts.input_file
;;
