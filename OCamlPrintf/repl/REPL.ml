(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib
open Stdio

type opts =
  { mutable dump_parsetree : bool
  ; mutable input_file : string option
  }

let run_single dump_parsetree input_source =
  let text =
    match input_source with
    | Some file_name -> In_channel.read_all file_name |> String.trim
    | None -> In_channel.input_all stdin |> String.trim
  in
  let ast = Parser.parse text in
  match ast with
  | Error error -> print_endline error
  | Ok ast -> if dump_parsetree then print_endline (Ast.show_structure ast)
;;

let () =
  let options = { dump_parsetree = false; input_file = None } in
  let () =
    let open Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-fromfile"
        , String (fun filename -> options.input_file <- Some filename)
        , "Read code from the file" )
      ]
      (fun _ ->
        Format.eprintf "Positional arguments are not supported\n";
        exit 1)
      "Read-Eval-Print-Loop for custom language"
  in
  run_single options.dump_parsetree options.input_file
;;
