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
    | Some file_name -> In_channel.read_all file_name |> Stdlib.String.trim
    | None -> In_channel.input_all stdin |> Stdlib.String.trim
  in
  let ast = Parser.parse text in
  match ast with
  | None -> Format.printf "Error!"
  | Some ast -> if dump_parsetree then print_endline (Ast.show_structure ast)
;;

let () =
  let opts = { dump_parsetree = false; input_file = None } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-fromfile"
        , String (fun filename -> opts.input_file <- Some filename)
        , "Read code from the file" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Positional arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for custom language"
  in
  run_single opts.dump_parsetree opts.input_file
;;
