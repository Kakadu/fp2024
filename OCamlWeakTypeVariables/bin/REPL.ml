[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lib

type opts =
  { mutable dump_parsetree : bool
  ; mutable dump_inference : bool
  ; mutable dump_parseprogram : bool
  ; mutable dump_inferprogram : bool
  }

let run_single opts =
  let text = In_channel.(input_all stdin) |> String.trim in
  if opts.dump_inferprogram
  then (
    let ast = Parser.parse_program text in
    match ast with
    | Error e -> Format.printf "Error: %s\n" e
    | Result.Ok program ->
      (match Infer.run_program_inferencer program with
       | Ok (env, names) -> Format.printf "%a\n" (Infer.TypeEnv.pp_names names) env
       | Error e -> Format.printf "Error: %a\n" Infer_print.pp_error_my e));
  if opts.dump_parseprogram
  then (
    let ast = Parser.parse_program text in
    match ast with
    | Error e -> Format.printf "Error: %s\n" e
    | Result.Ok program -> Format.printf "Parsed program: %a\n" Ast.pp_program program);
  if opts.dump_parsetree
  then (
    let ast = Parser.parse text in
    match ast with
    | Error e -> Format.printf "Error: %s\n%!" e
    | Result.Ok ast ->
      Format.printf "Parsed result: @[%a@]\n%!" Lib.Ast.pp_structure_item ast);
  if opts.dump_inference
  then (
    let ast = Parser.parse text in
    match ast with
    | Error e -> Format.printf "Error: %s\n%!" e
    | Result.Ok ast ->
      (match ast with
       | Pstr_eval expr ->
         (match Infer.run_expr_inferencer expr with
          | Ok t ->
            (* Format.printf "> %s;;\n\n" text; *)
            Format.printf "- : %a\n" Infer_print.pp_typ_my t
          | Error e -> Format.printf "%a\n" Infer_print.pp_error_my e)
       | _ ->
         (match Infer.run_structure_inferencer ast with
          | Ok (env, names) -> Format.printf "%a\n" (Infer.TypeEnv.pp_names names) env
          | Error e -> Format.printf "Error: %a\n" Infer_print.pp_error_my e)))
;;

let () =
  let opts =
    { dump_parsetree = false
    ; dump_inference = false
    ; dump_parseprogram = false
    ; dump_inferprogram = false
    }
  in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't eval anything" )
      ; ( "-dinference"
        , Unit (fun () -> opts.dump_inference <- true)
        , "Infer structure, don't eval anything" )
      ; ( "-dparseprogram"
        , Unit (fun () -> opts.dump_parseprogram <- true)
        , "Dump parse program, don't eval anything" )
      ; ( "-dinferprogram"
        , Unit (fun () -> opts.dump_inferprogram <- true)
        , "Infer program, don't eval anything" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Anonymous arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for my cool homka&damir's parser"
  in
  run_single opts
;;
