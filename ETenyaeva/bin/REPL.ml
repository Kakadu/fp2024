(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type options =
  { mutable dump_parsetree : bool
  ; mutable dump_inference : bool
  }

let run_single dump_parsetree dump_inference =
  let text = In_channel.(input_all stdin) |> String.trim in
  let ast = ETenyaeva_lib.Parser.parse text in
  match ast with
  | Error _ -> Format.printf "Syntax error"
  | Result.Ok ast ->
    if dump_parsetree then Format.printf "%a\n" ETenyaeva_lib.Ast.pp_structure ast;
    if dump_inference
    then (
      let infer =
        ETenyaeva_lib.Inferencer.run_inferencer
          ETenyaeva_lib.Inferencer.env_with_print_funs
          ast
      in
      match infer with
      | Error e ->
        Format.printf "Inferencer error: %a\n" ETenyaeva_lib.Inferencer.pp_error e
      | Result.Ok (_, infer_out_list) ->
        List.iter
          (function
            | Some id, ty ->
              Format.printf "val %s : %a\n" id ETenyaeva_lib.Inferencer.pp_type ty
            | None, ty -> Format.printf "- : %a\n" ETenyaeva_lib.Inferencer.pp_type ty)
          infer_out_list);
    if not (dump_inference || dump_parsetree)
    then (
      let infer =
        ETenyaeva_lib.Inferencer.run_inferencer
          ETenyaeva_lib.Inferencer.env_with_print_funs
          ast
      in
      match infer with
      | Error e ->
        Format.printf "Inferencer error: %a\n" ETenyaeva_lib.Inferencer.pp_error e
      | Result.Ok (_, _) ->
        let inter =
          ETenyaeva_lib.Interpreter.run_interpreter
            ETenyaeva_lib.Interpreter.env_with_print_funs
            ast
        in
        (match inter with
         | Error e ->
           Format.printf "Interpreter error: %a\n" ETenyaeva_lib.Interpreter.pp_error e
         | Result.Ok (_, inter_out_list) ->
           List.iter
             (function
               | Some id, val' ->
                 Format.printf "val %s = %a\n" id ETenyaeva_lib.Interpreter.pp_value val'
               | None, val' ->
                 Format.printf "- = %a\n" ETenyaeva_lib.Interpreter.pp_value val')
             inter_out_list))
;;

let () =
  let options = { dump_parsetree = false; dump_inference = false } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "--dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ; ( "--dinference"
        , Unit (fun () -> options.dump_inference <- true)
        , "Eval and display type inference info" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Anonymous arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for MiniML Calculus"
  in
  run_single options.dump_parsetree options.dump_inference
;;
