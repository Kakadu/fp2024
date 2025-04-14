(** Copyright 2025, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EMigunova_lib

type options =
  { mutable dump_parsetree : bool
  ; mutable dump_inference : bool
  }

let run_single dump_parsetree dump_inference =
  let text = In_channel.(input_all stdin) |> String.trim in
  let ast = Parse.parse text in
  match ast with
  | Error _ -> Format.printf "Syntax error"
  | Result.Ok ast ->
    if dump_parsetree then Format.printf "%a\n" Ast.pp_structure ast;
    if dump_inference
    then (
      let infer = Inference.run_inferencer ast in
      match infer with
      | Error e ->
        Printf.printf "Type inference error: ";
        Inference.print_error e
      | Result.Ok infer_result_list ->
        let inter = Interpreter.run_interpreter ast in
        (match inter with
         | Error e -> Interpreter.print_error e
         | Result.Ok inter_result_list ->
           Base.List.fold2_exn
             infer_result_list
             inter_result_list
             ~init:()
             ~f:(fun () (name, ty) (_, value) ->
               Printf.printf "val %s : " name;
               Inference.print_type ty;
               Printf.printf " = ";
               Interpreter.print_value value;
               Printf.printf "\n")));
    if not (dump_inference || dump_parsetree)
    then (
      match Inference.run_inferencer ast with
      | Error _ -> ()
      | _ ->
        let _ = Interpreter.run_interpreter ast in
        ())
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
