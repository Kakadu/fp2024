(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Riscv_asm_interpreter_lib.Ast
open Riscv_asm_interpreter_lib.Parser
open Angstrom
open Printf
open Stdio

type opts =
  { mutable dump_parse_tree : bool
  ; mutable file_path : string option
  }

let () =
  let opts = { dump_parse_tree = false; file_path = None } in
  let _ =
    Arg.parse
      [ "-dparsetree", Arg.Unit (fun () -> opts.dump_parse_tree <- true), "Dump AST\n"
      ; ( "-filepath"
        , Arg.String (fun file_path -> opts.file_path <- Some file_path)
        , "Input code in file\n" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Wrong arguments\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for RISC-V 64 ASM\n"
  in
  let input =
    match opts.file_path with
    | None -> In_channel.(input_all stdin) |> String.trim
    | Some path -> In_channel.read_all path |> String.trim
  in
  match parse_string ~consume:All parse_ast input with
  | Ok ast -> if opts.dump_parse_tree then print_endline (show_ast ast)
  | Error msg -> failwith (sprintf "Failed to parse file%s" msg)
;;
