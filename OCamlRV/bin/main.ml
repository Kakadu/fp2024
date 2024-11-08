(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Parser
open OCamlRV_lib.Pprintast
open Stdio

type opts =
  { mutable dump_parsetree : bool
  ; mutable read_from_file : bool
  ; mutable filename : string
  }

let run_single options =
  let text =
    if options.read_from_file
    then (
      try Stdlib.String.trim (Stdio.In_channel.read_all options.filename) with
      | Sys_error e ->
        Stdlib.Format.printf "%s\n" e;
        Stdlib.exit 1)
    else Stdlib.String.trim (In_channel.input_all stdin)
  in
  if options.dump_parsetree
  then Stdlib.Format.printf "%s\n" (parse_to_string text)
  else ()
;;

let () =
  if Array.length Sys.argv = 1
  then ()
  else (
    let opts = { dump_parsetree = false; read_from_file = false; filename = "" } in
    let () =
      let open Stdlib.Arg in
      parse
        [ ( "-dparsetree"
          , Unit (fun () -> opts.dump_parsetree <- true)
          , "Dump parse tree, don't eval enything" )
        ]
        (fun filename ->
          if opts.read_from_file
          then (
            Stdlib.Format.printf "It can handle only one input file.\n";
            Stdlib.exit 1);
          opts.read_from_file <- true;
          opts.filename <- filename)
        "Read-Eval-Print-Loop for OCamlRV"
    in
    run_single opts)
;;

Format.printf
  "%a\n"
  pp_structure_item_list
  [ SEval (ExprApply (ExprVariable "f", ExprVariable "x"))
  ; SEval (ExprCons (ExprVariable "f", ExprVariable "y"))
  ; SValue (NonRec, [ PVar "x", ExprLiteral (IntLiteral 5) ])
  ; SValue
      ( Rec
      , [ ( PVar "fact"
          , ExprFun
              ( PVar "n"
              , ExprIf
                  ( ExprBinOperation (Lte, ExprVariable "n", ExprLiteral (IntLiteral 1))
                  , ExprLiteral (IntLiteral 1)
                  , Some
                      (ExprBinOperation
                         ( Mul
                         , ExprVariable "n"
                         , ExprApply
                             ( ExprVariable "fact"
                             , ExprBinOperation
                                 (Sub, ExprVariable "n", ExprLiteral (IntLiteral 1)) ) ))
                  ) ) )
        ] )
  ; SValue
      ( NonRec
      , [ ( PVar "a"
          , ExprLet
              ( NonRec
              , [ PVar "b", ExprLiteral (IntLiteral 1) ]
              , ExprLet
                  ( NonRec
                  , [ PVar "c", ExprLiteral (IntLiteral 1) ]
                  , ExprBinOperation (Add, ExprVariable "b", ExprVariable "c") ) ) )
        ] )
  ]
