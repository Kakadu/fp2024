open Lib

type opts = { mutable dump_parsetree : bool }

let run_single dump_parsetree =
  let text = In_channel.(input_all stdin) |> String.trim in
  let ast = Parser.parse text in
  match ast with
  | Error e -> Format.printf "Error: %s\n%!" e
  | Result.Ok ast ->
    if dump_parsetree
    then Format.printf "Parsed result: @[%a@]\n%!" Lib.Ast.pp_structure_item ast
    else Format.printf "Not implemented. Only -dparsertree support"
;;

let () =
  let opts = { dump_parsetree = false } in
  let () =
    let open Stdlib.Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Anonymous arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for my cool homka&damir's parser"
  in
  run_single opts.dump_parsetree
;;
