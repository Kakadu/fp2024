open Lambda_lib

include struct
  open Ast
  open Utils

  type 'a status =
    | Done of 'a
    | WIP of 'a

  let fin x = Done x
  let wip x = WIP x

  let ao_small_step_strat =
    let rec helper = function
      | Var _ as l -> fin l
      | Abs (x, b) ->
        (match helper b with
         | WIP b2 -> wip (abs x b2)
         | Done b2 -> fin (abs x b2))
      | App (f, arg) ->
        (match helper f with
         | WIP f2 -> wip (app f2 arg)
         | Done (Abs (x, body)) ->
           (match helper arg with
            | Done arg -> wip (Lambda.subst x ~by:arg body)
            | WIP arg -> wip (app f arg))
         | Done f2 -> fin (App (f2, arg)))
    in
    let rec loop t =
      match helper t with
      | Done x -> x
      | WIP x ->
        Format.printf " -- %a\n%!" Pprintast.pp_hum x;
        loop x
    in
    let on_app _ f arg = loop (app f arg) in
    let on_abs _ f x = loop (abs f x) in
    let on_var _ x = loop (var x) in
    { Lambda.on_var; on_abs; on_app }
  ;;
end

type strategy =
  | CBN
  | CBV
  | NO
  | AO

type strategy_kind =
  | Small_step
  | Big_step

type stop_after =
  | SA_parsing
  | SA_never

type opts =
  { mutable dump_parsetree : bool
  ; mutable mode : strategy_kind * strategy
  ; mutable stop_after : stop_after
  }

let big_step_evaluator = function
  | AO -> Lambda.ao_strat
  | NO -> Lambda.nor_strat
  | CBN -> Lambda.cbn_strat
  | CBV -> Lambda.cbv_strat
;;

let run_single dump_parsetree stop_after eval =
  let text = In_channel.(input_all stdin) |> String.trim in
  let ast = Parser.parse text in
  match ast with
  | Error e -> Format.printf "Error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    if dump_parsetree then Format.printf "Parsed result: @[%a@]\n%!" Printast.pp_named ast;
    (match stop_after with
     | SA_parsing -> ()
     | SA_never ->
       let rez = eval ast in
       Format.printf "Evaluated result: %a\n%!" Pprintast.pp_hum rez)
;;

let () =
  let opts = { dump_parsetree = false; mode = Big_step, NO; stop_after = SA_never } in
  let pick_strategy stra () =
    let kind, _ = opts.mode in
    opts.mode <- kind, stra
  in
  let pick_step step () =
    let _, stra = opts.mode in
    opts.mode <- step, stra
  in
  let () =
    let open Stdlib.Arg in
    parse
      [ "-cbv", Unit (pick_strategy CBV), "Call-by-value strategy"
      ; "-cbn", Unit (pick_strategy CBN), "Call-by-name strategy"
      ; "-no", Unit (pick_strategy NO), "Normal Order strategy"
      ; "-ao", Unit (pick_strategy AO), "Applicative Order strategy"
      ; ( "-small"
        , Unit (pick_step Small_step)
        , "Small-step strategy kind (default is big-step)" )
      ; ( "-big"
        , Unit (pick_step Big_step)
        , "Small-step strategy kind (default is big-step)" )
      ; ( "-dparsetree"
        , Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't eval enything" )
      ; ( "-stop-after"
        , String
            (function
              | "parsing" -> opts.stop_after <- SA_parsing
              | _ -> failwith "Bad argument of -stop-after")
        , "" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Positioned arguments are not supported\n";
        Stdlib.exit 1)
      "Read-Eval-Print-Loop for Utyped Lambda Calculus"
  in
  run_single opts.dump_parsetree opts.stop_after (fun ast ->
    let stra =
      match opts.mode with
      | Big_step, stra -> big_step_evaluator stra
      | Small_step, AO -> ao_small_step_strat
      | _ -> raise (Failure "Implement it yourself")
    in
    Lambda.apply_strat stra ast)
;;
