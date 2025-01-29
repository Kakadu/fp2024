open Lib.Infer
open Lib.Parser
open Lib.Infer_print
open Lib.Types

let str =
  "let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10) and homka = 122"
;;

let str = "let f x y z homka damir chtooo = x y (homka + damir) chtooo"
let fac self n = if n <= 1 then 1 else n * self (n - 1)
let str = "let fac self n = if true then 1 else n * self (n - 1)"

let homka =
  match parse str with
  | Result.Ok x -> x
  | Result.Error e -> failwith e
;;

let () =
  Base.Map.iteri defaultEnv ~f:(fun ~key ~data:(Scheme (s, t)) ->
    Format.printf
      "val %s : %a . %a\n"
      key
      pp_typ_my
      (if TVarSet.is_empty s then TBase BUnit else TVar (TVarSet.choose s))
      pp_typ_my
      t)
;;

let homka_infer = run_structure_inferencer homka
let () = Format.printf "Structure: %s\n\n" str
let () = Format.printf "Environment:\n"

let () =
  Base.Map.iteri homka_infer ~f:(fun ~key ~data ->
    Format.printf
      "val %s : %a\n"
      key
      pp_typ_my
      (match data with
       | Lib.Types.Scheme (_, t) -> t))
;;

(* let homka =
   match parse_expr "fun foo -> foo + 2" with
   | Result.Ok x -> x
   | Result.Error e -> failwith e
   ;;

   let homka_infer =
   match run_expr_inferencer homka with
   | Result.Ok x -> x
   | Result.Error e -> failwith (show_error e)
   ;;

   let () = print_endline ("\nANSWER: " ^ show_typ homka_infer) *)
