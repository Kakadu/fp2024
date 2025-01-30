open Types

let min = true

let pp_base_type_my fmt = function
  | BInt -> Format.fprintf fmt "int"
  | BBool -> Format.fprintf fmt "bool"
  | BUnit -> Format.fprintf fmt "unit"
  | BString -> Format.fprintf fmt "string"
;;

let minimize_variable t =
  let map =
    let rec helper (min, map) = function
      | TVar v ->
        if Base.Map.mem map v
        then min, map
        else min + 1, Base.Map.add_exn map ~key:v ~data:min
      | TArrow (l, r) ->
        let min, map = helper (min, map) l in
        let min, map = helper (min, map) r in
        min, map
      | TTuple (f, s, xs) -> List.fold_left helper (min, map) (f :: s :: xs)
      | TList l -> helper (min, map) l
      | TBase _ -> min, map
    in
    helper (0, Base.Map.empty (module Base.Int)) t |> snd
  in
  let rec helper = function
    | TVar v -> TVar (Base.Map.find_exn map v)
    | TArrow (l, r) -> TArrow (helper l, helper r)
    | TTuple (f, s, xs) -> TTuple (helper f, helper s, List.map helper xs)
    | TList l -> TList (helper l)
    | TBase b -> TBase b
  in
  helper t
;;

let pp_typ_my fmt t =
  let t = if min then minimize_variable t else t in
  let rec helper fmt = function
    | TBase b -> pp_base_type_my fmt b
    | TVar v -> Format.fprintf fmt "'%c" (Char.chr (Char.code 'a' + v))
    | TArrow ((TArrow (_, _) as l), r) ->
      Format.fprintf fmt "(%a) -> %a" helper l helper r
    | TArrow (l, r) -> Format.fprintf fmt "%a -> %a" helper l helper r
    | TTuple (f, s, xs) ->
      Format.fprintf
        fmt
        "(%a)"
        (Format.pp_print_list ~pp_sep:(fun _ _ -> Format.printf " * ") helper)
        (f :: s :: xs)
    | TList l -> Format.fprintf fmt "%a list" helper l
  in
  helper fmt t
;;

let%expect_test _ =
  Format.printf "%a" pp_typ_my (TVar 4);
  [%expect {| 'a |}]
;;

let%expect_test _ =
  Format.printf "%a" pp_typ_my (TArrow (TVar 4, TArrow (TVar 3, TVar 4)));
  [%expect {| 'a -> 'b -> 'a |}]
;;

let%expect_test _ =
  Format.printf "%a" pp_typ_my (TArrow (TVar 2, TTuple (TVar 1, TVar 2, [ TVar 5 ])));
  [%expect {| 'a -> ('b * 'a * 'c) |}]
;;

let%expect_test _ =
  Format.printf
    "%a"
    pp_typ_my
    (TArrow (TVar 2, TTuple (TVar 1, TVar 2, [ TVar 5; TList (TVar 2) ])));
  [%expect {| 'a -> ('b * 'a * 'c * 'a list) |}]
;;
