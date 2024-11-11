(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

(*---------------------Check conditions---------------------*)

let is_keyword = function
  | "let" | "in" | "fun" | "rec" | "if" | "then" | "else" | "true" | "false" -> true
  | _ -> false
;;

(*---------------------Control characters---------------------*)

let pwhitespace = take_while Char.is_whitespace
let pws1 = take_while1 Char.is_whitespace
let pstoken s = pwhitespace *> string s
let ptoken s = pwhitespace *> s
let pparens p = pstoken "(" *> p <* pstoken ")"
let psqparens p = pstoken "[" *> p <* pstoken "]"

(*-------------------------Constants/Variables-------------------------*)

let pint =
  pwhitespace *> take_while1 Char.is_digit
  >>= fun str ->
  match Stdlib.int_of_string_opt str with
  | Some n -> return (Int n)
  | None -> fail "Integer value exceeds the allowable range for the int type"
;;

let pbool =
  choice [ pstoken "true" *> return true; pstoken "false" *> return false ]
  >>| fun x -> Bool x
;;

let pstr = char '"' *> take_till (Char.equal '"') <* char '"' >>| fun x -> String x
let punit = pstoken "()" *> return Unit
let const = choice [ pint; pbool; pstr; punit ]

let varname =
  ptoken
    (take_while (fun ch -> Char.is_digit ch || Char.equal ch '\'')
     >>= function
     | "" ->
       take_while1 (fun ch ->
         Char.is_alpha ch || Char.is_digit ch || Char.equal ch '_' || Char.equal ch '\'')
       >>= fun str ->
       if is_keyword str
       then fail "Variable name conflicts with a keyword"
       else return str
     | _ -> fail "Variable name must not start with a digit")
;;

let pat_var = varname >>| fun x -> PVar x
let pat_const = const >>| fun x -> PConst x
let pat_any = pstoken "_" *> return PAny
let ppattern = choice [ pat_const; pat_var; pat_any ]

(*------------------Binary operators-----------------*)

let pbinop op token =
  pwhitespace *> pstoken token *> return (fun e1 e2 -> Ebin_op (op, e1, e2))
;;

let add = pbinop Add "+"
let sub = pbinop Sub "-"
let mult = pbinop Mult "*"
let div = pbinop Div "/"

let relation =
  choice
    [ pbinop Eq "="
    ; pbinop Neq "<>"
    ; pbinop Lt "<"
    ; pbinop Gt ">"
    ; pbinop Lte "<="
    ; pbinop Gte ">="
    ]
;;

let logic = choice [ pbinop And "&&"; pbinop Or "||" ]

(*------------------Unary operators-----------------*)

let punop op token = pwhitespace *> pstoken token *> return (fun e1 -> Eun_op (op, e1))
let negation = pws1 *> punop Not "not"
let neg_sign = punop Negative "-"
let pos_sign = punop Positive "+"

(*------------------------Expressions----------------------*)

let chain e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let un_chain e op =
  let rec go () = op >>= (fun unop -> go () >>= fun e -> return (unop e)) <|> e in
  go ()
;;

let plet pexpr =
  let rec pbody pexpr =
    ppattern
    >>= function
    | PVar id -> pbody pexpr <|> (pstoken "=" *> pexpr >>| fun e -> Efun ([ PVar id ], e))
    | _ -> fail "Only variable patterns are supported"
  in
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> Elet (r, id, e1, e2))
       (pstoken "rec" *> (pws1 *> return Recursive) <|> return Non_recursive)
       (pparens varname <|> varname)
       (pstoken "=" *> pexpr <|> pbody pexpr)
       (pstoken "in" *> pexpr <|> return (Econst Unit))
;;

let pEfun pexpr =
  pstoken "fun" *> many1 ppattern
  >>= fun args -> pstoken "->" *> pexpr >>| fun body -> Efun (args, body)
;;

let pElist pexpr =
  let semicols = pstoken ";" in
  (* let pexpr = psqparens pexpr in *)
  psqparens (sep_by semicols pexpr <* (semicols <|> pwhitespace) >>| fun x -> Elist x)
;;

let pEtuple pexpr =
  let commas = pstoken "," in
  pparens (sep_by commas pexpr <* (commas <|> pwhitespace) >>| fun x -> Etuple x)
;;

let pEconst = const >>| fun x -> Econst x
let pEvar = varname >>| fun x -> Evar x
let pEapp e = chain e (return (fun e1 e2 -> Efun_application (e1, e2)))

let pbranch pexpr =
  lift3
    (fun e1 e2 e3 -> Eif_then_else (e1, e2, e3))
    (pstoken "if" *> pexpr)
    (pstoken "then" *> pexpr)
    (pstoken "else" *> pexpr >>| (fun e3 -> Some e3) <|> return None)
;;

let pexpr =
  fix (fun expr ->
    let atom_expr =
      choice [ pEconst; pEvar; pparens expr; pElist expr; pEtuple expr; pEfun expr ]
    in
    let let_expr = plet expr in
    let ite_expr = pbranch (expr <|> atom_expr) <|> atom_expr in
    let app_expr = pEapp (ite_expr <|> atom_expr) <|> ite_expr in
    let un_expr =
      choice
        [ un_chain app_expr negation
        ; un_chain app_expr neg_sign
        ; un_chain app_expr pos_sign
        ]
    in
    let factor_expr = chain un_expr (mult <|> div) in
    let sum_expr = chain factor_expr (add <|> sub) in
    let rel_expr = chain sum_expr relation in
    let log_expr = chain rel_expr logic in
    choice [ let_expr; log_expr ])
;;

let pstructure =
  let pseval = pexpr >>| fun e -> SEval e in
  let psvalue =
    plet pexpr
    >>| function
    | Elet (r, id, e1, e2) -> SValue (r, id, e1, e2)
    | _ -> failwith "Expected a let expression"
  in
  choice [ psvalue; pseval ]
;;

let structure : structure t =
  sep_by (pstoken ";;") pstructure <* (pstoken ";;" <|> pwhitespace)
;;

let parse_expr str = parse_string ~consume:All structure str

(*
   let pp_const = function
   | Int i -> string_of_int i
   | String s -> "\"" ^ s ^ "\""
   | Bool b -> string_of_bool b
   | Unit -> "()"
   ;;

   let pp_bin_op = function
   | Add -> "+"
   | Mult -> "*"
   | Sub -> "-"
   | Div -> "/"
   | Gt -> ">"
   | Lt -> "<"
   | Eq -> "="
   | Neq -> "<>"
   | Gte -> ">="
   | Lte -> "<="
   | And -> "&&"
   | Or -> "||"
   ;;

   let pp_un_op = function
   | Negative -> "-"
   | Positive -> "+"
   | Not -> "not"
   ;;

   let pp_rec_flag = function
   | Recursive -> "rec"
   | Non_recursive -> ""
   ;;

   let pp_pattern = function
   | PVar id -> id
   | PConst c -> pp_const c
   | PAny -> "_"
   ;;

   let rec pp_expr = function
   | Econst c -> pp_const c
   | Evar id -> id
   | Eif_then_else (e1, e2, None) ->
   "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2
   | Eif_then_else (e1, e2, Some e3) ->
   "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2 ^ " else " ^ pp_expr e3
   | Etuple es ->
   "(" ^ String.concat ~sep:", " (List.map ~f:pp_expr es) ^ ")"
   | Elist es ->
   "[" ^ String.concat ~sep:"; " (List.map ~f:pp_expr es) ^ "]"
   | Efun (patterns, e) ->
   "fun " ^ String.concat ~sep:" " (List.map ~f:pp_pattern patterns) ^ " -> " ^ pp_expr e
   | Ebin_op (op, e1, e2) ->
   "(" ^ pp_expr e1 ^ " " ^ pp_bin_op op ^ " " ^ pp_expr e2 ^ ")"
   | Eun_op (op, e) ->
   pp_un_op op ^ pp_expr e
   | Elet (rec_flag, id, e1, e2) ->
   "let " ^ pp_rec_flag rec_flag ^ " " ^ id ^ " = " ^ pp_expr e1 ^ " in " ^ pp_expr e2
   | Efun_application (e1, e2) ->
   pp_expr e1 ^ " " ^ pp_expr e2
   ;;

   let pp_structure_item (item: structure_item) : string =
   match item with
   | SEval e -> pp_expr e ^ ";"
   | SValue (rec_flag, id, e1, e2) ->
   "let " ^ pp_rec_flag rec_flag ^ " " ^ id ^ " = " ^ pp_expr e1 ^ " in " ^ pp_expr e2

   let prpr_structure fmt structure =
   List.iter ~f:(fun item -> Format.fprintf fmt "%s@." (pp_structure_item item)) structure
   ;;
*)
