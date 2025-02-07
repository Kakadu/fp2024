(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base
open Char

(*
   |░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▒ ░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░ ░▒▓██████▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░  ░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░  ░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓████████▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░  ░▒▓█▓▒░
*)

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let pass_ws = skip_while is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let pass_ws1 = skip is_whitespace *> pass_ws

let token s = pass_ws *> string s
let pparenth stmt = token "(" *> stmt <* token ")"

let ptowhitespace = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let pident_cap =
  let first_char_str =
    satisfy (function
      | 'A' .. 'Z' -> true
      | _ -> false)
  in
  lift2 (fun fc rs -> String.make 1 fc ^ rs) first_char_str (take_while ptowhitespace)
  >>= fun ident ->
  if Ast.is_not_keyword ident
  then return ident
  else fail "Found a keyword instead of an identifier"
;;

let pident_lc =
  let first_char_str =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  lift2 (fun fc rs -> String.make 1 fc ^ rs) first_char_str (take_while ptowhitespace)
  >>= fun ident ->
  if Ast.is_not_keyword ident
  then return ident
  else fail "Found a keyword instead of an identifier"
;;

(*
   |░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓███████▓▒░ ░▒▓███████▓▒░▒▓████████▓▒░▒▓██████▓▒░░▒▓███████▓▒░▒▓████████▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░         ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░         ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░   ░▒▓█▓▒░  ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░  ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░  ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   |░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░   ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░

   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓███████▓▒░░▒▓████████▓▒░░▒▓███████▓▒░▒▓███████▓▒░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░
*)
let pconstint =
  let* number = Int.of_string <$> take_while1 is_digit in
  return (Constant.Const_integer number)
;;

let pconstchar =
  let* c = token "'" *> any_char <* token "'" in
  return (Constant.Const_char c)
;;

let pconststring =
  token "\""
  *> lift
       (fun str -> Constant.Const_string str)
       (take_while (function
         | '"' -> false
         | _ -> true))
  <* token "\""
;;

let pconst = pconstchar <|> pconstint <|> pconststring

let lchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x
;;

let rchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     let new_acc = f acc y in
     loop new_acc)
    <|> return acc
  in
  let* x = p in
  loop x
;;

(*
   |░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓████████▓▒░
   |   ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   |   ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   |   ░▒▓█▓▒░    ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░
   |   ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░
   |   ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░
   |   ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓████████▓▒░

   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓███████▓▒░░▒▓████████▓▒░░▒▓███████▓▒░▒▓███████▓▒░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░
*)

let ptypearrow = pass_ws *> token "->" >>| fun _ lhs rhs -> TypeExpr.Type_arrow (lhs, rhs)

let ptypevar =
  let* id = token "'" *> (pident_lc <|> pident_cap) in
  return (TypeExpr.Type_var id)
;;

let ptypetuple ptype =
  let* el1 = ptype in
  let* el2 = token "*" *> ptype in
  let* rest = many (token "*" *> ptype) in
  return (TypeExpr.Type_tuple (el1, el2, rest))
;;

let ptypeconstr =
  fix (fun ptconstr ->
    let* tparams =
      option
        []
        (pparenth (sep_by (token ",") ptypevar)
         <|>
         let* typevar = ptypevar in
         return [ typevar ]
         <|>
         let* ctuple = pparenth (ptypetuple ptconstr) in
         return [ ctuple ]
         <|>
         let* ttuple = pparenth (ptypetuple ptypevar) in
         return [ ttuple ])
    in
    let* tname =
      option
        None
        (let* name = pass_ws *> pident_lc in
         return (Some name))
    in
    match tname, tparams with
    | Some "", [] | None, [] | None, [ TypeExpr.Type_var _ ] ->
      fail "Type constructor cannot have a single type parameter without a name"
    | Some name, _ -> return (TypeExpr.Type_construct (name, tparams))
    | None, _ -> return (TypeExpr.Type_construct ("", tparams)))
;;

let ptype =
  pass_ws
  *> fix (fun ptype ->
    let ptvar = choice [ pparenth ptype; ptypeconstr ] in
    let pttuple = ptypetuple ptvar <|> ptvar in
    rchain pttuple ptypearrow <|> pttuple)
;;

let ptypeconstr_app =
  let* base = ptypeconstr in
  let* extra_args = sep_by (token " ") ptypeconstr in
  match extra_args with
  | [] -> return base
  | _ ->
    (match base with
     | TypeExpr.Type_construct (name, args) ->
       return (TypeExpr.Type_construct (name, args @ extra_args))
     | _ -> failwith "hahahah")
;;

let ptype_adt = pass_ws *> ptypeconstr_app <|> ptypevar

(*
   ░▒▓███████▓▒░ ░▒▓██████▓▒░▒▓████████▓▒░▒▓████████▓▒░▒▓████████▓▒░▒▓███████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓███████▓▒░░▒▓████████▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓██████▓▒░ ░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
*)

let pspecials = choice [ token "()"; token "true"; token "false"; token "None" ]

let psome parse =
  let* id = token "Some" in
  let* arg = parse >>| Option.some in
  return (id, arg)
;;

let ppatconst =
  let* const = pconst in
  return (Pattern.Pat_constant const)
;;

let ptuplepat ppattern =
  let* el1 = ppattern in
  let* el2 = token "," *> ppattern in
  let* rest = many (token "," *> ppattern) in
  return (Pattern.Pat_tuple (el1, el2, rest))
;;

let ppatvar =
  let* id = pident_lc in
  match id with
  | "_" -> return Pattern.Pat_any
  | _ -> return (Pattern.Pat_var id)
;;

let ppatconstruct (ppattern : Pattern.t Angstrom.t) =
  let* name = pident_cap in
  let* arg = option None (ppattern >>| Option.some) in
  return (Pattern.Pat_construct (name, arg))
;;

let ppatconstraint ppattern =
  let* pat = token "(" *> ppattern in
  let* pattype = token ":" *> pass_ws *> ptype <* token ")" in
  return (Pattern.Pat_constraint (pat, pattype))
;;

let ppattern =
  fix (fun ppattern ->
    let poprnd =
      fix (fun poprnd ->
        pass_ws
        *> choice
             [ (pspecials >>| fun name -> Pattern.Pat_construct (name, None))
             ; ppatvar
             ; ppatconst
             ; (psome ppattern >>| fun (name, opt) -> Pattern.Pat_construct (name, opt))
             ; ppatconstruct poprnd
             ; pparenth ppattern
             ; ppatconstraint ppattern
             ])
    in
    ptuplepat poprnd <|> poprnd)
;;

(*
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓███████▓▒░░▒▓████████▓▒░░▒▓███████▓▒░▒▓███████▓▒░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░
*)

let pexprconst =
  let* const = pconst in
  return (Expression.Exp_constant const)
;;

let pidentexpr =
  pident_lc
  >>= fun ident ->
  if is_not_keyword ident
  then return (Expression.Exp_ident ident)
  else fail "Found a keyword instead of an identifier"
;;

let pcase pexpr =
  let* first = ppattern in
  let* second = token "->" *> pexpr in
  return { Expression.first; second }
;;

let ppatternmatching pexpr =
  let* casefs = option "" (token "|") *> pcase pexpr in
  let* casetl = option "" (token "|") *> (sep_by (token "|") @@ pcase pexpr) in
  return (casefs, casetl)
;;

let pfunction pexpr =
  let* cases = token "function" *> ppatternmatching pexpr in
  return (Expression.Exp_function cases)
;;

let pmatch pexpr =
  let* expr = token "match" *> pexpr in
  let* cases = token "with" *> ppatternmatching pexpr in
  return (Expression.Exp_match (expr, cases))
;;

let pletbinding pexpr =
  let psimple =
    let* pat = ppattern in
    let* expr = token "=" *> pexpr in
    return { Expression.pat; expr }
  in
  let pfun =
    let* pat = pass_ws *> ppatvar in
    let* parameterfs = ppattern in
    let* parametertl = many ppattern in
    let* exprw = token "=" *> pexpr in
    let expr = Expression.Exp_fun ((parameterfs, parametertl), exprw) in
    return { Expression.pat; expr }
  in
  choice [ psimple; pfun ]
;;

let plethelper pexpr =
  let precflag =
    token "rec" *> pass_ws1 *> return Expression.Recursive
    <|> return Expression.Nonrecursive
  in
  let* recflag = token "let" *> precflag in
  let* bindingfs = pletbinding pexpr in
  let* bindingtl = many (token "and" *> pletbinding pexpr) in
  return (recflag, bindingfs, bindingtl)
;;

let pletexpr pexpr =
  let* recflag, bindingfs, bindingtl = plethelper pexpr in
  let* expr = token "in" *> pass_ws *> pexpr in
  return (Expression.Exp_let (recflag, (bindingfs, bindingtl), expr))
;;

let ptupleexpr pexpr =
  let* el1 = pexpr in
  let* el2 = token "," *> pexpr in
  let* rest = many (token "," *> pexpr) in
  return (Expression.Exp_tuple (el1, el2, rest))
;;

let pifexpr pexpr =
  let* condition = token "if" *> pass_ws1 *> pexpr in
  let* thenexpr = token "then" *> pass_ws1 *> pexpr in
  let* elseexpr =
    option None (pass_ws1 *> token "else" >>| Option.some)
    >>= function
    | None -> return None
    | Some _ -> pexpr >>| Option.some
  in
  return (Expression.Exp_if (condition, thenexpr, elseexpr))
;;

let pfunexpr pexpr =
  lift3
    (fun first_pattern rest_patterns body_expr ->
      Expression.Exp_fun ((first_pattern, rest_patterns), body_expr))
    (token "fun" *> ppattern)
    (many ppattern)
    (token "->" *> pexpr)
;;

let rec parseprefop pexpr pop =
  (let* f = pop in
   let* expr = parseprefop pexpr pop in
   return @@ f expr)
  <|> pexpr
;;

let parsebinop binoptoken =
  token binoptoken
  *> return (fun e1 e2 ->
    Expression.Exp_apply (Exp_ident binoptoken, Exp_tuple (e1, e2, [])))
;;

let padd = parsebinop "+"
let psub = parsebinop "-"
let pdiv = parsebinop "/"
let pmul = parsebinop "*"

let pcompops =
  choice
    [ parsebinop ">="
    ; parsebinop "<="
    ; parsebinop "<>"
    ; parsebinop "<"
    ; parsebinop ">"
    ; parsebinop "="
    ]
;;

let plogops = choice [ parsebinop "&&"; parsebinop "||" ]

let pexprconstraint pexpr =
  let* expr = token "(" *> pexpr in
  let* exprtype = token ":" *> ptype <* token ")" in
  return (Expression.Exp_constraint (expr, exprtype))
;;

let papplyexpr =
  pass_ws
  >>| fun _ lhs rhs ->
  match lhs with
  | Expression.Exp_construct (id, None) -> Expression.Exp_construct (id, Some rhs)
  | _ -> Exp_apply (lhs, rhs)
;;

let pexpr =
  fix (fun pexpr ->
    let poprnd =
      pass_ws
      *> choice
           [ (pspecials >>| fun name -> Expression.Exp_construct (name, None))
           ; pparenth pexpr
           ; pidentexpr
           ; pexprconstraint pexpr
           ; (pident_cap >>| fun id -> Expression.Exp_construct (id, None))
           ; pexprconst
           ; (psome pexpr >>| fun (name, opt) -> Expression.Exp_construct (name, opt))
           ; pfunction pexpr
           ; pfunexpr pexpr
           ; pletexpr pexpr
           ; pifexpr pexpr
           ; pmatch pexpr
           ]
    in
    let pconstructor_apply =
      let* constr =
        pparenth (pident_cap >>| fun id -> Expression.Exp_construct (id, None))
      in
      let* arg = poprnd in
      return (Expression.Exp_apply (constr, arg))
    in
    let papply = lchain (pconstructor_apply <|> poprnd) papplyexpr in
    let prefop =
      parseprefop
        papply
        (choice [ token "+"; token "-" ]
         >>| fun id expr -> Expression.Exp_apply (Exp_ident id, expr))
      <|> papply
    in
    let pmuldiv = lchain prefop (pmul <|> pdiv) in
    let paddsub = lchain pmuldiv (padd <|> psub) in
    let pcompare = lchain paddsub pcompops in
    let plogop = rchain pcompare plogops in
    ptupleexpr plogop <|> plogop)
;;

(*
   |░▒▓███████▓▒░▒▓████████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓████████▓▒░
   ░▒▓█▓▒░         ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░         ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   |░▒▓██████▓▒░   ░▒▓█▓▒░   ░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓██████▓▒░
   |      ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   |      ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓███████▓▒░   ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░ ░▒▓██████▓▒░  ░▒▓█▓▒░    ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░

   ░▒▓█▓▒░▒▓████████▓▒░▒▓████████▓▒░▒▓██████████████▓▒░ ░▒▓███████▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓██████▓▒░ ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░
*)

let pseval = lift (fun expr -> Structure.Str_eval expr) pexpr

let pstrlet =
  let* recflag, bindingfs, bindingtl = plethelper pexpr in
  return (Structure.Str_value (recflag, (bindingfs, bindingtl)))
;;

let pstradt =
  let* _ = token "type" in
  let* type_param =
    option
      []
      (pparenth (sep_by (token ",") (token "'" *> pident_lc))
       <|> many (token "'" *> pident_lc))
  in
  let* type_name = pass_ws *> pident_lc in
  let var =
    let* cname = pass_ws *> pident_cap in
    let* ctype = option [] (token "of" *> sep_by (token "*") ptype_adt) in
    return (cname, ctype)
  in
  let* fvar = token "=" *> var in
  let* varl = many (token "|" *> var) in
  return (Structure.Str_adt (type_param, type_name, (fvar, varl)))
;;

let pstr_item = pseval <|> pstrlet <|> pstradt

let pstructure =
  let psemicolon = many (token ";;") in
  sep_by psemicolon pstr_item <* psemicolon <* pass_ws
;;

let parse str = parse_string ~consume:All pstructure str
let parse_str str = parse str |> Result.ok_or_failwith
