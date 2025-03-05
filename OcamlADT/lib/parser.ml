(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base
open Char

(*
   |    _       _   _  __  __               _                    _       ____     __   __
   |U  /"\  uU |"|u| | \ \/"/      ___     |"|        ___    U  /"\  uU |  _'\ u  \ \ / /
   | \/ _ \/  \| |\| | /\  /\     |_"_|  U | | u     |_"_|    \/ _ \/  \| |_) |/   \ V /
   | / ___ \   | |_| |U /  \ u     | |    \| |/__     | |     / ___ \   |  _ <    U_|"|_u
   |/_/   \_\ <<\___/  /_/\_\    U/| |\u   |_____|  U/| |\u  /_/   \_\  |_| \_\     |_|
   | \\    >>(__) )( ,-,>> \\_.-,_|___|_,-.//  \\.-,_|___|_,-.\\    >>  //   \\_.-,//|(_
   |(__)  (__)   (__) \_)  (__)\_)-' '-(_/(_")("_)\_)-' '-(_/(__)  (__)(__)  (__)\_) (__)
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
   |   ____   U  ___ u  _   _    ____     _____      _      _   _     _____
   |U /"___|   \/"_ \/ | \ |"|  / __"| u |_ " _| U  /"\  u | \ |"|   |_ " _|
   |\| | u     | | | |<|  \| |><\___ \/    | |    \/ _ \/ <|  \| |>    | |
   | | |/__.-,_| |_| |U| |\  |u u___) |   /| |\   / ___ \ U| |\  |u   /| |\
   |  \____|\_)-\___/  |_| \_|  |____/>> u |_|U  /_/   \_\ |_| \_|   u |_|U
   | _// \\      \\    ||   \\,-.)(  (__)_// \\_  \\    >> ||   \\,-._// \\_
   |(__)(__)    (__)   (_")  (_/(__)    (__) (__)(__)  (__)(_")  (_/(__) (__)

   |U _____ u __  __    ____      ____    U _____ u ____    ____                U  ___ u  _   _
   |\| ___'|/ \ \/"/  U|  _"\ uU |  _"\ u \| ___"|// __"| u/ __"| u      ___     \/"_ \/ | \ |"|
   | |  _|"   /\  /\  \| |_) |/ \| |_) |/  |  _|" <\___ \/<\___ \/      |_"_|    | | | |<|  \| |>
   | | |___  U /  \ u  |  __/    |  _ <    | |___  u___) | u___) |       | | .-,_| |_| |U| |\  |u
   | |_____|  /_/\_\   |_|       |_| \_\   |_____| |____/>>|____/>>    U/| |\u\_)-\___/  |_| \_|
   | <<   >>,-,>> \\_  ||>>_     //   \\_  <<   >>  )(  (__))(  (__).-,_|___|_,-.  \\    ||   \\,-.
   |(__) (__)\_)  (__)(__)__)   (__)  (__)(__) (__)(__)    (__)      \_)-' '-(_/  (__)   (_")  (_/
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
   |  _____   __   __  ____   U _____ u
   | |_ " _|  \ \ / /U|  _"\ u\| ___'|/
   |   | |     \ V / \| |_) |/ |  _|"
   |  /| |\   U_|"|_u |  __/   | |___
   | u |_|U     |_|   |_|      |_____|
   | _// \\_.-,//|(_  ||>>_    <<   >>
   |(__) (__)\_) (__)(__)__)  (__) (__)

   |U _____ u __  __    ____      ____    U _____ u ____    ____                U  ___ u  _   _
   |\| ___'|/ \ \/"/  U|  _"\ uU |  _"\ u \| ___"|// __"| u/ __"| u      ___     \/"_ \/ | \ |"|
   | |  _|"   /\  /\  \| |_) |/ \| |_) |/  |  _|" <\___ \/<\___ \/      |_"_|    | | | |<|  \| |>
   | | |___  U /  \ u  |  __/    |  _ <    | |___  u___) | u___) |       | | .-,_| |_| |U| |\  |u
   | |_____|  /_/\_\   |_|       |_| \_\   |_____| |____/>>|____/>>    U/| |\u\_)-\___/  |_| \_|
   | <<   >>,-,>> \\_  ||>>_     //   \\_  <<   >>  )(  (__))(  (__).-,_|___|_,-.  \\    ||   \\,-.
   |(__) (__)\_)  (__)(__)__)   (__)  (__)(__) (__)(__)    (__)      \_)-' '-(_/  (__)   (_")  (_/
*)

let ptypearrow = pass_ws *> token "->" >>| fun _ lhs rhs -> TypeExpr.Type_arrow (lhs, rhs)

let pmultiargsapp pty =
  let* args = pparenth @@ sep_by1 (pass_ws *> char ',') pty in
  let* id = pass_ws *> pident_lc in
  return (TypeExpr.Type_construct (id, args))
;;

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
  pass_ws
  *> fix (fun ptconstr ->
    let* tparams =
      pass_ws
      *> option
           []
           (pparenth (sep_by (token ",") ptypevar)
            <|> (let* typevar = ptypevar in
                 return [ typevar ])
            <|> (let* ctuple = pparenth (ptypetuple ptconstr) in
                 return [ ctuple ])
            <|>
            let* ttuple = pparenth (ptypetuple ptypevar) in
            return [ ttuple ])
    in
    let* tname =
      option
        None
        (let* name = pass_ws *> (pident_lc <|> pident_cap) in
         return (Some name))
    in
    match tname, tparams with
    | Some "", [] | None, [] | None, [ TypeExpr.Type_var _ ] ->
      fail "Type constructor cannot have a single type parameter without a name"
    | Some name, _ -> return (TypeExpr.Type_construct (name, tparams))
    | None, _ ->
      (match tparams with
       | x :: _ -> return x
       | _ -> fail "Not enough elementts"))
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
     | _ -> fail "Expected a type constructor, but found an incompatible expression")
;;

let ptype =
  pass_ws
  *> fix (fun ptype ->
    let ptvar =
      pass_ws
      *> choice
           [ (pident_lc >>| fun id -> TypeExpr.Type_construct (id, []))
           ; ptypevar
           ; pmultiargsapp ptype
           ; pparenth ptype
           ; ptypeconstr
           ]
    in
    let pttuple = ptypetuple ptvar <|> ptvar in
    let ptarr = rchain pttuple ptypearrow <|> pttuple in
    let* arg = ptarr in
    let rec pcons acc =
      option
        acc
        (pass_ws1 *> pident_lc >>= fun id -> pcons (TypeExpr.Type_construct (id, [ acc ])))
    in
    pcons arg)
;;

let ptype_adt = pass_ws *> ptypeconstr_app <|> ptypevar

(*
   |  ____       _       _____    _____  U _____ u   ____     _   _
   |U|  _"\ uU  /"\  u  |_ " _|  |_ " _| \| ___"|/U |  _"\ u | \ |'|
   |\| |_) |/ \/ _ \/     | |      | |    |  _|"   \| |_) |/<|  \| |>
   | |  __/   / ___ \    /| |\    /| |\   | |___    |  _ <  U| |\  |u
   | |_|     /_/   \_\  u |_|U   u |_|U   |_____|   |_| \_\  |_| \_|
   | ||>>_    \\    >>  _// \\_  _// \\_  <<   >>   //   \\_ ||   \\,-.
   |(__)__)  (__)  (__)(__) (__)(__) (__)(__) (__) (__)  (__)(_")  (_/
*)

let ppatlist ppat =
  let* list = token "[" *> sep_by (token ";") ppat <* token "]" in
  return
    (Stdlib.List.fold_right
       (fun x y ->
         Ast.Pattern.Pat_construct ("::", Some (Ast.Pattern.Pat_tuple (x, y, []))))
       list
       (Ast.Pattern.Pat_construct ("[]", None)))
;;

let ppatcons ppat =
  let rec consparser () =
    let* pat = ppat in
    token "::"
    >>= (fun c ->
          consparser ()
          >>= fun rest ->
          return
            (Ast.Pattern.Pat_construct (c, Some (Ast.Pattern.Pat_tuple (pat, rest, [])))))
    <|> return pat
  in
  consparser ()
;;

let pspecials = choice [ token "()"; token "true"; token "false"; token "None" ]

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
             ; ppatconstruct poprnd
             ; pparenth ppattern
             ; ppatconstraint ppattern
             ])
    in
    let plist = ppatlist poprnd <|> poprnd in
    let pcons = ppatcons plist <|> plist in
    ptuplepat pcons <|> pcons)
;;

(*
   |U _____ u __  __    ____      ____    U _____ u ____    ____                U  ___ u  _   _
   |\| ___'|/ \ \/"/  U|  _"\ uU |  _"\ u \| ___"|// __"| u/ __"| u      ___     \/"_ \/ | \ |"|
   | |  _|"   /\  /\  \| |_) |/ \| |_) |/  |  _|" <\___ \/<\___ \/      |_"_|    | | | |<|  \| |>
   | | |___  U /  \ u  |  __/    |  _ <    | |___  u___) | u___) |       | | .-,_| |_| |U| |\  |u
   | |_____|  /_/\_\   |_|       |_| \_\   |_____| |____/>>|____/>>    U/| |\u\_)-\___/  |_| \_|
   | <<   >>,-,>> \\_  ||>>_     //   \\_  <<   >>  )(  (__))(  (__).-,_|___|_,-.  \\    ||   \\,-.
   |(__) (__)\_)  (__)(__)__)   (__)  (__)(__) (__)(__)    (__)      \_)-' '-(_/  (__)   (_")  (_/
*)

let pexpcons expr =
  let rec consparser () =
    let* exp = expr in
    token "::"
    >>= (fun _ ->
          consparser ()
          >>= fun rest ->
          return
            (Ast.Expression.Exp_construct
               ("::", Some (Ast.Expression.Exp_tuple (exp, rest, [])))))
    <|> return exp
  in
  consparser ()
;;

let pexplist expr =
  let* list = token "[" *> sep_by (token ";") expr <* token "]" in
  return
    (Base.List.fold_right
       list
       ~f:(fun x y ->
         Ast.Expression.Exp_construct ("::", Some (Ast.Expression.Exp_tuple (x, y, []))))
       ~init:(Ast.Expression.Exp_construct ("[]", None)))
;;

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
  pass_ws
  *> option () (token "|" *> return ())
  *>
  let* first = pass_ws *> ppattern in
  let* second = token "->" *> pass_ws *> pexpr in
  return { Expression.first; second }
;;

let pfunction pexpr =
  token "function"
  *>
  let* first_case = pcase pexpr in
  let* case_list = sep_by (token "|") (pcase pexpr) in
  return (Ast.Expression.Exp_function (first_case, case_list))
;;

let pmatch pexpr =
  let* exp = token "match" *> pexpr <* token "with" in
  let* casefs = pcase pexpr in
  let* case_list = sep_by (token "|") (pcase pexpr) in
  return (Ast.Expression.Exp_match (exp, (casefs, case_list)))
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
           ; pfunction pexpr
           ; pfunexpr pexpr
           ; pexplist pexpr
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
    let pexpcons = pexpcons pcompare <|> pcompare in
    let plogop = rchain pexpcons plogops in
    let ptuple = ptupleexpr plogop <|> plogop in
    choice
      [ pfunction pexpr; pfunexpr pexpr; pletexpr pexpr; pifexpr pexpr; pmatch pexpr ]
    <|> ptuple)
;;

(*
   |  ____     _____    ____      _   _    ____   _____    _   _    ____    U _____ u
   | / __"| u |_ " _|U |  _"\ uU |"|u| |U /"___| |_ " _|U |"|u| |U |  _"\ u \| ___'|/
   |<\___ \/    | |   \| |_) |/ \| |\| |\| | u     | |   \| |\| | \| |_) |/  |  _|"
   | u___) |   /| |\   |  _ <    | |_| | | |/__   /| |\   | |_| |  |  _ <    | |___
   | |____/>> u |_|U   |_| \_\  <<\___/   \____| u |_|U  <<\___/   |_| \_\   |_____|
   |  )(  (__)_// \\_  //   \\_(__) )(   _// \\  _// \\_(__) )(    //   \\_  <<   >>
   | (__)    (__) (__)(__)  (__)   (__) (__)(__)(__) (__)   (__)  (__)  (__)(__) (__)

   |              _____  U _____ u  __  __    ____
   |     ___     |_ " _| \| ___"|/U|' \/ '|u / __"| u
   |    |_"_|      | |    |  _|"  \| |\/| |/<\___ \/
   |     | |      /| |\   | |___   | |  | |  u___) |
   |   U/| |\u   u |_|U   |_____|  |_|  |_|  |____/>>
   |.-,_|___|_,-._// \\_  <<   >> <<,-,,-.    )(  (__)
   | \_)-' '-(_/(__) (__)(__) (__) (./  \.)  (__)
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
    let* name = option None (pass_ws *> pident_cap >>= fun n -> return (Some n)) in
    match name with
    | Some x ->
      (* Constructor case: Can have "of" *)
      let* ctype =
        option
          None
          (token "of"
           *> let* types = sep_by (token "*") ptype_adt in
              match types with
              | x :: y :: xs -> return (Some (TypeExpr.Type_tuple (x, y, xs)))
              | [ x ] -> return (Some x)
              | [] -> fail "Expected type after 'of'")
      in
      return (x, ctype)
    | None ->
      (* Lowercase type alias case: Must have a type expression *)
      let* ctype =
        let* types = sep_by (token "*") ptype_adt in
        match types with
        | x :: y :: xs -> return (Some (TypeExpr.Type_tuple (x, y, xs))) (* Tuple case *)
        | [ x ] -> return (Some x) (* Single type *)
        | [] -> fail "Expected type definition"
      in
      return ("", ctype)
  in
  let* _ = token "=" in
  let* fvar =
    option
      None
      (option None (token "|" *> return None) *> (var >>= fun v -> return (Some v)))
  in
  let* varl = many (token "|" *> var) in
  match fvar with
  | Some fvar -> return (Structure.Str_adt (type_param, type_name, (fvar, varl)))
  | None -> fail "Expected at least one variant"
;;

let pstr_item = pseval <|> pstrlet <|> pstradt

let pstructure =
  let psemicolon = many (token ";;") in
  sep_by psemicolon pstr_item <* psemicolon <* pass_ws
;;

let parse str = parse_string ~consume:All pstructure str
let parse_str str = parse str |> Result.ok_or_failwith
