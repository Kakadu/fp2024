[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open FSharpActivePatterns.Ast
open FSharpActivePatterns.AstPrinter
open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrettyPrinter

let bin_e op e1 e2 = Bin_expr (op, e1, e2)

let shrink_lt =
  let open QCheck.Iter in
  function
  | Int_lt x -> QCheck.Shrink.int x >|= fun a' -> Int_lt a'
  | Bool_lt _ -> empty
  | Unit_lt -> empty
  | String_lt x -> QCheck.Shrink.string x >|= fun a' -> String_lt a'
;;

let exprs_from_let_binds let_binds =
  List.map
    (function
      | Let_bind (_, _, e) -> e)
    let_binds
;;

let rec shrink_let_bind =
  let open QCheck.Iter in
  function
  | Let_bind (name, args, e) ->
    shrink_expr e
    >|= (fun a' -> Let_bind (name, args, a'))
    <+> (QCheck.Shrink.list args >|= fun a' -> Let_bind (name, a', e))
    <+> (shrink_pattern name >|= fun a' -> Let_bind (a', args, e))

and shrink_expr =
  let open QCheck.Iter in
  function
  | Const lt -> shrink_lt lt >|= fun a' -> Const a'
  | Tuple (e1, e2, rest) ->
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun a' -> Tuple (a', e2, rest))
    <+> (shrink_expr e2 >|= fun a' -> Tuple (e1, a', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr rest >|= fun a' -> Tuple (e1, e2, a'))
  | List l -> QCheck.Shrink.list ~shrink:shrink_expr l >|= fun l' -> List l'
  | Bin_expr (op, e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun a' -> bin_e op a' e2)
    <+> (shrink_expr e2 >|= fun a' -> bin_e op e1 a')
  | Unary_expr (op, e) -> return e <+> (shrink_expr e >|= fun e' -> Unary_expr (op, e'))
  | If_then_else (i, t, Some e) ->
    of_list [ i; t; e; If_then_else (i, e, None) ]
    <+> (shrink_expr i >|= fun a' -> If_then_else (a', t, Some e))
    <+> (shrink_expr t >|= fun a' -> If_then_else (i, a', Some e))
  | If_then_else (i, t, None) ->
    of_list [ i; t ]
    <+> (shrink_expr i >|= fun a' -> If_then_else (a', t, None))
    <+> (shrink_expr t >|= fun a' -> If_then_else (i, a', None))
  | LetIn (rec_flag, let_bind, let_bind_list, inner_e) ->
    of_list (inner_e :: exprs_from_let_binds (let_bind :: let_bind_list))
    <+> (shrink_let_bind let_bind
         >|= fun a' -> LetIn (rec_flag, a', let_bind_list, inner_e))
    <+> (QCheck.Shrink.list ~shrink:shrink_let_bind let_bind_list
         >|= fun a' -> LetIn (rec_flag, let_bind, a', inner_e))
    <+> (shrink_expr inner_e >|= fun a' -> LetIn (rec_flag, let_bind, let_bind_list, a'))
  | Apply (f, arg) ->
    of_list [ f; arg ]
    <+> (shrink_expr f >|= fun a' -> Apply (a', arg))
    <+> (shrink_expr arg >|= fun a' -> Apply (f, a'))
  | Lambda (pat, pat_list, body) ->
    shrink_expr body
    >|= (fun body' -> Lambda (pat, pat_list, body'))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern pat_list
         >|= fun a' -> Lambda (pat, a', body))
  | Function ((pat1, expr1), cases) ->
    of_list (expr1 :: List.map snd cases)
    <+> (shrink_pattern pat1 >|= fun a' -> Function ((a', expr1), cases))
    <+> (shrink_expr expr1 >|= fun a' -> Function ((pat1, a'), cases))
    <+> (QCheck.Shrink.list
           ~shrink:(fun (p, e) ->
             (let* p_shr = shrink_pattern p in
              return (p_shr, e))
             <+>
             let* e_shr = shrink_expr e in
             return (p, e_shr))
           cases
         >|= fun a' -> Function ((pat1, expr1), a'))
  | Match (value, (pat1, expr1), cases) ->
    of_list (value :: expr1 :: List.map snd cases)
    <+> (shrink_expr value >|= fun a' -> Match (a', (pat1, expr1), cases))
    <+> (shrink_pattern pat1 >|= fun a' -> Match (value, (a', expr1), cases))
    <+> (shrink_expr expr1 >|= fun a' -> Match (value, (pat1, a'), cases))
    <+> (QCheck.Shrink.list
           ~shrink:(fun (p, e) ->
             (let* p_shr = shrink_pattern p in
              return (p_shr, e))
             <+>
             let* e_shr = shrink_expr e in
             return (p, e_shr))
           cases
         >|= fun a' -> Match (value, (pat1, expr1), a'))
  | Option (Some e) ->
    of_list [ e; Option None ] <+> (shrink_expr e >|= fun a' -> Option (Some a'))
  | Option None -> empty
  | Variable _ -> empty
  | EConstraint (e, t) -> return e <+> shrink_expr e >|= fun a' -> EConstraint (a', t)

and shrink_pattern =
  let open QCheck.Iter in
  function
  | PList l -> QCheck.Shrink.list ~shrink:shrink_pattern l >|= fun l' -> PList l'
  | PCons (l, r) ->
    shrink_pattern l
    >|= (fun l' -> PCons (l', r))
    <+> (shrink_pattern r >|= fun r' -> PCons (l, r'))
  | PTuple (p1, p2, rest) ->
    of_list [ p1; p2 ]
    <+> (shrink_pattern p1 >|= fun p1' -> PTuple (p1', p2, rest))
    <+> (shrink_pattern p2 >|= fun p2' -> PTuple (p1, p2', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern rest
         >|= fun rest' -> PTuple (p1, p2, rest'))
  | PConst lt -> shrink_lt lt >|= fun lt' -> PConst lt'
  | POption (Some p) -> return p
  | POption None -> empty
  | Wild -> empty
  | PVar _ -> empty
  | PConstraint (p, _) -> return p
;;

let shrink_statement =
  let open QCheck.Iter in
  function
  | Let (rec_flag, let_bind, let_bind_list) ->
    shrink_let_bind let_bind
    >|= (fun a' -> Let (rec_flag, a', let_bind_list))
    <+> (QCheck.Shrink.list ~shrink:shrink_let_bind let_bind_list
         >|= fun a' -> Let (rec_flag, let_bind, a'))
    <+>
      (match let_bind_list with
      | [] -> empty
      | hd :: _ -> return (Let (rec_flag, hd, [])))
;;

let shrink_construction =
  let open QCheck.Iter in
  function
  | Expr e -> shrink_expr e >|= fun a' -> Expr a'
  | Statement s ->
    shrink_statement s
    >|= (fun a' -> Statement a')
    <+>
      (match s with
      | Let (_, let_bind, let_binds) ->
        of_list (exprs_from_let_binds (let_bind :: let_binds)) >|= fun a' -> Expr a')
;;

let arbitrary_construction =
  QCheck.make
    gen_construction
    ~print:
      (let open Format in
       asprintf "%a" (fun fmt c ->
         let pp = print_construction in
         fprintf fmt "Generated:\n%a" pp c;
         match parse (Format.asprintf "%a\n" pp c) with
         | Ok parsed -> fprintf fmt "Parsed:\n%a" pp parsed
         | Error e -> fprintf fmt "Parsing error:\n%s\n" e))
    ~shrink:shrink_construction
;;

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_construction ~count:n (fun c ->
          Ok c = parse (Format.asprintf "%a\n" pp_construction c)))
    ]
;;
