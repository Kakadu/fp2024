(* * Copyright 2024, Victoria Lutsyuk

   (** SPDX-License-Identifier: MIT *)

   open Ast
   open Format
   open TypTree

   let pp_binop ppf = function
   | Add -> fprintf ppf "+"
   | Sub -> fprintf ppf "-"
   | Mul -> fprintf ppf "*"
   | Div -> fprintf ppf "/"
   | Lt -> fprintf ppf "<"
   | Gt -> fprintf ppf ">"
   | Eq -> fprintf ppf "="
   | Ne -> fprintf ppf "<>"
   | Le -> fprintf ppf "<="
   | Ge -> fprintf ppf ">="
   | And -> fprintf ppf "&&"
   | Or -> fprintf ppf "||"
   ;;

   let pp_typ =
   let rec helper ppf = function
   | TypConst TInt -> fprintf ppf "int"
   | TypConst TBool -> fprintf ppf "bool"
   | TypConst TStr -> fprintf ppf "string"
   | TypConst TUnit -> fprintf ppf "unit"
   | TypVar ty -> fprintf ppf "'%d" ty
   | TypArrow (l, r) ->
   (match l with
   | TypArrow _ -> fprintf ppf "(%a) -> %a" helper l helper r
   | _ -> fprintf ppf "%a -> %a" helper l helper r)
   | TypList t -> fprintf ppf "%a list" helper t
   | TypTuple l ->
   let rec pp_tuple ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" helper x
   | x :: xs ->
   fprintf ppf "%a * " helper x;
   pp_tuple ppf xs
   in
   fprintf ppf "%a" pp_tuple l
   | TypOption op -> fprintf ppf "%a option" helper op
   in
   helper
   ;;

   let pp_rec_flag ppf = function
   | NonRec -> fprintf ppf ""
   | Rec -> fprintf ppf " rec"
   ;;

   let pp_constant ppf = function
   | Int i -> fprintf ppf "%d" i
   | Bool b -> fprintf ppf "%b" b
   | Str s -> fprintf ppf "%S" s
   | Unit -> fprintf ppf "()"
   ;;

   let pp_pattern =
   let rec helper ppf = function
   | PatAny -> fprintf ppf "_"
   | PatConst l -> fprintf ppf "%a" pp_constant l
   | PatVar v -> fprintf ppf "%s" v
   | PatListCons (p1, p2) ->
   (match p1, p2 with
   | PatListCons _, PatListCons _ -> fprintf ppf "(%a)::(%a)" helper p1 helper p2
   | PatListCons _, _ -> fprintf ppf "(%a)::(%a)" helper p1 helper p2
   | _, PatListCons _ -> fprintf ppf "(%a)::(%a)" helper p1 helper p2
   | _ -> fprintf ppf "%a::%a" helper p1 helper p2)
   | PatTup (p1, p2, rest) ->
   let pp_tuple_helper ppf x = fprintf ppf "%a" helper x in
   let rec pp_tuple ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" pp_tuple_helper x
   | x :: xs ->
   fprintf ppf "%a, " pp_tuple_helper x;
   pp_tuple ppf xs
   in
   fprintf ppf "(%a)" pp_tuple (p1 :: p2 :: rest)
   | PatList (p1 :: rest) ->
   let pp_tuple_helper ppf x = fprintf ppf "%a" helper x in
   let rec pp_tuple ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" pp_tuple_helper x
   | x :: xs ->
   fprintf ppf "%a; " pp_tuple_helper x;
   pp_tuple ppf xs
   in
   fprintf ppf "[%a]" pp_tuple (p1 :: rest)
   | PatTyp (pat, tp) -> fprintf ppf "(%a : %a)" helper pat pp_annot tp
   in
   helper
   ;;

   let rec pp_expr =
   let rec helper ppf = function
   | Var v -> fprintf ppf "%s" v
   | Const l -> fprintf ppf "%a" pp_constant l
   | BinOp (op, e1, e2) ->
   (match e1, e2 with
   | Var _, Var _ -> fprintf ppf "%a %a %a" helper e1 pp_binop op helper e2
   | Const _, Const _ -> fprintf ppf "%a %a %a" helper e1 pp_binop op helper e2
   | Var _, _ | Const _, _ -> fprintf ppf "%a %a (%a)" helper e1 pp_binop op helper e2
   | _, Var _ | _, Const _ -> fprintf ppf "(%a) %a %a" helper e1 pp_binop op helper e2
   | _ -> fprintf ppf "((%a) %a (%a))" helper e1 pp_binop op helper e2)
   | Branch (c, th, el) ->
   let ppifexpr_helper ppf e =
   match e with
   | Var _ | ExprConstant _ -> fprintf ppf "%a" helper e
   | _ -> fprintf ppf "(%a)" helper e
   in
   let ppifexpr = function
   | None -> fprintf ppf "if %a then %a" ppifexpr_helper c ppifexpr_helper th
   | Some x ->
   fprintf
   ppf
   "if %a then %a else %a"
   ppifexpr_helper
   c
   ppifexpr_helper
   th
   ppifexpr_helper
   x
   in
   ppifexpr el
   | ExprMatch (e, branch, branches) ->
   fprintf ppf "match %a with\n" helper e;
   let ppmatch ppf branches =
   let pattern, branch_expr = branches in
   match branch_expr with
   | ExprVariable _ | ExprConstant _ ->
   fprintf ppf "| %a -> %a" pp_pattern pattern helper branch_expr
   | _ -> fprintf ppf "| %a -> (%a)" pp_pattern pattern helper branch_expr
   in
   let rec ppmatch_helper ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" ppmatch x
   | x :: xs ->
   fprintf ppf "%a\n" ppmatch x;
   ppmatch_helper ppf xs
   in
   ppmatch_helper ppf (branch :: branches)
   | ExprFunction (branch, branches) ->
   fprintf ppf "function\n";
   let ppmatch ppf branches =
   let pattern, branch_expr = branches in
   match branch_expr with
   | ExprVariable _ | ExprConstant _ ->
   fprintf ppf "| %a -> %a" pp_pattern pattern helper branch_expr
   | _ -> fprintf ppf "| %a -> (%a)" pp_pattern pattern helper branch_expr
   in
   let rec ppfunction_helper ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" ppmatch x
   | x :: xs ->
   fprintf ppf "%a\n" ppmatch x;
   ppfunction_helper ppf xs
   in
   ppfunction_helper ppf (branch :: branches)
   | ExprLet (rf, b, bl, e) ->
   fprintf ppf "let%a %a in %a" pp_rec_flag rf pp_binding_list (b :: bl) helper e
   | ExprApply (e1, e2) ->
   (match e1, e2 with
   | ExprVariable _, ExprVariable _ -> fprintf ppf "%a %a" helper e1 helper e2
   | _, ExprVariable _ -> fprintf ppf "(%a) %a" helper e1 helper e2
   | ExprVariable _, _ -> fprintf ppf "%a (%a)" helper e1 helper e2
   | _ -> fprintf ppf "(%a) (%a)" helper e1 helper e2)
   | ExprTuple (p1, p2, rest) ->
   let pp_tuple_helper ppf x =
   match x with
   | ExprBinOperation _
   | ExprUnOperation _
   | ExprIf _
   | ExprMatch _
   | ExprLet _
   | ExprApply _
   | ExprOption _
   | ExprFun _ -> fprintf ppf "(%a)" helper x
   | _ -> fprintf ppf "%a" helper x
   in
   let rec pp_tuple ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" pp_tuple_helper x
   | x :: xs ->
   fprintf ppf "%a, " pp_tuple_helper x;
   pp_tuple ppf xs
   in
   fprintf ppf "(%a)" pp_tuple (p1 :: p2 :: rest)
   | List (h, t) ->
   let pp_list_helper ppf x =
   match x with
   | BinOp _ | Branch _ | Let _ | App _ | Fun _ -> fprintf ppf "(%a)" helper x
   | _ -> fprintf ppf "%a" helper x
   in
   let rec pp_list ppf = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" pp_list_helper x
   | x :: xs ->
   fprintf ppf "%a; " pp_list_helper x;
   pp_list ppf xs
   in
   fprintf ppf "[%a]" pp_list (h :: t)
   | Fun (p, e) -> fprintf ppf "fun %a -> %a" pp_pattern p helper e
   | Option x ->
   (match x with
   | Some x -> fprintf ppf "Some (%a)" helper x
   | None -> fprintf ppf "None")
   | Typ (e, annot) -> fprintf ppf "(%a : %a)" helper e pp_annot annot
   in
   helper

   and pp_binding_list ppf =
   let pp_binding ppf binding =
   let p, e = binding in
   match e with
   | Fun (p1, e1) -> fprintf ppf "%a %a = %a" pp_pattern p pp_pattern p1 pp_expr e1
   | _ -> fprintf ppf "%a = %a" pp_pattern p pp_expr e
   in
   let rec helper = function
   | [] -> ()
   | [ x ] -> fprintf ppf "%a" pp_binding x
   | x :: xs ->
   fprintf ppf "%a and " pp_binding x;
   helper xs
   in
   helper
   ;;

   let pp_structure ppf = function
   | SEval e -> fprintf ppf "%a" pp_expr e
   | SValue (rf, b, bl) -> fprintf ppf "let%a %a" pp_rec_flag rf pp_binding_list (b :: bl)
   ;;

   let pp_structure_item_list ppf structure_list =
   List.iter (fun item -> fprintf ppf "%a;;\n\n" pp_structure item) structure_list
   ;; *)
