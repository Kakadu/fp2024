(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format
open Parser

let rec shrink_tp =
  let open QCheck.Iter in
  function
  | TUnit | TInt | TBool -> empty
  | TreeParam tp -> shrink_tp tp >|= fun a' -> TreeParam a'
  | ListParam tp -> shrink_tp tp >|= fun a' -> ListParam a'
  | MaybeParam tp -> shrink_tp tp >|= fun a' -> MaybeParam a'
  | TupleParams (first, second, rest) ->
    of_list [ first; second ]
    <+> (shrink_tp first >|= fun a' -> TupleParams (a', second, rest))
    <+> (shrink_tp second >|= fun b' -> TupleParams (first, b', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_tp rest
         >|= fun c' -> TupleParams (first, second, c'))
  | FunctionType functype -> shrink_functype functype >|= fun a' -> FunctionType a'

and shrink_functype : functype QCheck.Shrink.t =
  let open QCheck.Iter in
  function
  | FuncT (first, second, rest) ->
    shrink_tp first
    >|= (fun a' -> FuncT (a', second, rest))
    <+> (shrink_tp second >|= fun b' -> FuncT (first, b', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_tp rest >|= fun c' -> FuncT (first, second, c'))
;;

let rec shrink_pat =
  let open QCheck.Iter in
  function
  | PWildcard | PConst _ | PIdentificator _ -> empty
  | PList x -> shrink_listpat x >|= fun a' -> PList a'
  | PTuple (first, second, rest) ->
    shrink_pattern first
    >|= (fun a' -> PTuple (a', second, rest))
    <+> (shrink_pattern second >|= fun b' -> PTuple (first, b', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern rest
         >|= fun c' -> PTuple (first, second, c'))
  | PMaybe Nothing -> empty
  | PMaybe (Just x) -> shrink_pattern x >|= fun a' -> PMaybe (Just a')
  | PTree x -> shrink_treepat x >|= fun a' -> PTree a'

and shrink_pattern : pattern QCheck.Shrink.t =
  let open QCheck.Iter in
  function
  | ident_list, pat, tp_list ->
    QCheck.Shrink.list ident_list
    >|= (fun a' -> a', pat, tp_list)
    <+> (shrink_pat pat >|= fun b' -> ident_list, b', tp_list)
    <+> (QCheck.Shrink.list ~shrink:shrink_tp tp_list >|= fun c' -> ident_list, pat, c')

and shrink_listpat =
  let open QCheck.Iter in
  function
  | PCons (x, y) ->
    shrink_pattern x
    >|= (fun a' -> PCons (a', y))
    <+> (shrink_pattern y >|= fun b' -> PCons (x, b'))
  | PEnum list -> QCheck.Shrink.list ~shrink:shrink_pattern list >|= fun a' -> PEnum a'

and shrink_treepat =
  let open QCheck.Iter in
  function
  | PNul -> empty
  | PNode (x, y, z) ->
    shrink_pattern x
    >|= (fun a' -> PNode (a', y, z))
    <+> (shrink_pattern y >|= fun b' -> PNode (x, b', z))
    <+> (shrink_pattern z >|= fun c' -> PNode (x, y, c'))
;;

let rec shrink_expression =
  let open QCheck.Iter in
  function
  | Const _ | Identificator _ | ENothing | EJust -> empty
  | TupleBld (x, y, rest) ->
    shrink_expr x
    >|= (fun a' -> TupleBld (a', y, rest))
    <+> (shrink_expr y >|= fun b' -> TupleBld (x, b', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr rest >|= fun c' -> TupleBld (x, y, c'))
  | ListBld x -> shrink_listbld x >|= fun a' -> ListBld a'
  | Binop (x, binop, y) ->
    shrink_expr x
    >|= (fun a' -> Binop (a', binop, y))
    <+> (shrink_expr y >|= fun b' -> Binop (x, binop, b'))
  | Neg x -> shrink_expr x >|= fun a' -> Neg a'
  | IfThenEsle (x, y, z) ->
    shrink_expr x
    >|= (fun a' -> IfThenEsle (a', y, z))
    <+> (shrink_expr y >|= fun b' -> IfThenEsle (x, b', z))
    <+> (shrink_expr z >|= fun c' -> IfThenEsle (x, y, c'))
  | FunctionApply (x, y, rest) ->
    shrink_expr x
    >|= (fun a' -> FunctionApply (a', y, rest))
    <+> (shrink_expr y >|= fun b' -> FunctionApply (x, b', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr rest >|= fun c' -> FunctionApply (x, y, c')
        )
  | Lambda (x, list, y) ->
    shrink_pattern x
    >|= (fun a' -> Lambda (a', list, y))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern list >|= fun b' -> Lambda (x, b', y))
    <+> (shrink_expr y >|= fun c' -> Lambda (x, list, c'))
  | BinTreeBld x -> shrink_binary_tree_bld x >|= fun a' -> BinTreeBld a'
  | Case (x, (y, z), list) ->
    shrink_expr x
    >|= (fun a' -> Case (a', (y, z), list))
    <+> (shrink_list (y, z) >|= fun b' -> Case (x, b', list))
    <+> (QCheck.Shrink.list ~shrink:shrink_list list >|= fun c' -> Case (x, (y, z), c'))
  | InnerBindings (x, list, y) ->
    shrink_binding x
    >|= (fun b' -> InnerBindings (b', list, y))
    <+> (QCheck.Shrink.list ~shrink:shrink_binding list
         >|= fun c' -> InnerBindings (x, c', y))
    <+> (shrink_expr y >|= fun a' -> InnerBindings (x, list, a'))

and shrink_comprehension =
  let open QCheck.Iter in
  function
  | Condition x -> shrink_expr x >|= fun a' -> Condition a'
  | Generator (x, y) ->
    shrink_pattern x
    >|= (fun a' -> Generator (a', y))
    <+> (shrink_expr y >|= fun b' -> Generator (x, b'))

and shrink_ordinarylistbld =
  let open QCheck.Iter in
  function
  | ComprehensionList (x, y, rest) ->
    shrink_expr x
    >|= (fun a' -> ComprehensionList (a', y, rest))
    <+> (shrink_comprehension y >|= fun b' -> ComprehensionList (x, b', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_comprehension rest
         >|= fun c' -> ComprehensionList (x, y, c'))
  | IncomprehensionlList list ->
    QCheck.Shrink.list ~shrink:shrink_expr list >|= fun a' -> IncomprehensionlList a'

and shrink_listbld =
  let open QCheck.Iter in
  function
  | LazyList (x, None, None) -> shrink_expr x >|= fun a' -> LazyList (a', None, None)
  | LazyList (x, Some y, None) ->
    shrink_expr x
    >|= (fun a' -> LazyList (a', Some y, None))
    <+> (shrink_expr y >|= fun b' -> LazyList (x, Some b', None))
  | LazyList (x, None, Some z) ->
    shrink_expr x
    >|= (fun a' -> LazyList (a', None, Some z))
    <+> (shrink_expr z >|= fun b' -> LazyList (x, None, Some b'))
  | LazyList (x, Some y, Some z) ->
    shrink_expr x
    >|= (fun a' -> LazyList (a', Some y, Some z))
    <+> (shrink_expr y >|= fun b' -> LazyList (x, Some b', Some z))
    <+> (shrink_expr z >|= fun c' -> LazyList (x, Some y, Some c'))
  | OrdList x -> shrink_ordinarylistbld x >|= fun a' -> OrdList a'

and shrink_binary_tree_bld =
  let open QCheck.Iter in
  function
  | Nul -> empty
  | Node (x, y, z) ->
    shrink_expr x
    >|= (fun a' -> Node (a', y, z))
    <+> (shrink_expr y >|= fun b' -> Node (x, b', z))
    <+> (shrink_expr z >|= fun c' -> Node (x, y, c'))

and shrink_list (x, y) =
  let open QCheck.Iter in
  shrink_pattern x >|= (fun a' -> a', y) <+> (shrink_bindingbody y >|= fun b' -> x, b')

and shrink_expr : expr QCheck.Shrink.t =
  let open QCheck.Iter in
  function
  | expression, tp ->
    shrink_expression expression
    >|= (fun a' -> a', tp)
    <+> (QCheck.Shrink.list ~shrink:shrink_tp tp >|= fun c' -> expression, c')

and shrink_binding =
  let open QCheck.Iter in
  function
  | VarsDef (x, y, list) ->
    shrink_pattern x
    >|= (fun a' -> VarsDef (a', y, list))
    <+> (shrink_bindingbody y >|= fun c' -> VarsDef (x, c', list))
    <+> (QCheck.Shrink.list ~shrink:shrink_binding list >|= fun b' -> VarsDef (x, y, b'))
  | FunDef (x, y, pattern_list, z, binding_list) ->
    shrink_pattern y
    >|= (fun b' -> FunDef (x, b', pattern_list, z, binding_list))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern pattern_list
         >|= fun c' -> FunDef (x, y, c', z, binding_list))
    <+> (QCheck.Shrink.list ~shrink:shrink_binding binding_list
         >|= fun d' -> FunDef (x, y, pattern_list, z, d'))
    <+> (shrink_bindingbody z >|= fun e' -> FunDef (x, y, pattern_list, e', binding_list))
  | Decl (x, y) -> shrink_tp y >|= fun b' -> Decl (x, b')

and shrink_bindingbody =
  let open QCheck.Iter in
  function
  | Guards (x, rest) ->
    shrink_expr_expr x
    >|= (fun a' -> Guards (a', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr_expr rest >|= fun b' -> Guards (x, b'))
  | OrdBody x -> shrink_expr x >|= fun a' -> OrdBody a'

and shrink_expr_expr (x, y) =
  let open QCheck.Iter in
  shrink_expr x >|= (fun a' -> a', y) <+> (shrink_expr y >|= fun b' -> x, b')
;;

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.Test.make
        ~name:"test"
        ~count:n
        (QCheck.make
           gen_binding
           ~print:(asprintf "%a" Pprintast.pp_binding)
           ~shrink:shrink_binding)
        (fun t ->
          match parse_line (asprintf "%a" Pprintast.pp_binding t) with
          | Result.Ok [ ast ] -> ast = t
          | _ -> false)
    ]
;;
