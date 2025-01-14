(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Shrink
open QCheck.Iter
open Ast

let list ~shrink l =
  match l with
  | _ :: _ :: _ -> of_list ([ [] ] @ List.map (fun elem -> [ elem ]) l) <+> list ~shrink l
  | elem :: _ -> return [] <+> (shrink elem >|= fun elem -> [ elem ])
  | [] -> empty
;;

let shrink_ident = function
  | "a" -> empty
  | _ -> return "a"
;;

let rec shrink_type = function
  | Type_int | Type_string | Type_bool -> empty
  | Type_array (_, type') ->
    return Type_int
    <+> (shrink_type type' >|= fun t -> Type_array (0, t))
    <+> return type'
  | Type_func (arg_types, return_types) ->
    return Type_int
    <+> (list ~shrink:shrink_type arg_types
         >|= fun new_arg_types -> Type_func (new_arg_types, return_types))
    <+> (list ~shrink:shrink_type return_types
         >|= fun new_return_types -> Type_func (arg_types, new_return_types))
  | Type_chan (chan_dir, type') ->
    return Type_int
    <+> (shrink_type type' >|= fun t -> Type_chan (chan_dir, t))
    <+> return type'
;;

let shrink_id_and_type id_and_t =
  let ident, type' = id_and_t in
  (let* new_id = shrink_ident ident in
   return (new_id, type'))
  <+> let* new_type = shrink_type type' in
      return (ident, new_type)
;;

let shrink_anon_func shblock anon_func =
  if anon_func = { args = []; returns = None; body = [] }
  then empty
  else
    return { args = []; returns = None; body = [] }
    <+>
    let { args; returns; body } = anon_func in
    (let* new_args = list ~shrink:shrink_id_and_type args in
     return { args = new_args; returns; body })
    <+> (let* new_returns =
           match returns with
           | Some (Ident_and_types (first, hd :: tl)) ->
             let* new_ident_and_types =
               list ~shrink:shrink_id_and_type (first :: hd :: tl)
             in
             (match new_ident_and_types with
              | hd :: tl -> return (Some (Ident_and_types (hd, tl)))
              | [] -> return None)
           | Some (Ident_and_types (pair, [])) ->
             let* new_pair = shrink_id_and_type pair in
             of_list [ None; Some (Ident_and_types (new_pair, [])) ]
           | Some (Only_types (first, hd :: tl)) ->
             let* new_types = list ~shrink:shrink_type (first :: hd :: tl) in
             (match new_types with
              | hd :: tl -> return (Some (Only_types (hd, tl)))
              | [] -> return None)
           | Some (Only_types (type', [])) ->
             let* new_type = shrink_type type' in
             of_list [ None; Some (Only_types (new_type, [])) ]
           | None -> empty
         in
         return { args; returns = new_returns; body })
    <+>
    let* new_body = shblock body in
    return { args; returns; body = new_body }
;;

let shrink_const shexpr shblock = function
  | Const_int num ->
    (match num with
     | 0 -> empty
     | _ -> return (Const_int 0))
  | Const_string str ->
    (match str with
     | "" -> empty
     | _ -> return (Const_string ""))
  | Const_array (_, type', inits) ->
    return (Const_int 0)
    <+> (let* new_type = shrink_type type' in
         return (Const_array (0, new_type, inits)))
    <+>
    let* new_inits = list ~shrink:shexpr inits in
    return (Const_array (0, type', new_inits))
  | Const_func anon_func ->
    return (Const_int 0)
    <+> let* new_anon_func = shrink_anon_func shblock anon_func in
        return (Const_func new_anon_func)
;;

let shrink_func_call shexpr call =
  let func, args = call in
  return (Expr_ident "a", [])
  <+> (let* new_func = shexpr func in
       return (new_func, args))
  <+> let* new_args = list ~shrink:shexpr args in
      return (func, new_args)
;;

let rec shrink_expr shblock = function
  | Expr_ident id -> shrink_ident id >|= fun id -> Expr_ident id
  | Expr_const const ->
    return (Expr_ident "a")
    <+> (shrink_const (shrink_expr shblock) shblock const >|= fun c -> Expr_const c)
  | Expr_index (array, index) ->
    return (Expr_ident "a")
    <+> (let* new_array = (shrink_expr shblock) array in
         return (Expr_index (new_array, index)))
    <+>
    let* new_index = (shrink_expr shblock) index in
    return (Expr_index (array, new_index))
  | Expr_bin_oper (op, left, right) ->
    return (Expr_ident "a")
    <+> return left
    <+> return right
    <+> (let* new_right = shrink_expr shblock right in
         return (Expr_bin_oper (op, left, new_right)))
    <+>
    let* new_left = shrink_expr shblock left in
    return (Expr_bin_oper (op, new_left, right))
  | Expr_un_oper (op, expr) ->
    return (Expr_ident "a")
    <+> return expr
    <+> let* new_expr = shrink_expr shblock expr in
        return (Expr_un_oper (op, new_expr))
  | Expr_chan_receive expr ->
    return (Expr_ident "a")
    <+> return expr
    <+> let* new_expr = shrink_expr shblock expr in
        return (Expr_chan_receive new_expr)
  | Expr_call call ->
    return (Expr_ident "a")
    <+> let* new_call = shrink_func_call (shrink_expr shblock) call in
        return (Expr_call new_call)
;;

let shrink_id_with_expr shblock id_and_expr =
  let id, expr = id_and_expr in
  return ("a", Expr_ident "a")
  <+> (let* new_id = shrink_ident id in
       return (new_id, expr))
  <+> let* new_expr = shrink_expr shblock expr in
      return (id, new_expr)
;;

let shrink_type_option = function
  | Some t -> return None <+> (shrink_type t >|= Option.some)
  | None -> empty
;;

let shrink_long_decl shblock = function
  | Long_decl_no_init (type', first, hd :: tl) ->
    return (Long_decl_no_init (Type_int, "a", []))
    <+> (let* new_type = shrink_type type' in
         return (Long_decl_no_init (new_type, first, hd :: tl)))
    <+>
    let* new_first, new_rest =
      let* new_idents = list ~shrink:shrink_ident (first :: hd :: tl) in
      match new_idents with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    return (Long_decl_no_init (type', new_first, new_rest))
  | Long_decl_no_init (type', first, []) ->
    return (Long_decl_no_init (Type_int, "a", []))
    <+> (let* new_type = shrink_type type' in
         return (Long_decl_no_init (new_type, first, [])))
    <+>
    let* new_id = shrink_ident first in
    return (Long_decl_no_init (type', new_id, []))
  | Long_decl_mult_init (type', first, hd :: tl) ->
    return (Long_decl_no_init (Type_int, "a", []))
    <+> (let* new_type = shrink_type_option type' in
         return (Long_decl_mult_init (new_type, first, hd :: tl)))
    <+>
    let* new_first, new_rest =
      let* new_assigns = list ~shrink:(shrink_id_with_expr shblock) (first :: hd :: tl) in
      match new_assigns with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    return (Long_decl_mult_init (type', new_first, new_rest))
  | Long_decl_mult_init (type', first, []) ->
    return (Long_decl_no_init (Type_int, "a", []))
    <+> (let* new_type = shrink_type_option type' in
         return (Long_decl_mult_init (new_type, first, [])))
    <+>
    let* new_assign = shrink_id_with_expr shblock first in
    return (Long_decl_mult_init (type', new_assign, []))
  | Long_decl_one_init (type', first, second, hd :: tl, call) ->
    return (Long_decl_no_init (Type_int, "a", []))
    <+> of_list
          [ Long_decl_mult_init (type', (first, Expr_call call), [])
          ; Long_decl_mult_init (type', (second, Expr_call call), [])
          ]
    <+> (let* new_type = shrink_type_option type' in
         return (Long_decl_one_init (new_type, first, second, hd :: tl, call)))
    <+> (let* new_call = shrink_func_call (shrink_expr shblock) call in
         return (Long_decl_one_init (type', first, second, hd :: tl, new_call)))
    <+>
    let* new_first, new_second, new_rest =
      let* new_idents = list ~shrink:shrink_ident (first :: second :: hd :: tl) in
      match new_idents with
      | fst :: snd :: tl -> return (fst, snd, tl)
      | _ :: [] | [] -> return (first, second, [])
    in
    return (Long_decl_one_init (type', new_first, new_second, new_rest, call))
  | Long_decl_one_init (type', first, second, [], call) ->
    return (Long_decl_no_init (Type_int, "a", []))
    <+> of_list
          [ Long_decl_mult_init (type', (first, Expr_call call), [])
          ; Long_decl_mult_init (type', (second, Expr_call call), [])
          ]
    <+> (let* new_type = shrink_type_option type' in
         return (Long_decl_one_init (new_type, first, second, [], call)))
    <+> let* new_call = shrink_func_call (shrink_expr shblock) call in
        return (Long_decl_one_init (type', first, second, [], new_call))
;;

let shrink_short_decl shblock = function
  | Short_decl_mult_init (first, hd :: tl) ->
    return (Short_decl_mult_init (("a", Expr_ident "a"), []))
    <+>
    let* new_first, new_rest =
      let* new_assigns = list ~shrink:(shrink_id_with_expr shblock) (first :: hd :: tl) in
      match new_assigns with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    return (Short_decl_mult_init (new_first, new_rest))
  | Short_decl_mult_init (first, []) ->
    return (Short_decl_mult_init (("a", Expr_ident "a"), []))
    <+>
    let* new_pair = shrink_id_with_expr shblock first in
    return (Short_decl_mult_init (new_pair, []))
  | Short_decl_one_init (first, second, hd :: tl, call) ->
    return (Short_decl_mult_init (("a", Expr_ident "a"), []))
    <+> (let* new_call = shrink_func_call (shrink_expr shblock) call in
         return (Short_decl_one_init (first, second, hd :: tl, new_call)))
    <+> of_list
          [ Short_decl_mult_init ((first, Expr_call call), [])
          ; Short_decl_mult_init ((second, Expr_call call), [])
          ]
    <+>
    let* new_first, new_second, new_rest =
      let* new_idents = list ~shrink:shrink_ident (first :: second :: hd :: tl) in
      match new_idents with
      | fst :: snd :: tl -> return (fst, snd, tl)
      | _ :: [] | [] -> return (first, second, [])
    in
    return (Short_decl_one_init (new_first, new_second, new_rest, call))
  | Short_decl_one_init (first, second, [], call) ->
    return (Short_decl_mult_init (("a", Expr_ident "a"), []))
    <+> of_list
          [ Short_decl_mult_init ((first, Expr_call call), [])
          ; Short_decl_mult_init ((second, Expr_call call), [])
          ]
    <+> let* new_call = shrink_func_call (shrink_expr shblock) call in
        return (Short_decl_one_init (first, second, [], new_call))
;;

let rec shrink_lvalue shblcok = function
  | Lvalue_ident id -> shrink_ident id >|= fun id -> Lvalue_ident id
  | Lvalue_array_index (array, index) ->
    return (Lvalue_ident "a")
    <+> return array
    <+> (let* new_array = shrink_lvalue shblcok array in
         return (Lvalue_array_index (new_array, index)))
    <+>
    let* new_index = shrink_expr shblcok index in
    return (Lvalue_array_index (array, new_index))
;;

let shrink_lvalue_with_expr shblock pair =
  let lvalue, expr = pair in
  return (Lvalue_ident "a", Expr_ident "a")
  <+> (let* new_lvalue = shrink_lvalue shblock lvalue in
       return (new_lvalue, expr))
  <+>
  let* new_expr = shrink_expr shblock expr in
  return (lvalue, new_expr)
;;

let shrink_assign shblock = function
  | Assign_mult_expr (first, hd :: tl) ->
    return (Assign_mult_expr ((Lvalue_ident "a", Expr_ident "a"), []))
    <+>
    let* new_first, new_rest =
      let* new_assigns =
        list ~shrink:(shrink_lvalue_with_expr shblock) (first :: hd :: tl)
      in
      match new_assigns with
      | hd :: tl -> return (hd, tl)
      | [] -> of_list [ first, []; hd, [] ]
    in
    return (Assign_mult_expr (new_first, new_rest))
  | Assign_mult_expr (first, []) ->
    return (Assign_mult_expr ((Lvalue_ident "a", Expr_ident "a"), []))
    <+> let* new_pair = shrink_lvalue_with_expr shblock first in
        return (Assign_mult_expr (new_pair, []))
  | Assign_one_expr (first, second, hd :: tl, call) ->
    return (Assign_mult_expr ((Lvalue_ident "a", Expr_ident "a"), []))
    <+> return (Assign_mult_expr ((first, Expr_call call), []))
    <+> return (Assign_mult_expr ((second, Expr_call call), []))
    <+> (let* new_call = shrink_func_call (shrink_expr shblock) call in
         return (Assign_one_expr (first, second, hd :: tl, new_call)))
    <+>
    let* new_first, new_second, new_rest =
      let* new_lvalues =
        list ~shrink:(shrink_lvalue shblock) (first :: second :: hd :: tl)
      in
      match new_lvalues with
      | fst :: snd :: tl -> return (fst, snd, tl)
      | _ :: [] | [] -> return (first, second, [])
    in
    return (Assign_one_expr (new_first, new_second, new_rest, call))
  | Assign_one_expr (first, second, [], call) ->
    return (Assign_mult_expr ((Lvalue_ident "a", Expr_ident "a"), []))
    <+> return (Assign_mult_expr ((first, Expr_call call), []))
    <+> return (Assign_mult_expr ((second, Expr_call call), []))
    <+> let* new_call = shrink_func_call (shrink_expr shblock) call in
        return (Assign_one_expr (first, second, [], new_call))
;;

let shrink_chan_send shblock send =
  let chan, expr = send in
  return ("a", Expr_ident "a")
  <+> (let* new_chan = shrink_ident chan in
       return (new_chan, expr))
  <+>
  let* new_expr = shrink_expr shblock expr in
  return (chan, new_expr)
;;

let shrink_if_for_init shblock = function
  | Init_decl decl ->
    return (Init_incr "a")
    <+> let* new_decl = shrink_short_decl shblock decl in
        return (Init_decl new_decl)
  | Init_assign ass ->
    return (Init_incr "a")
    <+> let* new_ass = shrink_assign shblock ass in
        return (Init_assign new_ass)
  | Init_call call ->
    return (Init_incr "a")
    <+> let* new_call = shrink_func_call (shrink_expr shblock) call in
        return (Init_call new_call)
  | Init_decr id ->
    return (Init_incr "a")
    <+> let* new_id = shrink_ident id in
        return (Init_decr new_id)
  | Init_incr id ->
    return (Init_incr "a")
    <+> let* new_id = shrink_ident id in
        return (Init_incr new_id)
  | Init_send send ->
    return (Init_incr "a")
    <+> let* new_send = shrink_chan_send shblock send in
        return (Init_send new_send)
  | Init_receive chan ->
    return (Init_incr "a")
    <+> let* new_chan = shrink_expr shblock chan in
        return (Init_receive new_chan)
;;

let rec shrink_if shblock if' =
  let { init; cond; if_body; else_body } = if' in
  return { init = None; cond = Expr_ident "a"; if_body = []; else_body = None }
  <+> (let* new_init =
         return None
         <+>
         match init with
         | Some init -> return None <+> (shrink_if_for_init shblock init >|= Option.some)
         | None -> empty
       in
       return { init = new_init; cond; if_body; else_body })
  <+> (let* new_cond = shrink_expr shblock cond in
       return { init; cond = new_cond; if_body; else_body })
  <+> (let* new_if_body = shblock if_body in
       return { init; cond; if_body = new_if_body; else_body })
  <+> let* new_else_body =
        return None
        <+>
        match else_body with
        | Some (Else_block block) ->
          return None <+> (shblock block >|= fun block -> Some (Else_block block))
        | Some (Else_if if') ->
          return None <+> (shrink_if shblock if' >|= fun if' -> Some (Else_if if'))
        | None -> empty
      in
      return { init; cond; if_body; else_body = new_else_body }
;;

let shrink_stmt shblock = function
  | Stmt_break -> empty
  | Stmt_continue -> empty
  | Stmt_incr id -> return Stmt_break <+> (shrink_ident id >|= fun id -> Stmt_incr id)
  | Stmt_decr id -> return Stmt_break <+> (shrink_ident id >|= fun id -> Stmt_decr id)
  | Stmt_long_var_decl decl ->
    return Stmt_break
    <+> (shrink_long_decl shblock decl >|= fun decl -> Stmt_long_var_decl decl)
  | Stmt_short_var_decl decl ->
    return Stmt_break
    <+> (shrink_short_decl shblock decl >|= fun decl -> Stmt_short_var_decl decl)
  | Stmt_assign ass ->
    return Stmt_break <+> (shrink_assign shblock ass >|= fun ass -> Stmt_assign ass)
  | Stmt_call call ->
    return Stmt_break
    <+> (shrink_func_call (shrink_expr shblock) call >|= fun call -> Stmt_call call)
  | Stmt_go call ->
    return Stmt_break
    <+> (shrink_func_call (shrink_expr shblock) call >|= fun call -> Stmt_go call)
  | Stmt_defer call ->
    return Stmt_break
    <+> (shrink_func_call (shrink_expr shblock) call >|= fun call -> Stmt_defer call)
  | Stmt_chan_send send ->
    return Stmt_break
    <+> (shrink_chan_send shblock send >|= fun send -> Stmt_chan_send send)
  | Stmt_chan_receive chan ->
    return Stmt_break <+> (shrink_expr shblock chan >|= fun chan -> Stmt_chan_receive chan)
  | Stmt_return exprs ->
    return Stmt_break
    <+> (list ~shrink:(shrink_expr shblock) exprs >|= fun exprs -> Stmt_return exprs)
  | Stmt_if if' -> return Stmt_break <+> (shrink_if shblock if' >|= fun if' -> Stmt_if if')
  | Stmt_for { init; cond; post; body } ->
    return Stmt_break
    <+> (let* new_init =
           return None
           <+>
           match init with
           | Some init -> return None <+> (shrink_if_for_init shblock init >|= Option.some)
           | None -> empty
         in
         return (Stmt_for { init = new_init; cond; post; body }))
    <+> (let* new_cond =
           return None
           <+>
           match cond with
           | Some cond -> return None <+> (shrink_expr shblock cond >|= Option.some)
           | None -> empty
         in
         return (Stmt_for { init; cond = new_cond; post; body }))
    <+> (let* new_post =
           return None
           <+>
           match post with
           | Some post -> return None <+> (shrink_if_for_init shblock post >|= Option.some)
           | None -> empty
         in
         return (Stmt_for { init; cond; post = new_post; body }))
    <+>
    let* new_body = shblock body in
    return (Stmt_for { init; cond; post; body = new_body })
  | Stmt_block block ->
    return Stmt_break <+> (shblock block >|= fun block -> Stmt_block block)
;;

let rec shrink_block block = list ~shrink:(shrink_stmt shrink_block) block

let shrink_func_decl decl =
  let ident, args_returns_and_body = decl in
  return ("a", { args = []; returns = None; body = [] })
  <+> (let* new_ident = shrink_ident ident in
       return (new_ident, args_returns_and_body))
  <+> let* new_args_returns_and_body =
        shrink_anon_func shrink_block args_returns_and_body
      in
      return (ident, new_args_returns_and_body)
;;

let shrink_top_decl = function
  | Decl_func decl ->
    return (Decl_var (Long_decl_no_init (Type_int, "a", [])))
    <+> (shrink_func_decl decl >|= fun decl -> Decl_func decl)
  | Decl_var decl ->
    return (Decl_var (Long_decl_no_init (Type_int, "a", [])))
    <+> (shrink_long_decl shrink_block decl >|= fun decl -> Decl_var decl)
;;

let shrink_file file = list ~shrink:shrink_top_decl file
