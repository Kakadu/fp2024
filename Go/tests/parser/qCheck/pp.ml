(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let print_bin_op fmt = function
  | Bin_sum -> fprintf fmt " + "
  | Bin_multiply -> fprintf fmt " * "
  | Bin_subtract -> fprintf fmt " - "
  | Bin_divide -> fprintf fmt " / "
  | Bin_modulus -> fprintf fmt " %% "
  | Bin_equal -> fprintf fmt " == "
  | Bin_not_equal -> fprintf fmt " != "
  | Bin_greater -> fprintf fmt " > "
  | Bin_greater_equal -> fprintf fmt " >= "
  | Bin_less -> fprintf fmt " < "
  | Bin_less_equal -> fprintf fmt " <= "
  | Bin_and -> fprintf fmt " && "
  | Bin_or -> fprintf fmt " || "
;;

let rec print_type fmt type' =
  match type' with
  | Type_int -> fprintf fmt " int "
  | Type_string -> fprintf fmt " string "
  | Type_bool -> fprintf fmt " bool "
  | Type_array (int, type') ->
    fprintf fmt "[%i]" int;
    print_type fmt type'
  | Type_func (lst1, lst2) ->
    fprintf fmt "func(";
    List.iter (print_type fmt) lst1;
    fprintf fmt ") (";
    List.iter (print_type fmt) lst2;
    fprintf fmt ") "
  | Type_chan chan_type ->
    (match chan_type with
     | Chan_bidirectional t ->
       fprintf fmt "chan ";
       print_type fmt t
     | Chan_receive t ->
       fprintf fmt "<- chan ";
       print_type fmt t
     | Chan_send t ->
       fprintf fmt "chan<- ";
       print_type fmt t)
;;

let print_pair_type fmt t =
  let t1, t2 = List.split t in
  List.iter2
    (fun a b ->
      fprintf fmt "%s" a;
      print_type fmt b)
    t1
    t2
;;

let print_un_op fmt = function
  | Unary_not -> fprintf fmt " !"
  | Unary_plus -> fprintf fmt " +"
  | Unary_minus -> fprintf fmt " -"
  | Unary_recieve -> fprintf fmt " <-"
;;

let print_return_values fmt = function
  | Only_types t ->
    fprintf fmt "( ";
    List.iter (print_type fmt) t;
    fprintf fmt " )"
  | Ident_and_types t ->
    fprintf fmt "( ";
    print_pair_type fmt t;
    fprintf fmt " )"
;;

let print_func_call fmt t f =
  let e1, el = t in
  f e1;
  fprintf fmt "( ";
  List.iter f el;
  fprintf fmt " )"
;;

let rec print_expr fmt expr =
  match expr with
  | Expr_const (Const_int num) -> fprintf fmt "%i" num
  | Expr_const (Const_string str) -> fprintf fmt "%s" str
  | Expr_const (Const_array (intt, type', expr_lst)) ->
    fprintf fmt "[%i]" intt;
    print_type fmt type';
    fprintf fmt "{";
    List.iter (print_expr fmt) expr_lst;
    fprintf fmt "}"
  | Expr_const (Const_func afunc) ->
    fprintf fmt "func";
    print_pair_type fmt afunc.args;
    (match afunc.returns with
     | Some x ->
       fprintf fmt "( ";
       print_return_values fmt x;
       fprintf fmt "( " (*print block*)
     | None -> fprintf fmt " ")
  | Expr_ident idnt -> fprintf fmt "%s" idnt
  | Expr_index (exp1, exp2) ->
    print_expr fmt exp1;
    fprintf fmt "[ ";
    print_expr fmt exp2;
    fprintf fmt "[ "
  | Expr_bin_oper (t, arg1, arg2) ->
    print_expr fmt arg1;
    print_bin_op fmt t;
    print_expr fmt arg2
  | Expr_un_oper (t, arg1) ->
    print_un_op fmt t;
    print_expr fmt arg1
  | Expr_call fc -> print_func_call fmt fc (print_expr fmt)
;;

let print_pair_expr fmt t f1 div =
  let t1, t2 = List.split t in
  List.iter (fun a -> f1 a) t1;
  fprintf fmt " %s " div;
  List.iter (fun b -> print_expr fmt b) t2
;;

let rec print_lvalue fmt lv =
  match lv with
  | Lvalue_ident s -> fprintf fmt "%s" s
  | Lvalue_array_index (lv, ex) ->
    print_expr fmt ex;
    print_lvalue fmt lv
;;

let print_block a f = List.iter f a

let rec print_stmt fmt stmt =
  match stmt with
  | Stmt_long_var_decl lvd ->
    (match lvd with
     | Long_decl_mult_init (t, li) ->
       fprintf fmt "var ";
       print_pair_expr fmt li (fprintf fmt "%s") "=";
       (match t with
        | Some x -> print_type fmt x
        | None -> fprintf fmt " ")
     | Long_decl_no_init (t, il) ->
       fprintf fmt "var ";
       List.iter (fprintf fmt "%s") il;
       print_type fmt t
     | Long_decl_one_init (t, il, ex) ->
       fprintf fmt "var ";
       List.iter (fprintf fmt "%s") il;
       (match t with
        | Some x -> print_type fmt x
        | None -> fprintf fmt " ");
       fprintf fmt " = ";
       print_expr fmt ex)
  | Stmt_short_var_decl vd ->
    (match vd with
     | Short_decl_mult_init ls -> print_pair_expr fmt ls (fprintf fmt "%s") " := "
     | Short_decl_one_init (il, ex) ->
       List.iter (fprintf fmt "%s") il;
       fprintf fmt " = ";
       print_expr fmt ex)
  | Stmt_assign ad ->
    (match ad with
     | Assign_mult_expr ls -> print_pair_expr fmt ls (print_lvalue fmt) " = "
     | Assign_one_expr (ls, ex) ->
       List.iter (print_lvalue fmt) ls;
       fprintf fmt " = ";
       print_expr fmt ex)
  | Stmt_incr id -> fprintf fmt "%s++" id
  | Stmt_decr id -> fprintf fmt "%s--" id
  | Stmt_if usl ->
    fprintf fmt "if ";
    (match usl.init with
     | Some x ->
       print_stmt fmt x;
       fprintf fmt "; "
     | None -> fprintf fmt " ");
    print_expr fmt usl.cond;
    fprintf fmt "{ ";
    print_block usl.if_body (print_stmt fmt);
    fprintf fmt " }";
    (match usl.else_body with
     | Some x ->
       fprintf fmt "else { ";
       print_stmt fmt x;
       fprintf fmt " }"
     | None -> fprintf fmt " ")
  | Stmt_for usl ->
    fprintf fmt "for ";
    (match usl.init with
     | Some x ->
       print_stmt fmt x;
       fprintf fmt "; "
     | None -> fprintf fmt " ");
    (match usl.cond with
     | Some x ->
       print_expr fmt x;
       fprintf fmt "; "
     | None -> fprintf fmt " ");
    (match usl.post with
     | Some x ->
       print_stmt fmt x;
       fprintf fmt "; "
     | None -> fprintf fmt " ");
    fprintf fmt "{ ";
    print_block usl.body (print_stmt fmt);
    fprintf fmt " }"
  | Stmt_chan_send (idnt, ex) ->
    fprintf fmt "%s " idnt;
    fprintf fmt " <- ";
    print_expr fmt ex
  | Stmt_range _ -> fprintf fmt " "
  | Stmt_break -> fprintf fmt " break "
  | Stmt_continue -> fprintf fmt " continue "
  | Stmt_return exl ->
    fprintf fmt " return ";
    List.iter (print_expr fmt) exl
  | Stmt_block bl -> print_block bl (print_stmt fmt)
  | Stmt_call cl -> print_func_call fmt cl (print_expr fmt)
  | Stmt_defer cl ->
    fprintf fmt "defer ";
    print_func_call fmt cl (print_expr fmt)
  | Stmt_go cl ->
    fprintf fmt "go ";
    print_func_call fmt cl (print_expr fmt)
;;

let print_func_decl fmt dlc =
  let idnt, ls = dlc in
  fprintf fmt "func %s" idnt;
  print_expr fmt (Expr_const (Const_func ls))
;;

let print_top_decl fmt dcl =
  match dcl with
  | Decl_var lvd -> print_stmt fmt (Stmt_long_var_decl lvd)
  | Decl_func fdl -> print_func_decl fmt fdl
;;

let print_file fmt dcl = List.iter (print_top_decl fmt) dcl

(*
   let%expect_test "arithmetic" =
   print_expr
   (Expr_bin_oper
   ( Bin_sum
   , Expr_bin_oper
   ( Bin_multiply
   , Expr_un_oper (Unary_minus, Expr_const (Const_int 5))
   , Expr_ident "_r" )
   , Expr_const (Const_int 8) ));
   [%expect {| -5 * _r + 8 |}]
   ;;

   let%expect_test "simple_call" =
   print_expr
   (Expr_call
   ( Expr_ident "fac"
   , [ Expr_bin_oper
             ( Bin_sum
             , Expr_const (Const_int 4)
             , Expr_call
                 ( Expr_ident "fac"
                 , [ Expr_bin_oper
                       (Bin_sum, Expr_const (Const_int 4), Expr_const (Const_int 4))
                   ] ) )
         ] ));
   [%expect {| fac( 4 + fac( 4 + 4 ) ) |}]
   ;;
*)
