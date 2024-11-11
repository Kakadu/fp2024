(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Printf

let print_bin_op = function
  | Bin_sum -> printf " + "
  | Bin_multiply -> printf " * "
  | Bin_subtract -> printf " - "
  | Bin_divide -> printf " / "
  | Bin_modulus -> printf " %% "
  | Bin_equal -> printf " == "
  | Bin_not_equal -> printf " != "
  | Bin_greater -> printf " > "
  | Bin_greater_equal -> printf " >= "
  | Bin_less -> printf " < "
  | Bin_less_equal -> printf " <= "
  | Bin_and -> printf " && "
  | Bin_or -> printf " || "
;;

let rec print_type type' =
  match type' with
  | Type_int -> printf " int "
  | Type_string -> printf " string "
  | Type_bool -> printf " bool "
  | Type_array (int, type') ->
    printf "[%i]" int;
    print_type type'
  | Type_func (lst1, lst2) ->
    printf "func(";
    List.iter print_type lst1;
    printf ") (";
    List.iter print_type lst2;
    printf ") "
  | Type_chan chan_type ->
    (match chan_type with
     | Chan_bidirectional t ->
       printf "chan ";
       print_type t
     | Chan_receive t ->
       printf "<- chan ";
       print_type t
     | Chan_send t ->
       printf "chan<- ";
       print_type t)
;;

let print_pair_type t =
  let t1, t2 = List.split t in
  List.iter2
    (fun a b ->
      printf "%s" a;
      print_type b)
    t1
    t2
;;

let print_un_op = function
  | Unary_not -> printf " !"
  | Unary_plus -> printf " +"
  | Unary_minus -> printf " -"
  | Unary_recieve -> printf " <-"
;;

let print_return_values = function
  | Only_types t ->
    printf "( ";
    List.iter print_type t;
    printf " )"
  | Ident_and_types t ->
    printf "( ";
    print_pair_type t;
    printf " )"
;;

let print_func_call t f =
  let e1, el = t in
  f e1;
  printf "( ";
  List.iter f el;
  printf " )"
;;

let rec print_expr expr =
  match expr with
  | Expr_const (Const_int num) -> printf "%i" num
  | Expr_const (Const_string str) -> printf "%s" str
  | Expr_const (Const_array (intt, type', expr_lst)) ->
    printf "[%i]" intt;
    print_type type';
    printf "{";
    List.iter print_expr expr_lst;
    printf "}"
  | Expr_const (Const_func afunc) ->
    printf "func";
    print_pair_type afunc.args;
    (match afunc.returns with
     | Some x ->
       printf "( ";
       print_return_values x;
       printf "( " (*print block*)
     | None -> printf " ")
  | Expr_ident idnt -> printf "%s" idnt
  | Expr_index (exp1, exp2) ->
    print_expr exp1;
    printf "[ ";
    print_expr exp2;
    printf "[ "
  | Expr_bin_oper (t, arg1, arg2) ->
    print_expr arg1;
    print_bin_op t;
    print_expr arg2
  | Expr_un_oper (t, arg1) ->
    print_un_op t;
    print_expr arg1
  | Expr_call fc -> print_func_call fc print_expr
;;

let print_pair_expr t f1 div =
  let t1, t2 = List.split t in
  List.iter (fun a -> f1 a) t1;
  printf " %s " div;
  List.iter (fun b -> print_expr b) t2
;;

let rec print_lvalue lv =
  match lv with
  | Lvalue_ident s -> printf "%s" s
  | Lvalue_array_index (lv, ex) ->
    print_expr ex;
    print_lvalue lv
;;

let rec print_block a f = List.iter f a

let rec print_stmt stmt =
  match stmt with
  | Stmt_long_var_decl lvd ->
    (match lvd with
     | Long_decl_mult_init (t, li) ->
       printf "var ";
       print_pair_expr li (printf "%s") "=";
       (match t with
        | Some x -> print_type x
        | None -> printf " ")
     | Long_decl_no_init (t, il) ->
       printf "var ";
       List.iter (printf "%s") il;
       print_type t
     | Long_decl_one_init (t, il, ex) ->
       printf "var ";
       List.iter (printf "%s") il;
       (match t with
        | Some x -> print_type x
        | None -> printf " ");
       printf " = ";
       print_expr ex)
  | Stmt_short_var_decl vd ->
    (match vd with
     | Short_decl_mult_init ls -> print_pair_expr ls (printf "%s") " := "
     | Short_decl_one_init (il, ex) ->
       List.iter (printf "%s") il;
       printf " = ";
       print_expr ex)
  | Stmt_assign ad ->
    (match ad with
     | Assign_mult_expr ls -> print_pair_expr ls print_lvalue " = "
     | Assign_one_expr (ls, ex) ->
       List.iter print_lvalue ls;
       printf " = ";
       print_expr ex)
  | Stmt_incr id -> printf "%s++" id
  | Stmt_decr id -> printf "%s--" id
  | Stmt_if usl ->
    printf "if ";
    (match usl.init with
     | Some x ->
       print_stmt x;
       printf "; "
     | None -> printf " ");
    print_expr usl.cond;
    printf "{ ";
    print_block usl.if_body print_stmt;
    printf " }";
    (match usl.else_body with
     | Some x ->
       printf "else { ";
       print_stmt x;
       printf " }"
     | None -> printf " ")
  | Stmt_for usl ->
    printf "for ";
    (match usl.init with
     | Some x ->
       print_stmt x;
       printf "; "
     | None -> printf " ");
    (match usl.cond with
     | Some x ->
       print_expr x;
       printf "; "
     | None -> printf " ");
    (match usl.post with
     | Some x ->
       print_stmt x;
       printf "; "
     | None -> printf " ");
    printf "{ ";
    print_block usl.body print_stmt;
    printf " }"
  | Stmt_chan_send (idnt, ex) ->
    printf "%s " idnt;
    printf " <- ";
    print_expr ex
  | Stmt_range rng -> printf " "
  | Stmt_break -> printf " break "
  | Stmt_continue -> printf " continue "
  | Stmt_return exl ->
    printf " return ";
    List.iter print_expr exl
  | Stmt_block bl -> print_block bl print_stmt
  | Stmt_call cl -> print_func_call cl print_expr
  | Stmt_defer cl ->
    printf "defer ";
    print_func_call cl print_expr
  | Stmt_go cl ->
    printf "go ";
    print_func_call cl print_expr
;;

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
