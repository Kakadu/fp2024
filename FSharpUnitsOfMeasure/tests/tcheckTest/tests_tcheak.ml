(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Typecheck

let rec type_string t =
  match t with
  | TIdent i -> i
  | TFun (a, b) -> type_string a ^ " -> " ^ type_string b
  | TAny n -> "T " ^ string_of_int n
  | TList t -> type_string t ^ " list"
  | TTuple (x, y, ls) ->
    type_string x
    ^ " * "
    ^ type_string y
    ^ String.concat ~sep:" * " (List.map ls ~f:type_string)
;;

let pp t = print_endline (type_string t)

let pp_res res =
  print_endline
    (type_string (fst res)
     ^ "\n"
     ^ String.concat
         (List.map (snd res) ~f:(fun x -> "\n" ^ fst x ^ ": " ^ type_string (snd x))))
;;

let new_empty =
  [ "+", TFun (TIdent "int", TFun (TIdent "int", TIdent "int"))
  ; "*", TFun (TIdent "int", TFun (TIdent "int", TIdent "int"))
  ; ">=", TFun (TIdent "int", TFun (TIdent "int", TIdent "bool"))
  ; "a", TIdent "int"
  ; "b", TIdent "int"
  ; "c", TIdent "int"
  ]
;;

let%expect_test "typecheak int as const int" =
  let res = typeof new_empty (Expr_const (Const_int 1)) in
  pp (fst res);
  [%expect {| int |}]
;;

let%expect_test "typecheak int var as int" =
  let res = typeof new_empty (Expr_ident_or_op "a") in
  pp (fst res);
  [%expect {| int |}]
;;

let%expect_test "typecheak int var as int" =
  let res = typeof new_empty (Expr_ident_or_op "a") in
  pp (fst res);
  [%expect {| int |}]
;;

let%expect_test "typecheak int op as int" =
  let res = typeof new_empty (Expr_apply (Expr_ident_or_op "+", Expr_ident_or_op "a")) in
  pp (fst res);
  [%expect {| int -> int |}]
;;

let%expect_test "typecheak int op as int" =
  let res = typeof new_empty (Expr_typed (Expr_ident_or_op "a", Type_ident "int")) in
  pp (fst res);
  [%expect {| int |}]
;;

let%expect_test "typecheak ite" =
  let res =
    typeof
      new_empty
      (Expr_ifthenelse
         (Expr_ident_or_op "a", Expr_ident_or_op "b", Some (Expr_ident_or_op "c")))
  in
  pp (fst res);
  [%expect {| int |}]
;;

let%expect_test "typecheak int op as int with unified types" =
  let res =
    typeof
      new_empty
      (Expr_ifthenelse
         ( Expr_ident_or_op "a"
         , Expr_ident_or_op "b"
         , Some
             (Expr_let
                ( Nonrecursive
                , Bind (Pattern_ident_or_op "d", Expr_const (Const_int 1))
                , []
                , Expr_ident_or_op "d" )) ))
  in
  pp (fst res);
  [%expect {| int |}]
;;

let%expect_test "typecheak int op as int with unified types" =
  let res =
    typeof new_empty (Expr_tuple (Expr_ident_or_op "a", Expr_ident_or_op "b", []))
  in
  pp (fst res);
  [%expect {| int * int |}]
;;

let%expect_test "typecheak lam" =
  let res =
    typeof
      new_empty
      (Expr_lam
         ( Pattern_ident_or_op "x"
         , Expr_apply
             ( Expr_apply (Expr_ident_or_op "+", Expr_ident_or_op "a")
             , Expr_ident_or_op "x" ) ))
  in
  pp (fst res);
  [%expect {| int -> int |}]
;;

let%expect_test "typecheak list" =
  let res = typeof new_empty (Expr_list [ Expr_ident_or_op "a"; Expr_ident_or_op "b" ]) in
  pp (fst res);
  [%expect {| int list |}]
;;
