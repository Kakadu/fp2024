(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open TypeCheckMonad
open TypeCheckMonad.CheckMonad
open TypeCheckErrors
open Ast

let retrieve_paris_first args = List.map (fun (x, _) -> x) args
let retrieve_paris_second args = List.map (fun (_, x) -> x) args

let find_func name code =
  let func_name = function
    | Decl_func (func_name, _) -> func_name
    | Decl_var _ -> ""
  in
  Stdlib.List.find_opt (fun func -> String.equal (func_name func) name) code
;;

let retrieve_anon_func x =
  let args = retrieve_paris_second x.args in
  match x.returns with
  | Some (Only_types (x, y)) -> Ctype (Type_func (args, x :: y))
  | Some (Ident_and_types (x, y)) ->
    Ctype (Type_func (args, retrieve_paris_second (x :: y)))
  | None -> Ctype (Type_func (args, []))
;;

let check_anon_func x f =
  let args = retrieve_paris_second x.args in
  let save_args = iter (fun (x, y) -> save_local_ident_l x (Ctype y)) x.args in
  write_env
  *> save_args
  *> iter (fun x -> f x) x.body
  *> delete_env
  *>
  match x.returns with
  | Some (Only_types (x, y)) -> return (Ctype (Type_func (args, x :: y)))
  | Some (Ident_and_types (x, y)) ->
    return (Ctype (Type_func (args, retrieve_paris_second (x :: y))))
  | None -> return (Ctype (Type_func (args, [])))
;;

let retrieve_type_const check_afc = function
  | Const_array (x, y, _) -> return (Ctype (Type_array (x, y)))
  | Const_int _ -> return (Ctype Type_int)
  | Const_string _ -> return (Ctype Type_string)
  | Const_func x -> check_anon_func x check_afc
;;

let check_main code =
  match find_func "main" code with
  | Some (Decl_func (_, { args = []; returns = None; body = _ })) -> return ()
  | Some (Decl_func _) ->
    fail
      (TypeCheckError
         (Incorrect_main
            (Printf.sprintf "func main must have no arguments and no return values")))
  | _ -> fail (TypeCheckError (Incorrect_main (Printf.sprintf "main func not found")))
;;

let retrieve_ident ident = seek_ident ident *> read_ident ident

let eq e el1 el2 =
  match e el1 el2 with
  | true -> return el1
  | false -> fail (TypeCheckError (Mismatched_types "Types mismatched in binoper"))
;;

let eq_type t1 t2 = eq equal_ctype t1 t2

let check_eq t1 t2 =
  match equal_ctype t1 t2 with
  | true -> return ()
  | false -> fail (TypeCheckError (Mismatched_types "Types mismatched in binoper"))
;;

let rec retrieve_type_expr caf = function
  | Expr_const x -> retrieve_type_const caf x
  | Expr_un_oper (_, x) -> retrieve_type_expr caf x
  | Expr_ident x -> retrieve_ident x
  | Expr_bin_oper (o, x, y) ->
    let compare_arg_typ type1 type2 =
      retrieve_type_expr caf type1
      >>= fun type1 -> retrieve_type_expr caf type2 >>= fun type2 -> eq_type type1 type2
    in
    let compare_operation_typ type1 type2 t =
      compare_arg_typ type1 type2 >>= fun e -> eq_type e t
    in
    (match o with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ x y (Ctype Type_int) *> return (Ctype Type_int)
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ x y (Ctype Type_int) *> return (Ctype Type_bool)
     | Bin_or | Bin_and ->
       compare_operation_typ x y (Ctype Type_bool) *> return (Ctype Type_bool)
     | Bin_equal | Bin_not_equal -> compare_arg_typ x y *> return (Ctype Type_bool))
  | Expr_call (x, y) ->
    map (retrieve_type_expr caf) y *> (retrieve_type_expr caf) x
    >>= (function
     | Ctype (Type_func (_, y)) ->
       (match List.length y with
        | 1 -> return (Ctype (List.nth y 0))
        | _ -> return (Ctuple y))
     | _ -> fail (TypeCheckError Check_failed))
  | Expr_chan_receive x -> retrieve_type_expr caf x
  | Expr_index (x, y) ->
    if retrieve_type_expr caf y = return (Ctype Type_int)
    then retrieve_type_expr caf x
    else fail (TypeCheckError (Mismatched_types "Array index type mismatched"))
;;

let retrieve_idents_from_long_var_decl caf env decl =
  let env_r =
    match env with
    | Loc -> save_local_ident_r
    | Glob -> save_global_ident_r
  in
  let env_l =
    match env with
    | Loc -> save_local_ident_l
    | Glob -> save_global_ident_l
  in
  match decl with
  | Long_decl_no_init (k, x, y) -> iter (env_r (Ctype k)) (x :: y)
  | Long_decl_mult_init (k, (x, z), y) ->
    (match k with
     | Some k ->
       iter
         ((fun k (i, x) ->
            (retrieve_type_expr caf x >>= fun x -> check_eq x k) *> env_r k i)
            (Ctype k))
         ((x, z) :: y)
     | None ->
       iter
         (fun (x, z) -> retrieve_type_expr caf z >>= env_l x)
         (List.combine (x :: retrieve_paris_first y) (z :: retrieve_paris_second y)))
  | Long_decl_one_init (k, x, y, z, l) ->
    (match k with
     | Some k ->
       if retrieve_type_expr caf (Expr_call l) != return (Ctype k)
       then
         fail
           (TypeCheckError (Mismatched_types (Printf.sprintf "%s" (print_type (Ctype k)))))
       else iter (env_r (Ctype k)) (x :: y :: z)
     | None ->
       retrieve_type_expr caf (Expr_call l)
       >>= fun x1 ->
       (match x1 with
        | Ctype _ ->
          fail (TypeCheckError (Mismatched_types "multiple return types mismatched"))
        | Ctuple tl ->
          if List.length tl == List.length (x :: y :: z)
          then iter2 (fun x y -> env_l x y) (x :: y :: z) (List.map (fun x -> Ctype x) tl)
          else fail (TypeCheckError (Mismatched_types "multiple return types mismatched")))
       *> return ())
;;

(*iter
  (fun (x, z) -> retrieve_type_expr z >>= env_l x)
  (List.combine (x :: y :: z) (List.map (fun _ -> Expr_call l) (x :: y :: z))))*)

let retrieve_idents_from_short_var_decl caf = function
  | Short_decl_mult_init ((x, z), y) ->
    iter
      (fun (x, z) -> retrieve_type_expr caf z >>= save_local_ident_l x)
      (List.combine (x :: retrieve_paris_first y) (z :: retrieve_paris_second y))
  | Short_decl_one_init (x, y, z, l) ->
    iter
      (fun (x, z) -> retrieve_type_expr caf z >>= save_local_ident_l x)
      (List.combine (x :: y :: z) (List.map (fun _ -> Expr_call l) (x :: y :: z)))
;;

let check_func_call caf (x, y) =
  retrieve_type_expr caf x *> map (retrieve_type_expr caf) y *> return ()
;;

let rec check_lvalue caf = function
  | Lvalue_ident x -> retrieve_ident x
  | Lvalue_array_index (x, y) -> check_lvalue caf x *> retrieve_type_expr caf y
;;

let check_assign caf = function
  | Assign_mult_expr (x, z) ->
    iter
      (fun (x, y) ->
        check_lvalue caf x
        >>= fun type1 -> retrieve_type_expr caf y >>= fun type2 -> check_eq type1 type2)
      (x :: z)
  | Assign_one_expr (x, y, _, w) ->
    check_lvalue caf x *> check_lvalue caf y *> check_func_call caf w
;;

let check_init caf = function
  | Some x ->
    (match x with
     | Init_assign x -> check_assign caf x
     | Init_call x -> check_func_call caf x
     | Init_decl _ -> return () (*доделать*)
     | Init_decr x -> seek_ident x
     | Init_incr x -> seek_ident x
     | Init_receive x -> retrieve_type_expr caf x *> return ()
     | Init_send (x, y) -> seek_ident x *> retrieve_type_expr caf y *> return ())
  | None -> return ()
;;

let rec check_stmt = function
  | Stmt_long_var_decl x -> retrieve_idents_from_long_var_decl check_stmt Loc x
  | Stmt_short_var_decl x -> retrieve_idents_from_short_var_decl check_stmt x
  | Stmt_incr x -> retrieve_ident x >>= fun e -> check_eq e (Ctype Type_int)
  | Stmt_decr x -> retrieve_ident x >>= fun e -> check_eq e (Ctype Type_int)
  | Stmt_assign x -> check_assign check_stmt x
  | Stmt_call x -> retrieve_type_expr check_stmt (Expr_call x) *> return ()
  | Stmt_defer x -> check_func_call check_stmt x
  | Stmt_go x -> check_func_call check_stmt x
  | Stmt_chan_send (x, y) -> seek_ident x *> retrieve_type_expr check_stmt y *> return ()
  | Stmt_block x -> iter check_stmt x
  | Stmt_break -> return ()
  | Stmt_chan_receive x -> retrieve_type_expr check_stmt x *> return ()
  | Stmt_continue -> return ()
  | Stmt_return x ->
    (get_func_name
     >>= read_ident
     >>= fun x1 ->
     (match x1 with
      | Ctype (Type_func (_, y)) ->
        (match List.length x = List.length y with
         | true -> return (List.combine x (List.map (fun x -> Ctype x) y))
         | false -> fail (TypeCheckError (Mismatched_types "func return types mismatch")))
      | _ -> fail (TypeCheckError Check_failed))
     >>= iter (fun (x, y) ->
       retrieve_type_expr check_stmt x >>= fun type1 -> check_eq y type1))
    *> return ()
  | Stmt_if x ->
    check_init check_stmt x.init
    *> retrieve_type_expr check_stmt x.cond
    *> iter check_stmt x.if_body
    *>
      (match x.else_body with
      | Some x ->
        (match x with
         | Else_block x -> iter check_stmt x
         | Else_if x -> check_stmt (Stmt_if x))
      | None -> return ())
  | Stmt_for x ->
    check_init check_stmt x.init
    *> check_init check_stmt x.post
    *> iter check_stmt x.body
    *>
      (match x.cond with
      | Some x -> retrieve_type_expr check_stmt x *> return ()
      | None -> return ())
;;

let check_top_decl_funcs = function
  | Decl_func (x, y) -> save_global_ident_r (retrieve_anon_func y) x
  | Decl_var _ -> return ()
;;

let check_top_decl = function
  | Decl_func (x, y) -> write_func_name x *> check_anon_func y check_stmt *> return ()
  | Decl_var x -> retrieve_idents_from_long_var_decl check_stmt Glob x
;;

let type_check code =
  run
    (check_main code *> iter check_top_decl_funcs code *> iter check_top_decl code)
    (MapIdent.empty, [ MapIdent.empty ], None)
;;

let pp ast =
  match type_check ast with
  | _, Result.Ok _ -> print_endline "CORRECT"
  | _, Result.Error x ->
    prerr_string "ERROR WHILE TYPECHECK WITH ";
    (match x with
     | TypeCheckError Check_failed -> prerr_endline "Check failed"
     | TypeCheckError (Multiple_declaration x) ->
       prerr_string "Multiple declaration error: ";
       prerr_string x
     | TypeCheckError (Incorrect_main x) ->
       prerr_endline "Incorrect main error:";
       prerr_string x
     | TypeCheckError (Undefined_ident x) ->
       prerr_endline "Undefined ident error:";
       prerr_string x
     | TypeCheckError (Mismatched_types x) ->
       prerr_endline "Mismatched types";
       prerr_string x
     | TypeCheckError (Cannot_assign x) ->
       prerr_endline "Mismatched types";
       prerr_string x)
;;

let%expect_test "multiple func declaration" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: main is redeclared in func() |}]
;;

let%expect_test "multiple declaration in args" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a", Type_int; "a", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a is redeclared in int |}]
;;

let%expect_test "correct long_var_decl" =
  pp
    [ Decl_var (Long_decl_one_init (None, "a", "foo", [ "c" ], (Expr_ident "get", [])))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "get"
        , { args = []
          ; returns = Some (Only_types (Type_int, [ Type_int; Type_int ]))
          ; body = []
          } )
    ];
  [%expect {|
    CORRECT |}]
;;

let%expect_test "incorrect long_var_decl with different lengths" =
  pp
    [ Decl_var (Long_decl_one_init (None, "a", "foo", [ "c" ], (Expr_ident "get", [])))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "get"
        , { args = []
          ; returns = Some (Only_types (Type_int, [ Type_int; Type_int; Type_int ]))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    multiple return types mismatched |}]
;;

let%expect_test "multiple declaration in global space" =
  pp
    [ Decl_var (Long_decl_one_init (None, "a", "foo", [ "c" ], (Expr_ident "foo", [])))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = []
          ; returns = Some (Only_types (Type_int, [ Type_int; Type_int ]))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: foo is redeclared in int |}]
;;

let%expect_test "correct declarations #1" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a", Type_int; "b", Type_int; "c", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "foo1"
        , { args = [ "a", Type_int; "b", Type_int; "c", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect {|
    CORRECT |}]
;;

let%expect_test "incorrect declaration type" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "a"
           , "b"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo1"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "foo2"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    <-chan [5]int |}]
;;

let%expect_test "undefined var inc" =
  pp
    [ Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = [ Stmt_incr "a2" ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    a2 is not defined |}]
;;

let%expect_test "undefined func call" =
  pp
    [ Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "main"
        , { args = []; returns = None; body = [ Stmt_call (Expr_ident "foo2", []) ] } )
    ; Decl_func
        ( "foo"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    foo2 is not defined |}]
;;

let%expect_test "multiple declarations in func body with args" =
  pp
    [ Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "foo"
        , { args = [ "a2", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = [ Stmt_long_var_decl (Long_decl_no_init (Type_int, "a2", [])) ]
          } )
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ("get", { args = []; returns = Some (Only_types (Type_int, [])); body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Multiple declaration error: a2 is redeclared in int |}]
;;

let%expect_test "main with returns and args" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "a"
           , "b"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "main"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "main1"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Incorrect main error:
    func main must have no arguments and no return values |}]
;;

let%expect_test "arg not declared" =
  pp
    [ Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body = [ Stmt_block [ Stmt_call (Expr_ident "println", [ Expr_ident "a" ]) ] ]
          } )
    ; Decl_func ("println", { args = []; returns = None; body = [] })
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    a is not defined |}]
;;

let%expect_test "no main func" =
  pp
    [ Decl_var
        (Long_decl_one_init
           ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
           , "main"
           , "b"
           , [ "c" ]
           , (Expr_ident "get", []) ))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func
        ( "main4"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ; Decl_func
        ( "main1"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Incorrect main error:
    main func not found |}]
;;

let%expect_test "correct fac" =
  pp
    [ Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_block [ Stmt_call (Expr_ident "fac", [ Expr_const (Const_int 6) ]) ]
              ]
          } )
    ; Decl_func
        ( "fac"
        , { args = [ "n", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body =
              [ Stmt_if
                  { init = None
                  ; cond =
                      Expr_bin_oper (Bin_equal, Expr_ident "n", Expr_const (Const_int 1))
                  ; if_body = [ Stmt_return [ Expr_const (Const_int 1) ] ]
                  ; else_body =
                      Some
                        (Else_block
                           [ Stmt_return
                               [ Expr_bin_oper
                                   ( Bin_multiply
                                   , Expr_ident "n"
                                   , Expr_call
                                       ( Expr_ident "fac"
                                       , [ Expr_bin_oper
                                             ( Bin_subtract
                                             , Expr_ident "n"
                                             , Expr_const (Const_int 1) )
                                         ] ) )
                               ]
                           ])
                  }
              ]
          } )
    ];
  [%expect {|
    CORRECT |}]
;;

let%expect_test "unknown var in if cond" =
  pp
    [ Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_block [ Stmt_call (Expr_ident "fac", [ Expr_const (Const_int 6) ]) ]
              ]
          } )
    ; Decl_func
        ( "fac"
        , { args = [ "n", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body =
              [ Stmt_if
                  { init = None
                  ; cond =
                      Expr_bin_oper (Bin_equal, Expr_ident "a", Expr_const (Const_int 1))
                  ; if_body = [ Stmt_return [ Expr_const (Const_int 1) ] ]
                  ; else_body =
                      Some
                        (Else_block
                           [ Stmt_return
                               [ Expr_bin_oper
                                   ( Bin_multiply
                                   , Expr_ident "n"
                                   , Expr_call
                                       ( Expr_ident "fac"
                                       , [ Expr_bin_oper
                                             ( Bin_subtract
                                             , Expr_ident "n"
                                             , Expr_const (Const_int 1) )
                                         ] ) )
                               ]
                           ])
                  }
              ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Undefined ident error:
    a is not defined |}]
;;

let%expect_test "mismatched types in binop" =
  pp
    [ Decl_var (Long_decl_mult_init (None, ("a", Expr_const (Const_int 5)), []))
    ; Decl_var (Long_decl_mult_init (None, ("b", Expr_const (Const_string "st")), []))
    ; Decl_func ("test", { args = []; returns = None; body = [ Stmt_return [] ] })
    ; Decl_func
        ( "pritln"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_func
        ( "id"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_var (Long_decl_no_init (Type_int, "f", []))
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_defer (Expr_ident "test", [])
              ; Stmt_long_var_decl
                  (Long_decl_mult_init
                     ( None
                     , ("c", Expr_bin_oper (Bin_sum, Expr_ident "a", Expr_ident "b"))
                     , [] ))
              ; Stmt_go
                  ( Expr_ident "println"
                  , [ Expr_call (Expr_ident "id", [ Expr_const (Const_int 10) ]) ] )
              ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    Types mismatched in binoper |}]
;;

let%expect_test "mismatched type in decl # 1" =
  pp
    [ Decl_var (Long_decl_mult_init (None, ("a", Expr_const (Const_int 5)), []))
    ; Decl_var
        (Long_decl_mult_init (Some Type_string, ("b", Expr_const (Const_int 5)), []))
    ; Decl_func ("test", { args = []; returns = None; body = [ Stmt_return [] ] })
    ; Decl_func
        ( "println"
        , { args = [ "a", Type_int ]
          ; returns = None
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_func
        ( "id"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_var (Long_decl_no_init (Type_int, "f", []))
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_defer (Expr_ident "test", [])
              ; Stmt_long_var_decl
                  (Long_decl_mult_init
                     ( None
                     , ("c", Expr_bin_oper (Bin_sum, Expr_ident "a", Expr_ident "b"))
                     , [] ))
              ; Stmt_go
                  ( Expr_ident "println"
                  , [ Expr_call (Expr_ident "id", [ Expr_const (Const_int 10) ]) ] )
              ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    Types mismatched in binoper |}]
;;

let%expect_test "mismatched type in decl # 2" =
  pp
    [ Decl_var (Long_decl_mult_init (None, ("a", Expr_const (Const_string "s")), []))
    ; Decl_var (Long_decl_mult_init (None, ("b", Expr_const (Const_int 5)), []))
    ; Decl_func ("test", { args = []; returns = None; body = [ Stmt_return [] ] })
    ; Decl_func
        ( "println"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_func
        ( "id"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_var (Long_decl_no_init (Type_int, "f", []))
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_defer (Expr_ident "test", [])
              ; Stmt_long_var_decl
                  (Long_decl_mult_init
                     ( None
                     , ("c", Expr_bin_oper (Bin_sum, Expr_ident "a", Expr_ident "b"))
                     , [] ))
              ; Stmt_go
                  ( Expr_ident "println"
                  , [ Expr_call (Expr_ident "id", [ Expr_const (Const_int 10) ]) ] )
              ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    Types mismatched in binoper |}]
;;

let%expect_test "mismatched type in func_call" =
  pp
    [ Decl_var (Long_decl_mult_init (None, ("a", Expr_const (Const_int 5)), []))
    ; Decl_var (Long_decl_mult_init (None, ("b", Expr_const (Const_int 5)), []))
    ; Decl_func ("test", { args = []; returns = None; body = [ Stmt_return [] ] })
    ; Decl_func
        ( "println"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_func
        ( "id"
        , { args = [ "a", Type_string ]
          ; returns = Some (Only_types (Type_string, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_var (Long_decl_no_init (Type_int, "f", []))
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_defer (Expr_ident "test", [])
              ; Stmt_long_var_decl
                  (Long_decl_mult_init
                     ( None
                     , ( "c"
                       , Expr_bin_oper
                           ( Bin_sum
                           , Expr_ident "a"
                           , Expr_call
                               (Expr_ident "id", [ Expr_const (Const_string "st") ]) ) )
                     , [] ))
              ; Stmt_go
                  ( Expr_ident "println"
                  , [ Expr_call (Expr_ident "id", [ Expr_const (Const_int 10) ]) ] )
              ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    Types mismatched in binoper |}]
;;

let%expect_test "correct #3" =
  pp
    [ Decl_var (Long_decl_mult_init (None, ("a", Expr_const (Const_int 5)), []))
    ; Decl_var (Long_decl_mult_init (Some Type_int, ("b", Expr_const (Const_int 5)), []))
    ; Decl_func ("test", { args = []; returns = None; body = [ Stmt_return [] ] })
    ; Decl_func
        ( "println"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_func
        ( "id"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_var (Long_decl_no_init (Type_int, "f", []))
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_defer (Expr_ident "test", [])
              ; Stmt_long_var_decl
                  (Long_decl_mult_init
                     ( None
                     , ("c", Expr_bin_oper (Bin_sum, Expr_ident "a", Expr_ident "b"))
                     , [] ))
              ; Stmt_go
                  ( Expr_ident "println"
                  , [ Expr_call (Expr_ident "id", [ Expr_const (Const_int 10) ]) ] )
              ]
          } )
    ];
  [%expect {|
    CORRECT |}]
;;

let%expect_test "return type of func mismatch" =
  pp
    [ Decl_var (Long_decl_mult_init (None, ("a", Expr_const (Const_int 5)), []))
    ; Decl_var (Long_decl_mult_init (Some Type_int, ("b", Expr_const (Const_int 5)), []))
    ; Decl_func ("test", { args = []; returns = None; body = [ Stmt_return [] ] })
    ; Decl_func
        ( "println"
        , { args = [ "a", Type_string ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_func
        ( "id"
        , { args = [ "a", Type_int ]
          ; returns = Some (Only_types (Type_int, []))
          ; body = [ Stmt_return [ Expr_ident "a" ] ]
          } )
    ; Decl_var (Long_decl_no_init (Type_int, "f", []))
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_defer (Expr_ident "test", [])
              ; Stmt_long_var_decl
                  (Long_decl_mult_init
                     ( None
                     , ("c", Expr_bin_oper (Bin_sum, Expr_ident "a", Expr_ident "b"))
                     , [] ))
              ; Stmt_go
                  ( Expr_ident "println"
                  , [ Expr_call (Expr_ident "id", [ Expr_const (Const_int 10) ]) ] )
              ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    Types mismatched in binoper |}]
;;

let%expect_test "return with empty func returns" =
  pp
    [ Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a", Type_int; "b", Type_int ]
          ; returns = None
          ; body = [ Stmt_return [ Expr_const (Const_int 5) ] ]
          } )
    ];
  [%expect
    {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    func return types mismatch |}]
;;

let%expect_test "correct anon_func" =
  pp
    [ Decl_func ("s", { args = [ "a", Type_string ]; returns = None; body = [] })
    ; Decl_func
        ( "main"
        , { args = []
          ; returns = None
          ; body =
              [ Stmt_short_var_decl
                  (Short_decl_mult_init
                     ( ( "value"
                       , Expr_const
                           (Const_func
                              { args = [ "a", Type_string ]
                              ; returns = None
                              ; body =
                                  [ Stmt_short_var_decl
                                      (Short_decl_mult_init
                                         ( ( "g"
                                           , Expr_const
                                               (Const_func
                                                  { args = [ "a", Type_string ]
                                                  ; returns = None
                                                  ; body =
                                                      [ Stmt_call
                                                          ( Expr_ident "s"
                                                          , [ Expr_const
                                                                (Const_string "Test")
                                                            ] )
                                                      ]
                                                  }) )
                                         , [] ))
                                  ; Stmt_call
                                      ( Expr_ident "s"
                                      , [ Expr_const (Const_string "Test") ] )
                                  ; Stmt_call
                                      (Expr_ident "g", [ Expr_const (Const_string "2") ])
                                  ]
                              }) )
                     , [] ))
              ; Stmt_call (Expr_ident "value", [ Expr_const (Const_string "4") ])
              ]
          } )
    ];
  [%expect {|
    CORRECT |}]
;;
