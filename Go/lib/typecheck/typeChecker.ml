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
  | Some x ->
    (match x with
     | Only_types (x, y) -> Type_func (args, x :: y)
     | Ident_and_types (x, y) -> Type_func (args, retrieve_paris_second (x :: y)))
  | None -> Type_func (args, [])
;;

let retrieve_type_const x =
  match x with
  | Const_array (x, y, _) -> return (Type_array (x, y))
  | Const_int _ -> return Type_int
  | Const_string _ -> return Type_string
  | Const_func x -> return (retrieve_anon_func x)
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

let eq_type t1 t2 = eq equal_type' t1 t2

let check_eq t1 t2 =
  match equal_type' t1 t2 with
  | true -> return ()
  | false -> fail (TypeCheckError (Mismatched_types "Types mismatched in binoper"))
;;

let rec retrieve_type_expr x =
  match x with
  | Expr_const x -> retrieve_type_const x
  | Expr_un_oper (_, x) -> retrieve_type_expr x
  | Expr_ident x -> retrieve_ident x
  | Expr_bin_oper (o, x, y) ->
    let compare_arg_typ type1 type2 =
      retrieve_type_expr type1
      >>= fun type1 -> retrieve_type_expr type2 >>= fun type2 -> eq_type type1 type2
    in
    let compare_operation_typ type1 type2 t =
      compare_arg_typ type1 type2 >>= fun e -> eq_type e t
    in
    (match o with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ x y Type_int *> return Type_int
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ x y Type_int *> return Type_bool
     | Bin_or | Bin_and -> compare_operation_typ x y Type_bool *> return Type_bool
     | Bin_equal | Bin_not_equal -> compare_arg_typ x y *> return Type_bool)
  | Expr_call (x, y) ->
    map retrieve_type_expr y *> retrieve_type_expr x
    >>= fun x ->
    (match x with
     | Type_func (x, y) ->
       (match List.length y with
        | 1 -> return (List.nth y 0)
        | _ -> return (Type_func (x, y)))
     | _ -> fail (TypeCheckError Check_failed))
  | Expr_chan_receive x -> retrieve_type_expr x
  | Expr_index (x, y) ->
    if retrieve_type_expr y = return Type_int
    then retrieve_type_expr x
    else fail (TypeCheckError (Mismatched_types "Array index type mismatched"))
;;

let retrieve_idents_from_long_var_decl env decl =
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
  | Long_decl_no_init (k, x, y) -> iter (env_r k) (x :: y)
  | Long_decl_mult_init (k, (x, z), y) ->
    (match k with
     | Some k ->
       iter
         ((fun k (i, x) -> (retrieve_type_expr x >>= fun x -> check_eq x k) *> env_r k i)
            k)
         ((x, z) :: y)
     | None ->
       iter
         (fun (x, z) -> retrieve_type_expr z >>= env_l x)
         (List.combine (x :: retrieve_paris_first y) (z :: retrieve_paris_second y)))
  | Long_decl_one_init (k, x, y, z, l) ->
    (match k with
     | Some k ->
       if retrieve_type_expr (Expr_call l) != return k
       then fail (TypeCheckError (Mismatched_types (Printf.sprintf "%s" (print_type k))))
       else iter (env_r k) (x :: y :: z)
     | None ->
       iter
         (fun (x, z) -> retrieve_type_expr z >>= env_l x)
         (List.combine (x :: y :: z) (List.map (fun _ -> Expr_call l) (x :: y :: z))))
;;

let retrieve_idents_from_short_var_decl = function
  | Short_decl_mult_init ((x, z), y) ->
    iter
      (fun (x, z) -> retrieve_type_expr z >>= save_local_ident_l x)
      (List.combine (x :: retrieve_paris_first y) (z :: retrieve_paris_second y))
  | Short_decl_one_init (x, y, z, l) ->
    iter
      (fun (x, z) -> retrieve_type_expr z >>= save_local_ident_l x)
      (List.combine (x :: y :: z) (List.map (fun _ -> Expr_call l) (x :: y :: z)))
;;

let check_func_call (x, y) = retrieve_type_expr x *> map retrieve_type_expr y *> return ()

let rec check_lvalue = function
  | Lvalue_ident x -> retrieve_ident x
  | Lvalue_array_index (x, y) -> check_lvalue x *> retrieve_type_expr y
;;

let check_assign = function
  | Assign_mult_expr (x, z) ->
    iter
      (fun (x, y) ->
        check_lvalue x
        >>= fun type1 -> retrieve_type_expr y >>= fun type2 -> check_eq type1 type2)
      (x :: z)
  | Assign_one_expr (x, y, _, w) -> check_lvalue x *> check_lvalue y *> check_func_call w
;;

let check_init = function
  | Some x ->
    (match x with
     | Init_assign x -> check_assign x
     | Init_call x -> check_func_call x
     | Init_decl _ -> return () (*доделать*)
     | Init_decr x -> seek_ident x
     | Init_incr x -> seek_ident x
     | Init_receive x -> retrieve_type_expr x *> return ()
     | Init_send (x, y) -> seek_ident x *> retrieve_type_expr y *> return ())
  | None -> return ()
;;

let rec check_stmt = function
  | Stmt_long_var_decl x -> retrieve_idents_from_long_var_decl Loc x
  | Stmt_short_var_decl x -> retrieve_idents_from_short_var_decl x
  | Stmt_incr x -> retrieve_ident x >>= fun e -> check_eq e Type_int
  | Stmt_decr x -> retrieve_ident x >>= fun e -> check_eq e Type_int
  | Stmt_assign x -> check_assign x
  | Stmt_call x -> retrieve_type_expr (Expr_call x) *> return ()
  | Stmt_defer x -> check_func_call x
  | Stmt_go x -> check_func_call x
  | Stmt_chan_send (x, y) -> seek_ident x *> retrieve_type_expr y *> return ()
  | Stmt_block x -> iter check_stmt x
  | Stmt_break -> return ()
  | Stmt_chan_receive x -> retrieve_type_expr x *> return ()
  | Stmt_continue -> return ()
  | Stmt_return x ->
    (get_func_name
     >>= read_ident
     >>= fun x1 ->
     (match x1 with
      | Type_func (_, y) ->
        (match List.length x = List.length y with
         | true -> return (List.combine x y)
         | false -> fail (TypeCheckError (Mismatched_types "func return types mismatch")))
      | _ -> fail (TypeCheckError Check_failed))
     >>= iter (fun (x, y) -> retrieve_type_expr x >>= fun type1 -> check_eq y type1))
    *> return ()
  | Stmt_if x ->
    check_init x.init
    *> retrieve_type_expr x.cond
    *> iter check_stmt x.if_body
    *>
      (match x.else_body with
      | Some x ->
        (match x with
         | Else_block x -> iter check_stmt x
         | Else_if x -> check_stmt (Stmt_if x))
      | None -> return ())
  | Stmt_for x ->
    check_init x.init
    *> check_init x.post
    *> iter check_stmt x.body
    *>
      (match x.cond with
      | Some x -> retrieve_type_expr x *> return ()
      | None -> return ())
;;

let check_func args body =
  iter2 save_local_ident_r (retrieve_paris_second args) (retrieve_paris_first args)
  *> iter check_stmt body
;;

let check_top_decl_funcs = function
  | Decl_func (x, y) ->
    write_local MapIdent.empty *> save_global_ident_r (retrieve_anon_func y) x
  | Decl_var x -> retrieve_idents_from_long_var_decl Glob x
;;

let check_top_decl = function
  | Decl_func (x, y) ->
    write_local MapIdent.empty *> write_func_name x *> check_func y.args y.body
  | Decl_var _ -> return ()
;;

let type_check code =
  run
    (check_main code *> iter check_top_decl_funcs code *> iter check_top_decl code)
    (MapIdent.empty, MapIdent.empty, None)
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

let%expect_test "multiple declaration in global space" =
  pp
    [ Decl_var
        (Long_decl_one_init (Some Type_int, "a", "foo", [ "c" ], (Expr_ident "get", [])))
    ; Decl_var (Long_decl_no_init (Type_int, "x", []))
    ; Decl_func ("main", { args = []; returns = None; body = [] })
    ; Decl_func
        ( "foo"
        , { args = [ "a1", Type_int; "c", Type_int; "b", Type_int ]
          ; returns = Some (Only_types (Type_bool, []))
          ; body = []
          } )
    ];
  [%expect {|
    ERROR WHILE TYPECHECK WITH Mismatched types
    int |}]
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
