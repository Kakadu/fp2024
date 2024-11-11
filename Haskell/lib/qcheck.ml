(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format
open Parser
open Pprintast

let integer n = Integer (Integer.Nonnegative_integer.of_int n)
let createbool b = Bool b
let treeparam x = TreeParam x
let listparam x = ListParam x
let tupleparams x y = TupleParams (x, y, [])
let functiontype x = FunctionType x
let funct x y = FuncT (x, y, [])
let const_gen = QCheck.Gen.(oneof [ map integer nat; map createbool bool; return Unit ])
let functype_gen tp_gen = QCheck.Gen.(map2 funct tp_gen tp_gen)

let tp_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      match n with
      | 0 -> oneof [ return TUnit; return TInt; return TBool ]
      | n ->
        oneof
          [ return TUnit
          ; return TInt
          ; return TBool
          ; map treeparam (self (n / 5))
          ; map listparam (self (n / 5))
          ; map2 tupleparams (self (n / 5)) (self (n / 5))
          ; map functiontype (functype_gen (self (n / 5)))
          ]))
;;

let binop_gen =
  QCheck.Gen.(
    oneof
      [ return And
      ; return Or
      ; return Plus
      ; return Minus
      ; return Divide
      ; return Mod
      ; return Cons
      ; return Multiply
      ; return Equality
      ; return Pow
      ; return Inequality
      ; return Less
      ; return Greater
      ; return EqualityOrLess
      ; return EqualityOrGreater
      ])
;;

let ident x = Ident (sprintf "%c" x)

let ident_gen =
  let open Char in
  QCheck.Gen.(map ident (map chr (int_range (Char.code 'a') (Char.code 'z'))))
;;

let pattern_gen pat_gen : pattern QCheck.Gen.t =
  QCheck.Gen.(map (fun p -> [], p, []) pat_gen)
;;

let pcons x y = PCons (x, y)
let penum x = PEnum [ x ]

let listpat_gen pat_gen =
  QCheck.Gen.(
    oneof
      [ map2 pcons (pattern_gen pat_gen) (pattern_gen pat_gen)
      ; map penum (pattern_gen pat_gen)
      ])
;;

let pnode x y z = PNode (x, y, z)

let treepat_gen pat_gen =
  QCheck.Gen.(
    oneof
      [ return PNul
      ; map3 pnode (pattern_gen pat_gen) (pattern_gen pat_gen) (pattern_gen pat_gen)
      ])
;;

let pconst_gen =
  QCheck.Gen.(
    oneof
      [ map (fun x -> OrdinaryPConst x) const_gen
      ; map (fun x -> NegativePInteger (Integer.Nonnegative_integer.of_int x)) nat
      ])
;;

let pconst x = PConst x
let pidentificator x = PIdentificator x
let plist x = PList x
let ptuple x y = PTuple (x, y, [])
let pmaybe x = PMaybe (Just x)
let ptree x = PTree x

let pat_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      match n with
      | 0 ->
        oneof
          [ return PWildcard
          ; map pconst pconst_gen
          ; map pidentificator ident_gen
          ; return (PMaybe Nothing)
          ]
      | n ->
        oneof
          [ return PWildcard
          ; map pconst pconst_gen
          ; map pidentificator ident_gen
          ; map plist (listpat_gen (self (n / 5)))
          ; map ptree (treepat_gen (self (n / 5)))
          ; map2 ptuple (pattern_gen (self (n / 5))) (pattern_gen (self (n / 5)))
          ; oneof [ map pmaybe (pattern_gen (self (n / 5))); return (PMaybe Nothing) ]
          ]))
;;

let expr_gen expression_gen : expr QCheck.Gen.t =
  QCheck.Gen.(map (fun x -> x, []) expression_gen)
;;

let comprehension_gen expression_gen pat_gen =
  QCheck.Gen.(
    oneof
      [ map (fun x -> Condition x) (expr_gen expression_gen)
      ; map2 (fun x y -> Generator (x, y)) (pattern_gen pat_gen) (expr_gen expression_gen)
      ])
;;

let ordinarylistbld_gen expression_gen pat_gen =
  QCheck.Gen.(
    oneof
      [ map2
          (fun x y -> ComprehensionList (x, y, []))
          (expr_gen expression_gen)
          (comprehension_gen expression_gen pat_gen)
      ; map (fun x -> IncomprehensionlList [ x ]) (expr_gen expression_gen)
      ])
;;

let listbld_gen expression_gen pat_gen =
  QCheck.Gen.(
    oneof
      [ map (fun x -> LazyList (x, None, None)) (expr_gen expression_gen)
      ; map (fun x -> OrdList x) (ordinarylistbld_gen expression_gen pat_gen)
      ])
;;

let bindingbody_gen expression_gen =
  QCheck.Gen.(
    oneof
      [ map2
          (fun x y -> Guards ((x, y), []))
          (expr_gen expression_gen)
          (expr_gen expression_gen)
      ; map (fun x -> OrdBody x) (expr_gen expression_gen)
      ])
;;

let binding_gen expression_gen pat_gen =
  QCheck.Gen.(
    oneof
      [ map2
          (fun x y -> VarsDef (x, y, []))
          (pattern_gen pat_gen)
          (bindingbody_gen expression_gen)
      ; map3
          (fun x y z -> FunDef (x, y, [], z, []))
          ident_gen
          (pattern_gen pat_gen)
          (bindingbody_gen expression_gen)
      ; map2 (fun x y -> Decl (x, y)) (pattern_gen pat_gen) tp_gen
      ])
;;

let binary_tree_bld_gen expression_gen =
  QCheck.Gen.(
    oneof
      [ return Nul
      ; map3
          (fun x y z -> Node (x, y, z))
          (expr_gen expression_gen)
          (expr_gen expression_gen)
          (expr_gen expression_gen)
      ])
;;

let expression_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      match n with
      | 0 ->
        oneof
          [ map (fun x -> Const x) const_gen; map (fun x -> Identificator x) ident_gen ]
      | n ->
        oneof
          [ map2
              (fun x y -> TupleBld (x, y, []))
              (expr_gen (self (n / 5)))
              (expr_gen (self (n / 5)))
          ; map (fun x -> ListBld x) (listbld_gen (self (n / 5)) pat_gen)
          ; map3
              (fun x y z -> Binop (x, y, z))
              (expr_gen (self (n / 5)))
              binop_gen
              (expr_gen (self (n / 5)))
          ; map (fun x -> Neg x) (expr_gen (self (n / 5)))
          ; map3
              (fun x y z -> IfThenEsle (x, y, z))
              (expr_gen (self (n / 3)))
              (expr_gen (self (n / 3)))
              (expr_gen (self (n / 3)))
          ; map2
              (fun x y -> FunctionApply (x, y, []))
              (expr_gen (self (n / 5)))
              (expr_gen (self (n / 5)))
          ; map2
              (fun x y -> Lambda (x, [], y))
              (pattern_gen pat_gen)
              (expr_gen (self (n / 5)))
          ; map (fun x -> BinTreeBld x) (binary_tree_bld_gen (self (n / 5)))
          ; map3
              (fun x y z -> Case (x, (y, z), []))
              (expr_gen (self (n / 5)))
              (pattern_gen pat_gen)
              (bindingbody_gen (self (n / 5)))
          ; map2
              (fun x y -> InnerBindings (x, [], y))
              (binding_gen (self (n / 5)) pat_gen)
              (expr_gen (self (n / 5)))
          ]))
;;

let arbitrary_binding expression_gen pat_gen =
  QCheck.make (binding_gen expression_gen pat_gen) ~print:(asprintf "%a" pp_binding)
;;

let test expression_gen pat_gen =
  QCheck.Test.make
    ~name:"test"
    ~count:1
    (arbitrary_binding expression_gen pat_gen)
    (fun t ->
       printf "%a" pp_binding t;
       match parse_line (asprintf "%a" pp_binding t) with
       | Result.Ok _ -> true
       | Result.Error _ -> false)
;;

QCheck_runner.run_tests [ test expression_gen pat_gen ]
