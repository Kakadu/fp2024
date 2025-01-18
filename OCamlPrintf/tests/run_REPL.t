Copyright 2024-2025, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../repl/REPL.exe -dparsetree -fromfile factorial.txt
  [(Struct_value (Recursive,
      { pat = (Pat_var "factorial");
        exp =
        (Exp_fun ((Pat_var "n"), [],
           (Exp_ifthenelse (
              (Exp_apply
                 ((Exp_ident "<="),
                  (Exp_apply
                     ((Exp_ident "n"), (Exp_constant (Const_integer 1)))))),
              (Exp_constant (Const_integer 1)),
              (Some (Exp_apply
                       ((Exp_ident "*"),
                        (Exp_apply
                           ((Exp_ident "n"),
                            (Exp_apply
                               ((Exp_ident "factorial"),
                                (Exp_apply
                                   ((Exp_ident "-"),
                                    (Exp_apply
                                       ((Exp_ident "n"),
                                        (Exp_constant (Const_integer 1)))))))))))))
              ))
           ))
        },
      []))
    ]

  $ ../repl/REPL.exe -dparsetree <<EOF
  > let prime n = 
  >  let rec check_zero x d = 
  >    match d with 
  >    | 1 -> true
  >    | _ -> x mod d <> 0 && check_zero x (d - 1) 
  >  in
  >  match n with
  >  | 0 -> false
  >  | 1 -> false
  >  | _ -> check_zero n (n - 1);;
  [(Struct_value (Nonrecursive,
      { pat = (Pat_var "prime");
        exp =
        (Exp_fun ((Pat_var "n"), [],
           (Exp_let (Recursive,
              { pat = (Pat_var "check_zero");
                exp =
                (Exp_fun ((Pat_var "x"), [(Pat_var "d")],
                   (Exp_match ((Exp_ident "d"),
                      { left = (Pat_constant (Const_integer 1));
                        right = (Exp_construct ("true", None)) },
                      [{ left = Pat_any;
                         right =
                         (Exp_apply
                            ((Exp_ident "&&"),
                             (Exp_apply
                                ((Exp_apply
                                    ((Exp_ident "<>"),
                                     (Exp_apply
                                        ((Exp_apply
                                            ((Exp_apply
                                                ((Exp_ident "x"),
                                                 (Exp_ident "mod"))),
                                             (Exp_ident "d"))),
                                         (Exp_constant (Const_integer 0)))))),
                                 (Exp_apply
                                    ((Exp_apply
                                        ((Exp_ident "check_zero"),
                                         (Exp_ident "x"))),
                                     (Exp_apply
                                        ((Exp_ident "-"),
                                         (Exp_apply
                                            ((Exp_ident "d"),
                                             (Exp_constant (Const_integer 1))))))))))))
                         }
                        ]
                      ))
                   ))
                },
              [],
              (Exp_match ((Exp_ident "n"),
                 { left = (Pat_constant (Const_integer 0));
                   right = (Exp_construct ("false", None)) },
                 [{ left = (Pat_constant (Const_integer 1));
                    right = (Exp_construct ("false", None)) };
                   { left = Pat_any;
                     right =
                     (Exp_apply
                        ((Exp_apply ((Exp_ident "check_zero"), (Exp_ident "n"))),
                         (Exp_apply
                            ((Exp_ident "-"),
                             (Exp_apply
                                ((Exp_ident "n"),
                                 (Exp_constant (Const_integer 1))))))))
                     }
                   ]
                 ))
              ))
           ))
        },
      []))
    ]

  $ ../repl/REPL.exe -fromfile factorial.txt
  val factorial : int -> int

  $ ../repl/REPL.exe -fromfile manytests/do_not_type/001.ml
  Infer error: Undefined variable 'fac'

  $ ../repl/REPL.exe -fromfile manytests/do_not_type/002if.ml
  Infer error: Unification failed on int and bool

  $ ../repl/REPL.exe -fromfile manytests/do_not_type/003occurs.ml
  Infer error: Occurs check failed: the type variable 'ty1 occurs inside 'ty1 -> 'ty3

  $ ../repl/REPL.exe -fromfile manytests/do_not_type/004let_poly.ml
  Infer error: Unification failed on int and bool

  $ ../repl/REPL.exe -fromfile manytests/do_not_type/015tuples.ml
  Infer error: Only variables are allowed as left-hand side of `let rec'

  $ ../repl/REPL.exe -fromfile manytests/do_not_type/099.ml
  Infer error: Only variables are allowed as left-hand side of `let rec'

  $ ../repl/REPL.exe -fromfile manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/004manyargs.ml
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/005fix.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ../repl/REPL.exe -fromfile manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ../repl/REPL.exe -fromfile manytests/typed/010sukharev.ml
  val _1 : int -> int -> int * 'a -> bool
  val _2 : int
  val _3 : (int * string) option
  val _4 : int -> 'a
  val _5 : int
  val _6 : 'a option -> 'a
  val int_of_option : int option -> int
  val _42 : int -> bool
  val id1 : 'a -> 'a
  val id2 : 'b -> 'b

  $ ../repl/REPL.exe -fromfile manytests/typed/015tuples.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('b -> 'a) -> 'b * 'b -> 'a * 'a
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ../repl/REPL.exe -fromfile manytests/typed/016lists.ml
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'b list -> 'a list -> ('b * 'a) list
  val main : int
