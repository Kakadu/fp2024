(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Eval
open Typecheck

let pp str =
  match parse parse_file str with
  | Error _ -> print_endline ": syntax error"
  | Ok ast ->
    (match TypeChecker.type_check ast with
     | Result.Error (Runtime_error _) -> ()
     | Result.Error (Type_check_error err) ->
       prerr_endline ("Typecheck error: " ^ Errors.pp_typecheck_error err)
     | Result.Ok _ ->
       (match eval ast with
        | Result.Error (Type_check_error _) -> ()
        | Result.Error (Runtime_error err) ->
          prerr_endline ("Runtime error: " ^ Errors.pp_runtime_error err)
        | Result.Ok _ -> prerr_endline "Correct evaluating"))
;;

let%expect_test "ok: single main" =
  pp
    {|
    func main() {
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")      
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
    }
    |};
  [%expect
    {|
    Correct evaluating
    kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml kill OCaml |}]
;;

let%expect_test "ok: single long_var_init" =
  pp {|
    var x = "kill OCaml"
    func main() {print(x)}
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: multiple long_var_init" =
  pp {|
    var x, y = "kill ", "OCaml"
    func main() {print(x, y)}
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: simple func_call with args" =
  pp
    {|
    var x, y = "kill ", "OCaml"
    func foo(x string, y string) {
      print(x, y)
    }
  
    func main() {foo(x, y)}
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: simple value func call" =
  pp
    {|
    var x = "kill "
    func foo(x string, y string) {
      print(x, y)
    }
  
    func main() {
      foo(x, "OCaml")
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: local var decl func call" =
  pp
    {|
    var x = "kill "
  
    func main() {
      var y = "OCaml"
      print(x, y)
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: func args init" =
  pp
    {|
    var x = "kill "
    func foo(x string, k string) {
      var z = " OCaml"
      print(x, k, z)
    }
  
    func main() {
      var y = "OCaml"
      foo(x, y)
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml OCaml |}]
;;

let%expect_test "ok: assignment check" =
  pp
    {|
    var x = "kill "
    func foo(x string, k string) {
      var z = " OCaml"
      print(x, k, z)
    }
  
    func main() {
      x = "OCaml "
      var y = "OCaml"
      foo(x, y)
    }
    |};
  [%expect {|
    Correct evaluating
    OCaml OCaml OCaml |}]
;;

let%expect_test "ok: simple arithmetic check" =
  pp
    {|
    var x int = 1
    func foo(k int) {
      z := 1
      z++
      print(k + 1 + z)
    }
  
    func main() {
      x = 100
      x++       
      foo(x + 1)
    }
    |};
  [%expect {|
    Correct evaluating
    105 |}]
;;

let%expect_test "ok: short var init" =
  pp
    {|
    func foo(k int) {
      z := k 
      z = z + 1
      print(k + 1 + z)
    }
  
    func main() {
      x := 100
      x++       
      foo(x + 1)
    }
    |};
  [%expect {|
    Correct evaluating
    206 |}]
;;

let%expect_test "ok: simple if" =
  pp
    {|
    func foo(k int) {
      var z = k 
      if k < 100 {
          print("Error")
        } else{
        print("Correct")
      }
      if k >= 100 {
          print(" Correct")
        } else{
        print(" Error")
      }
      if k < 10 {
          print("Error")
        } else if k < 20 {
        print("Error")
      } else {
         print(" Correct")
      }
      if k < 20 {
        print("Error")
      }
    }
  
    func main() {
      x := 100
      x++       
      foo(x + 1)
    }
    |};
  [%expect {|
    Correct evaluating
    Correct Correct Correct |}]
;;

let%expect_test "ok: nested if decl" =
  pp
    {|
    func foo(k int) {
      z := k 
      if k >= 100 {
        o := 1
        z = 1
        print(o)
        print(z)
      }
      print(z)
    }
  
    func main() {
      x := 100
      x++       
      foo(x + 1)
      x = 1
      print(x)
      print(x)
    }
    |};
  [%expect {|
    Correct evaluating
    11111 |}]
;;

let%expect_test "ok: nested if decl" =
  pp
    {|

    func main() {
        i := 1
        for i <= 3 {
            println(i)
            i = i + 1
        }
        for j := 0; j < 3; j++ {
            
            println(j)
        }
        for {
            println("loop")
            break
        }
      }
    |};
  [%expect {|
    Correct evaluating

    1
    2
    3
    0
    1
    2
    loop |}]
;;

let%expect_test "ok: noreturn check" =
  pp
    {|
    var x = "kill "
    func foo(){
      return 
      x = "XXXXXXERRROR"
    }
    func main() {
      var y = "OCaml"
      foo()
      print(x, y) 
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml |}]
;;

let%expect_test "ok: simple return check" =
  pp
    {|
    var x = "kill "
    func foo() string {
      return "CORRECT"
    }
    func main() {
      var y = "OCaml"
      print(x, y, foo()) 
      print(x, y, foo()) 
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCamlCORRECTkill OCamlCORRECT |}]
;;

let%expect_test "ok: factorial check" =
  pp
    {|
    func fac(n int) int {
        if n == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    } 
    func main() {
      print(fac(5))
    }
    |};
  [%expect {|
    Correct evaluating
    120 |}]
;;

let%expect_test "ok: funclit check" =
  pp
    {|
      func main() {
        a := 0  
        f := func(x int) {
          a = a + x
        }
        a = a + 1
        f(1)
        print(a)
      }
    |};
  [%expect {|
    Correct evaluating
    2 |}]
;;

(*(Decl_func
        ("adder",
         { args = [("x", Type_int)];
           returns =
           (Some (Only_types ((Type_func ([Type_int], [Type_int])), [])));
           body =
           [(Stmt_short_var_decl
               (Short_decl_mult_init (("sum", (Expr_const (Const_int 0))), [])));
             (Stmt_return
                [(Expr_const
                    (Const_func
                       { args = [("x", Type_int)];
                         returns = (Some (Only_types (Type_int, [])));
                         body =
                         [(Stmt_assign
                             (Assign_mult_expr (
                                ((Lvalue_ident "sum"),
                                 (Expr_bin_oper (Bin_sum, (Expr_ident "sum"),
                                    (Expr_ident "x")))),
                                [])));
                           (Stmt_return [(Expr_ident "sum")])]
                         }))
                  ])
             ]
           }));
      (Decl_func
         ("main",
          { args = []; returns = None;
            body =
            [(Stmt_short_var_decl
                (Short_decl_mult_init (
                   ("pos",
                    (Expr_call
                       ((Expr_ident "adder"), [(Expr_const (Const_int 1))]))),
                   [("neg",
                     (Expr_call
                        ((Expr_ident "adder"), [(Expr_const (Const_int 1))])))
                     ]
                   )));
              (Stmt_call
                 ((Expr_ident "print"), [(Expr_const (Const_string "GAINED"))]));
              Stmt_for {
                init =
                (Some (Init_decl
                         (Short_decl_mult_init (
                            ("i", (Expr_const (Const_int 0))), []))));
                cond =
                (Some (Expr_bin_oper (Bin_less, (Expr_ident "i"),
                         (Expr_const (Const_int 10)))));
                post = (Some (Init_incr "i"));
                body =
                [(Stmt_call
                    ((Expr_ident "println"),
                     [(Expr_call ((Expr_ident "pos"), [(Expr_ident "i")]));
                       (Expr_call
                          ((Expr_ident "neg"),
                           [(Expr_bin_oper (Bin_multiply,
                               (Expr_un_oper (Unary_minus,
                                  (Expr_const (Const_int 2)))),
                               (Expr_ident "i")))
                             ]))
                       ]))
                  ]}
              ]
            }))
      ]*)
let%expect_test "ok: closure check" =
  pp
    {|
    func adder(x int) func(int) int {
      sum := 0
      return func(x int) int {
        sum = sum + x
        return sum
      }
    }

    func main() {
      pos, neg := adder(1), adder(1)
      for i := 0; i < 10; i++ {
       println(pos(i), neg(-2 * i))
      }
    }
    |};
  [%expect
    {|
    Correct evaluating

    00
    1-2
    3-6
    6-12
    10-20
    15-30
    21-42
    28-56
    36-72
    45-90 |}]
;;

let%expect_test "ok: spimple array test" =
  pp
    {|
      func main() {
        var a = [2]string{"a", "a"}
        a[0] = "Kill "
        a[1] = "Ocaml"
        println(a[0], a[1])
    }

    |};
  [%expect {|
    Correct evaluating

    Kill Ocaml |}]
;;

(* goroutines *)

let%expect_test "ok: two goroutine sync with unbuffered chanel" =
  pp
    {|
    func goroutine2(c chan int) {
      println("go2: trying to receive")
      a := <-c
      println("go2: receive success. Value:", a)
    }

    func main() {
      c := make(chan int)

      v := 0

      go goroutine2(c)

      println("go1: trying to send. Value:", v)
      c <- v
      println("go1: send success")
    }
    |};
  [%expect
    {|
    Correct evaluating

    go1: trying to send. Value:0
    go2: trying to receive
    go2: receive success. Value:0
    go1: send success |}]
;;

let%expect_test "ok: receive and send back" =
  pp
    {|
    func goroutine2(c chan int) {
      println("go2: trying to receive")
      a := <-c
      println("go2: receive success. Value:", a)

      println("go2: trying to send. Value:", a)
      c <- a
      println("go2: send success")
    }

    func main() {
      c := make(chan int)

      v := 0

      go goroutine2(c)

      println("go1: trying to send. Value:", v)
      c <- v
      println("go1: send success")

      println("go1: trying to receive")
      a := <-c
      println("go1: receive success. Value:", a)
    }
    |};
  [%expect
    {|
    Correct evaluating

    go1: trying to send. Value:0
    go2: trying to receive
    go2: receive success. Value:0
    go2: trying to send. Value:0
    go1: send success
    go1: trying to receive
    go1: receive success. Value:0 |}]
;;

let%expect_test "err: sender without receiver" =
  pp {|
    func main() {
      c := make(chan int)
      c <- 0
    }
    |};
  [%expect {| Runtime error: Deadlock: goroutine 1 trying to send to chan 1 |}]
;;

let%expect_test "err: receiver without sender" =
  pp {|
    func main() {
      c := make(chan int)
      <-c
    }
    |};
  [%expect {| Runtime error: Deadlock: goroutine 1 trying to receive from chan 1 |}]
;;

(* let%expect_test "ok: two goroutines sending to the same chanel before value received" =
  pp
    {|
    func main2(c chan int) {
      println("go2: sending value 2")
      c <- 2
      println("go2: value 2 sent successfully")
    }

    func main3(c chan int) {
      println("go3: received value: ", <-c)
    }

    func main() {
      c := make(chan int)
      
      go main2(c)
      go main3(c)

      println("go1: sending value 1")
      c <- 1
      println("go1: value 1 sent successfully")
    }
    |};
  [%expect {| |}]
;; *)

(* let%expect_test "ok: synchronised printing" =
  pp
    {|
    var c = make(chan int)

    var a = 0

    func main2() {
      <-c
      a++
      println("second: ", a)
      c <- 0
    }

    func main() {
      go main2()

      c <- 0
      a++
      println("first: ", a)
      <-c
    }
    |};
  [%expect {| Runtime error: No goroutine running |}]
;; *)
