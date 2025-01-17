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
