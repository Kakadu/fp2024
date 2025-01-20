(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Eval
open Typecheck

let pp str =
  match parse parse_file str with
  | Error _ -> print_endline ": syntax error"
  | Ok ast ->
    (match type_check ast with
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

let%expect_test "ok: empty main" =
  pp {|
    func main() {}
    |};
  [%expect {|
    Correct evaluating |}]
;;

let%expect_test "ok: simple main with prints" =
  pp
    {|
    func main() {
      print("kill OCaml ")
      print("kill OCaml ")
      print("kill OCaml ")
    }
    |};
  [%expect {|
    Correct evaluating
    kill OCaml kill OCaml kill OCaml |}]
;;

let%expect_test "ok: single long_var_init" =
  pp {|
    var x = "kill OCaml"
    func main() { print(x) }
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
    kill  OCaml |}]
;;

let%expect_test "err: division by zero" =
  pp {|
    func main() { a := 1 / 0 }
    |};
  [%expect {|
    Runtime error: division by zero |}]
;;

let%expect_test "err: by zero modulus" =
  pp {|
    func main() { a := 1 % 0 }
    |};
  [%expect {|
    Runtime error: division by zero |}]
;;

let%expect_test "ok: func call in global var decl" =
  pp {|
    var a = len("asd")
    func main() {
      print(a)
    }
    |};
  [%expect {|
    Correct evaluating
    3 |}]
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
    kill  OCaml |}]
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
    kill  OCaml |}]
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
    kill  OCaml |}]
;;

let%expect_test "ok: func args init" =
  pp
    {|
    var x = "kill"
    func foo(x string, k string) {
      var z = "OCaml"
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
      var z = "OCaml"
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
    OCaml  OCaml OCaml |}]
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

let%expect_test "ok: if with init" =
  pp
    {|
    func main() {
      if a := true; a {
        println("норм")
      }
    }
    |};
  [%expect {|
    Correct evaluating
    норм |}]
;;

let%expect_test "ok: simple for" =
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
      }
    |};
  [%expect {|
    Correct evaluating
    1
    2
    3
    0
    1
    2 |}]
;;

let%expect_test "ok: break" =
  pp
    {|
    func main() {
        a := 0
        for {
            a++
            println(a)
            if a == 5 {
              if true {
                break  
              }
            }
        }
    }
    |};
  [%expect {|
    Correct evaluating
    1
    2
    3
    4
    5 |}]
;;

let%expect_test "ok: continue" =
  pp
    {|
    func main() {
        for i := 0; i < 5; i++ {
            if i == 3 {
                continue
            }
            println(i)
        }
    }
    |};
  [%expect {|
    Correct evaluating
    0
    1
    2
    4 |}]
;;

let%expect_test "ok: function returns on return" =
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
    kill  OCaml |}]
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
    kill  OCaml CORRECTkill  OCaml CORRECT |}]
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
    0 0
    1 -2
    3 -6
    6 -12
    10 -20
    15 -30
    21 -42
    28 -56
    36 -72
    45 -90 |}]
;;

(* arrays *)

let%expect_test "ok: simple array assignment and call" =
  pp
    {|
    func main() {
      var a = [2]string{"a", "a"}
      a[0] = "Kill"
      a[1] = "Ocaml"
      println(a[0], a[1])
    }
    |};
  [%expect {|
    Correct evaluating
    Kill Ocaml |}]
;;

let%expect_test "ok: full array printing" =
  pp
    {|
    func main() {
      a := [...]int{0, 1, 2, 3, 4}
      for i := 0; i < len(a); i++ {
        print(a[i])
      }
    }
    |};
  [%expect {|
    Correct evaluating
    01234 |}]
;;

let%expect_test "ok: array of functions with auto size" =
  pp
    {|
    func main() {
      funcs := [...]func(){
        func() { print(1) },
        func() { print(2) },
        func() { print(3) },
      }

      for i := 0; i < len(funcs); i++ {
        funcs[i]()
      }
    }
    |};
  [%expect {|
    Correct evaluating
    123 |}]
;;

let%expect_test "ok: multidimensional array test & vlong_var_decl no init" =
  pp
    {|
      func main() {
        var a [2][3][4][5]string
        a[0][1][2][1] = "Kill"
        a[1][0][0][3] = "Ocaml"
        println(a[0][1][2][1], a[1][0][0][3])
    }

    |};
  [%expect {|
    Correct evaluating
    Kill Ocaml |}]
;;

let%expect_test "err: array index out of bounds in expr" =
  pp
    {|
      func main() {
        var a [2][3][4][5]string
        a[0][1][2][1] = "Kill"
        a[1][0][0][3] = "Ocaml"
        println(a[0][1][10][1], a[1][0][0][3])
    }

    |};
  [%expect {|
    Runtime error: array index out of bounds |}]
;;

let%expect_test "err: array index out of bounds in lvalue" =
  pp
    {|
      func main() {
        var a [2][3][4][5]string
        a[0][1][2][1] = "Kill"
        a[1][0][10][3] = "Ocaml"
        println(a[0][1][2][1], a[1][0][0][3])
    }

    |};
  [%expect {|
    Runtime error: array index out of bounds |}]
;;

(* defer, panic, recover *)

let%expect_test "ok: simple defer check change local value & return not chenged local \
                 value"
  =
  pp
    {|
      func foo() int{
          a := 0 panic
        defer func (h int){
        a++
        print(a)
        }(a)
        a = a + 1
        print(a)
        return a
      }

      func main() {
        print(foo())
      }
    |};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "ok: defer check function reassignment value" =
  pp
    {|
      func foo() int{
        a := 0 
        
        f := func (){
          a++
          println(a)
        }
        
        defer f()
        
        f = func (){
          a = a + 100
          println(a)
        }
        
        a = a + 1
        print(a)
        return a
      }

      func main() {
        println(foo())
      }
    |};
  [%expect {|
    Correct evaluating
    12
    1 |}]
;;

let%expect_test "ok: defer check function example" =
  pp
    {|
      
func main() {
    f()
    println("Returned normally from f.")
}

func f() {
      println("Calling g.")
      g(0)
      println("Returned normally from g.")
    }

    func g(i int) {
        if i > 3 {
            println("Stop!")
            return 
        }
        defer println("Defer in g", i)
        println("Printing in g", i)
        g(i + 1)
    }
    |};
  [%expect
    {|
    Correct evaluating
    Calling g.
    Printing in g 0
    Printing in g 1
    Printing in g 2
    Printing in g 3
    Stop!
    Defer in g 3
    Defer in g 2
    Defer in g 1
    Defer in g 0
    Returned normally from g.
    Returned normally from f. |}]
;;

let%expect_test "ok: panic does not impact on goroutine without chanels" =
  pp
    {|
      
    func main() {
      println("Creating new goroutine")
      go f()
      println("Finish")
    }

    func f() {
      println("Calling g.")
      g(0)
      println("Returned normally from g.")
    }

    func g(i int) {
      if i > 3 {
          println("Panicking!")
          panic(i)
      }

      defer println("Defer in g", i)
      println("Printing in g", i)
      g(i + 1)
    }
    |};
  [%expect {|
    Correct evaluating
    Creating new goroutine
    Finish |}]
;;

let%expect_test "ok: panic with recover" =
  pp
    {|
      
    func main() {
        f()
        println("Returned normally from f.")
    }

    func f() {
      defer func() {
          r := recover()
          println("Recovered in f with value:", r)
          
      }()
      println("Calling g.")
      g(0)
      println("Returned normally from g.")
    }

    func g(i int) {
        if i > 3 {
            println("Panicking!")
            panic(i)
        }
        defer println("Defer in g", i)
        println("Printing in g", i)
        g(i + 1)
        
    }
    |};
  [%expect
    {|
    Correct evaluating
    Calling g.
    Printing in g 0
    Printing in g 1
    Printing in g 2
    Printing in g 3
    Panicking!
    Defer in g 3
    Defer in g 2
    Defer in g 1
    Defer in g 0
    Recovered in f with value: 4
    Returned normally from f. |}]
;;

let%expect_test "err: not recovered panic" =
  pp
    {|
    func main() {
        f()
        println("Returned normally from f.")
    }

    func f() {
      println("Calling g.")
      g(0)
      println("Returned normally from g.")
    }

    func g(i int) {
        if i > 3 {
          println("Panicking!")
          panic(i)
        }
        defer println("Defer in g", i)
        println("Printing in g", i)
        g(i + 1)
    }
    |};
  [%expect
    {|
    Runtime error: Panic: 4
    Calling g.
    Printing in g 0
    Printing in g 1
    Printing in g 2
    Printing in g 3
    Panicking!
    Defer in g 3
    Defer in g 2
    Defer in g 1
    Defer in g 0 |}]
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
    go1: trying to send. Value: 0
    go2: trying to receive
    go2: receive success. Value: 0
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
      println("go2: send success") // this doesn't execute
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
    go1: trying to send. Value: 0
    go2: trying to receive
    go2: receive success. Value: 0
    go2: trying to send. Value: 0
    go1: send success
    go1: trying to receive
    go1: receive success. Value: 0 |}]
;;

let%expect_test "err: sender without receiver" =
  pp {|
    func main() {
      c := make(chan int)
      c <- 0
    }
    |};
  [%expect {|
    Runtime error: Deadlock: goroutine 1 trying to send to chan 1 |}]
;;

let%expect_test "err: receiver without sender" =
  pp {|
    func main() {
      c := make(chan int)
      <-c
    }
    |};
  [%expect {|
      Runtime error: Deadlock: goroutine 1 trying to receive from chan 1 |}]
;;

let%expect_test "ok: save goroutine receiving two times" =
  pp
    {|
    var a = 0

    func sender(c chan int) {
      a++
      c <- a
    }

    func main() {
      c := make(chan int)
      go sender(c)
      go sender(c)

      println(<-c, <-c)
    }
    |};
  [%expect {|
    Correct evaluating
    1 2 |}]
;;

let%expect_test "ok: simple goroutine test" =
  pp
    {|
      func sum(s [6]int, c chan int) {
        sum := 0
        for v := 0; v < 6; v++{
          sum = sum + v
        }	
        c <- sum
      }

      func main() {
        s := [6]int{7, 2, 8, -9, 4, 0}

        c := make(chan int)
        go sum(s, c)

        println("Waiting for channel receive")
        x := <-c

        println(x)
      }
    
    |};
  [%expect {|
    Correct evaluating
    Waiting for channel receive
    15 |}]
;;

let%expect_test "ok: two goroutines sending to the same chanel before value received" =
  pp
    {|
    func main2(c chan int) {
      println("go2: sending value 2")
      c <- 2
      println("go2: value 2 sent successfully")
    }

    func main3(c chan int) {
      println("go3: received value:", <-c)
      println("go3: received value:", <-c)
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
  [%expect
    {|
    Correct evaluating
    go1: sending value 1
    go2: sending value 2
    go3: received value: 1
    go3: received value: 2
    go1: value 1 sent successfully |}]
;;
