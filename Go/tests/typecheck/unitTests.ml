(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Typecheck

let pp str =
  match parse parse_file str with
  | Error _ -> print_endline ": syntax error"
  | Ok ast ->
    (match type_check ast with
     | Result.Ok _ -> print_endline "CORRECT"
     | Result.Error (Runtime_error _) -> ()
     | Result.Error (Type_check_error err) ->
       prerr_string ("Typecheck error: " ^ Errors.pp_typecheck_error err))
;;

(********** main func **********)

let%expect_test "ok: single main" =
  pp {|
    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "err: multiple main" =
  pp {|
    func main() {}
    func main() {}
    |};
  [%expect {|
    Typecheck error: Multiple declaration error: main is redeclared |}]
;;

let%expect_test "err: main with returns" =
  pp {|
  func main() bool { return true }
  |};
  [%expect
    {| Typecheck error: Incorrect main error: func main must have no arguments and no return values |}]
;;

let%expect_test "err: main with args" =
  pp {|
  func main(a int) {}
  |};
  [%expect
    {| Typecheck error: Incorrect main error: func main must have no arguments and no return values |}]
;;

let%expect_test "err: no main" =
  pp {|
  var a int
  func foo(b int) {}
  |};
  [%expect {| Typecheck error: Undefined ident error: main is not defined |}]
;;

let%expect_test "ok: main call" =
  pp {|
    func foo() {
        main()
    }

    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

(********** top var decl **********)

let%expect_test "ok: single var decl no type with simple init " =
  pp {|
  var a = 5

  func main() {}
  |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: single var decl with type and right init " =
  pp {|
  var a int = 5

  func main() {}
  |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: single var decl with type and wrong init " =
  pp {|
  var a int = ""

  func main() {}
  |};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: func call init with right number of elements" =
  pp
    {|
    var a, b, c  = get3()

    func get3() (int, int, int) {
        return 1, 2, 3
    }
    
    func main() {}
    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: func call one init with mismatched number of elements" =
  pp {|
    var a, b, c  = get0()

    func get0() {}
    
    func main() {}
    |};
  [%expect
    {| Typecheck error: Mismatched types: Function without returns in expression |}]
;;

let%expect_test "err: func call one init with mismathced types" =
  pp
    {|
    var a, b, c bool = get3()

    func get3() (int, int, int) {
        return 1, 2, 3
    }
    
    func main() {}
    |};
  [%expect
    {| Typecheck error: Mismatched types: (bool, bool, bool) and (int, int, int) |}]
;;

let%expect_test "err: func call one init with mismathced range" =
  pp
    {|
    var a, b = get3()

    func get3() (int, int, int) {
        return 1, 2, 3
    }
    
    func main() {}
    |};
  [%expect
    {| Typecheck error: Mismatched types: function returns wrong number of elements in multiple var assign |}]
;;

let%expect_test "err: var redeclaration" =
  pp {|
    var a = 0

    var a = ""
    
    func main() {}
    |};
  [%expect {| Typecheck error: Multiple declaration error: a is redeclared |}]
;;

(********** top func decl **********)

let%expect_test "ok: simple func" =
  pp {|
    func foo() {}

    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "ok: id func " =
  pp {|
    func id(a int) int {
        return a
    }

    func main() {}
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "err: repeated idents in args" =
  pp {|
    func foo(a, a int) {}

    func main() {}
    |};
  [%expect {|
    Typecheck error: Multiple declaration error: a is redeclared |}]
;;

let%expect_test "err: func redeclaration" =
  pp
    {|
    func foo(a int) {}

    func foo() int {
        return 5
    }

    func main() {}
    |};
  [%expect {|
    Typecheck error: Multiple declaration error: foo is redeclared |}]
;;

let%expect_test "err: func arg redeclaration" =
  pp {|
    func foo(a int) {
        var a int
    }

    func main() {}
    |};
  [%expect {|
    Typecheck error: Multiple declaration error: a is redeclared |}]
;;

let%expect_test "ok: correct var multiple returns short_decl" =
  pp
    {|
    func foo(a int) (int, int){
        return a, 5
    }

    func main() {
        a, b := foo(4)
    }
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "err: incorrect var multiple assign" =
  pp
    {|
    func foo(a int) (int, int){
        return a, 5
    }
    func foo2(a int) (int, int, int){
        return a, 5, 4
    }

    func main() {
        a, b := foo(4)
        a, b = foo2(4)
    }
    |};
  [%expect {|
    Typecheck error: Cannot assign: Multiple return assign failed |}]
;;

let%expect_test "ok: correct var multiple assign" =
  pp
    {|
    func foo(a int) (int, int){
        return a, 5
    }
    func foo2(a int) (int, int){
        return a, 5
    }

    func main() {
        a, b := foo(4)
        a, b = foo2(4)
    }
    |};
  [%expect {|
    CORRECT |}]
;;

let%expect_test "err: incorrect var multiple assign after multiple decl with wrong types" =
  pp
    {|
    func foo(a int) (int, string){
        return a, "g"
    }
    func foo2(a int) (int, int){
        return a, 5
    }

    func main() {
        a, b := foo(4)
        a, b = foo2(4)
    }
    |};
  [%expect {|
    Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: var and func with the same name" =
  pp {|
    var foo int

    func foo() {}

    func main() {}
    |};
  [%expect {|
    Typecheck error: Multiple declaration error: foo is redeclared |}]
;;

let%expect_test "ok: correct declarations #1" =
  pp
    {|
    func main() {}

    func foo(a int, b int, c int) {}

    func foo1(a int, b int, c int) {}   |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: factorial func" =
  pp
    {|
    func main() {
        fac(6)
    }

    func fac(n int) int {
        if n == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    }
  |};
  [%expect {| CORRECT |}]
;;

(********** stmt **********)

let%expect_test "err: incorrect call in stmt" =
  pp
    {|

    func main() {
      println(1, 1, "k", 3)
    }
    
    func swap() (string, string) {
      return "a", "b"
    }
    func println(a string, b string, c string) (string, string, string) {
      return a, b, c
    }

|};
  [%expect {| Typecheck error: Mismatched types: Number of given args mismatched |}]
;;

let%expect_test "err: undefined var inc" =
  pp
    {|
    var x int

    func main() {}

    func foo(a1 int, c int, b int) {
        a2++
    }  
    |};
  [%expect {| Typecheck error: Undefined ident error: a2 is not defined |}]
;;

let%expect_test "ok: global var decl before it's use in code" =
  pp
    {|
    var x int

    func foo(a1 int, c int, b int){
      x++
    }  

    func main() {}
    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: redefined int example" =
  pp {|
    var int string 
    func main() {}

    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: global var decl after it's use in code" =
  pp
    {|
    func foo(a1 int, c int, b int) {
        x++
    } 

    var x int

    func main() {}
    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "fail: missing return statement" =
  pp
    {|
    func foo(a1 int, c int, b int) bool {
        x++
    } 

    var x int

    func main() {}
    |};
  [%expect {| Typecheck error: Missing return: Missing return |}]
;;

let%expect_test "ok: correct returns in different branches of if" =
  pp
    {|
    func foo(a1 int, c int, b int) int {
        if c == 1 {
		      return 1
				} else { 
          if c == 2 {
            return 3
          } else {
            return 2
          }
	      }
    } 
    func main() {}
    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "fail: missing return in nested branch of if" =
  pp
    {|
    var x int
    func foo(a1 int, c int, b int) int {
        if c == 1 {
		      return 1
				} else { 
          if c == 2 {
            return 3
          } else {
            x++
          }
	      }
    } 
    func main() {}
    |};
  [%expect {| Typecheck error: Missing return: Missing return |}]
;;

let%expect_test "err: undefined func call" =
  pp
    {|
    var x int

    func main() {
        foo2()
    }

    func foo(a1 int, c int, b int) bool {}  |};
  [%expect {| Typecheck error: Undefined ident error: foo2 is not defined |}]
;;

let%expect_test "err: arg not declared" =
  pp {|
    func main() {
        println(a)
    }

    func println(a int) {}
  |};
  [%expect {| Typecheck error: Undefined ident error: a is not defined |}]
;;

let%expect_test "err: unknown var in if cond" =
  pp
    {|
    func main() {
        {
            fac(6)
        }
    }

    func fac(n int) int {
        if a == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    }
  |};
  [%expect {| Typecheck error: Undefined ident error: a is not defined |}]
;;

let%expect_test "err: mismatched types in binop" =
  pp
    {|
    var a = 5

    var b = "st"

    func test() {
        return
    }

    func pritln(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: mismatched type in decl # 1" =
  pp
    {|
    var a = 5

    var b = "st"

    func test() {
        return
    }

    func pritln(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
  |};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: mismatched type in decl # 2" =
  pp
    {|
    var a = "s"

    var b = 5

    func test() {
        return
    }

    func println(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| Typecheck error: Mismatched types: string and int |}]
;;

let%expect_test "err: mismatched type in func_call" =
  pp
    {|
    var a = 5

    var b = 5

    func test() {
        return
    }

    func println(a int) int {
        return a
    }

    func id(a string) string {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + id("st")
        go println(id(10))
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: correct example #3" =
  pp
    {|
    var a = 5

    var b int = 5

    func test() {
        return
    }

    func println(a int) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: return type of func mismatch" =
  pp
    {|
    var a = 5

    var b int = 5

    func test() {
        return
    }

    func println(a string) int {
        return a
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        var c = a + b
        go println(id(10))
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: return with empty func returns" =
  pp {|
    func main() {}

    func foo(a int, b int) {
        return 5
    }
|};
  [%expect {| Typecheck error: Mismatched types: func return types mismatch |}]
;;

let%expect_test "ok: single correct return with no func args" =
  pp {|
    func main() {}

    func foo(a int, b int) {
        return
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: multiple returns type mismatch" =
  pp
    {|
    func main() {}
    func foo(a int, b int) (int, string){
        return 5, 5
    }
|};
  [%expect {| Typecheck error: Mismatched types: string and int |}]
;;

let%expect_test "err: incorrect anon_func return in nested func" =
  pp
    {|
    func s(a string) int { return 1 }

    func main() {
      value := func(a string) {
        g := func(a string) string {
          return s("Test")
        }
        s("Test")
        g("2")
      }
      value("4")
    }
|};
  [%expect {| Typecheck error: Mismatched types: string and int |}]
;;

let%expect_test "err: incorrect anon_func local redeclaration nested func" =
  pp
    {|
    
func s(a string) string { return "g" }

func main() {
	value := func(a int) int{
		g := func(b string) string {
			return a
		}
		s("Test")
		g("2")
    return a
	}
	value(1)
}
|};
  [%expect {| Typecheck error: Mismatched types: string and int |}]
;;

let%expect_test "ok: correct anon_func local redeclaration nested func" =
  pp
    {|
    
func s(a string) string { return "g" }

func main() {
	value := func(a int) int{
		g := func(a string) string {
			return a
		}
		s("Test")
		g("2")
    return a
	}
	value(1)
}
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: incorrect multiple returns in single-value context" =
  pp
    {|

    func main() {
      println(swap(), "j")
    }
    
    func swap() (string, string) {
      return "a", "b"
    }
    func println(a string, b string, c string) (string, string, string) {
      return a, b, c
    }

|};
  [%expect {| Typecheck error: Mismatched types: Expected single type |}]
;;

let%expect_test "ok: correct chans return context" =
  pp
    {|
    func sum(c chan int) {
      c <- 5
    }

    func main() {
      var c chan int
      go sum(c)
      x, y := <-c, <-c 
      x = 5
    }

|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: incorrect chans return context" =
  pp
    {|
    func sum(c chan int) {
      c <- 5
    }

    func main() {
      var c chan int
      go sum(c)
      x, y := <-c, <-c 
      x = "g"
    }

|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: incorrect chan send type" =
  pp
    {|
    func sum(c chan int) {
      c <- "5"
    }

    func main() {
      var c chan int
      go sum(c)
      x, y := <-c, <-c 
      x = 5
    }

|};
  [%expect {| Typecheck error: Mismatched types: string and int |}]
;;

(********** expr **********)

let%expect_test "ok: right types in bin sum" =
  pp {|
    func main() {
        c := 2 + 2
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: mismatched types in bin sum" =
  pp {|
    var a = 5

    var b = "st"

    func main() {
        var c = a + b
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: right type in unary minus" =
  pp {|
    func main() {
        c := -7
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: wrong type in unary minus" =
  pp {|
    var a bool

    func main() {
        c := -a
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and bool |}]
;;

let%expect_test "ok: right types in const array inits" =
  pp {|
  
    func main() {
        a := 7

        c := [2]int{1, a}
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: wrong types in const array inits" =
  pp {|
    func main() {
        c := [2]int{1, func() {}}
    }
|};
  [%expect {| Typecheck error: Mismatched types: func() and int |}]
;;

let%expect_test "ok: too much const array inits" =
  pp {|
    func main() {
        c := [2]string{"", "a", "123"}
    }
|};
  [%expect
    {| Typecheck error: Mismatched types: Array's size less thai it's inits count |}]
;;

let%expect_test "ok: simple array index call" =
  pp
    {|
    var arr = [4]int{}

    func main() {
        c := arr[func() int {return 0}()]
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: array index call with non int index" =
  pp {|
    var arr = [4]int{}

    func main() {
        c := arr["0"]
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: array index assignment" =
  pp {|
    var arr = [4]int{}

    func main() {
        arr[2] = 7
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: array index assignment with non-int index" =
  pp {|
    var arr = [4]int{}

    func main() {
        arr[func() {}] = 7
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and func() |}]
;;

let%expect_test "err: array index assignment with wrong expr" =
  pp {|
    var arr = [4]int{}

    func main() {
        arr[10] = ""
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: multidimensional array index assignment" =
  pp
    {|
    var arr = [4][7]int{}

    func main() {
        i := 3
        j := 2

        arr[i][j] = 10000
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: correct for break" =
  pp
    {|

    func adder() func(int) int {
      sum := 0
      return func(x int) int {
        sum = sum + x
        return sum
      }
    }

    func f(a int) { return }

    func main() {
      pos, neg := adder(), adder()
      for i := 0; i < 10; i++ {
        a := pos(i)
        f(a)
        f(neg(-2 * i))
        break
      }
    }|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: multidimensional array index assignment with wrong index less than \
                 needed"
  =
  pp
    {|
    var arr = [4][7]int{}

    func main() {
        i := 3
        j := 2

        arr[i] = 10000
    }
|};
  [%expect {| Typecheck error: Mismatched types: [7]int and int |}]
;;

let%expect_test "err: multidimensional array index assignment with wrong index more than \
                 it needed"
  =
  pp
    {|
    var arr = [4][7]int{}

    func main() {
        i := 3
        j := 2

        arr[i][i][8] = 10000
    }
|};
  [%expect
    {| Typecheck error: Mismatched types: Number of indicies in array element assigment is incorrect |}]
;;

let%expect_test "ok: multidimensional array index binoper" =
  pp
    {|
    var arr = [4][7]int{}

    func main() {
        i := 3
        j := 2

        i = arr[1][0] + 1
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: multidimensional array index returns array" =
  pp
    {|
    var arr = [4][7]int{}

    func main() {
        i := 3
        j := 2

        i = arr[1] + 1
    }
|};
  [%expect {| Typecheck error: Mismatched types: [7]int and int |}]
;;

let%expect_test "err: multidimensional array index more than it's dimension " =
  pp
    {|
    var arr = [4][7]int{}

    func main() {
        i := 3
        j := 2

        i = arr[1][0][0] + 1
    }
|};
  [%expect {| Typecheck error: Mismatched types: Non-array type in array index call |}]
;;

let%expect_test "ok: multiple nested index inside nested index" =
  pp
    {|
    var arr = [4][7][1][4][5]int{}

    var t1 = [4][3]int{}
    func main() {
        i := 3
        j := 2

        i = arr[1][0][0][t1[2][1]][8] + 1
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: wrong not integer multiple nested index inside nested index" =
  pp
    {|
    var arr = [4][7][1][4][5]int{}

    var t1 = [4][3]string{}
    func main() {
        i := 3
        j := 2

        i = arr[1][0][0][t1[2][1]][8] + 1
    }
|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: predeclared true and false" =
  pp {|
    func main() {
        a := true && false
    }
|};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: correct closure" =
  pp
    {|

    func adder() func(int) int {
      sum := 0
      return func(x int) int {
        sum = sum + x
        return sum
      }
    }

    func f(a int) { return }

    func main() {
      pos, neg := adder(), adder()
      for i := 0; i < 10; i++ {
        a := pos(i)
        f(a)
        f(neg(-2 * i))
      }
    }|};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: mismatched type in closure" =
  pp
    {|

      func adder() func(int) int {
        sum := 0
        return func(x string) int {
          return x
        }
      }

      func f(a int) { return }

      func main() {
        pos, neg := adder(), adder()
        for i := 0; i < 10; i++ {
          a := pos(i)
          f(a)
          f(neg(-2 * i))
        }
      }|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: mismatched type in closure func return" =
  pp
    {|

      func adder() func(int) int {
        sum := 0
        return func(x string) int {
          return sum + 1
        }
      }

      func f(a int) { return }

      func main() {
        pos, neg := adder(), adder()
        for i := 0; i < 10; i++ {
          a := pos(i)
          f(a)
          f(neg(-2 * i))
        }
      }|};
  [%expect {| Typecheck error: Mismatched types: func(int) int and func(string) int |}]
;;

let%expect_test "err: mismatched type inside return of func in closure" =
  pp
    {|

      func adder() func(int) int {
        sum := 0
        return func(x int) int {
          return "t"
        }
      }

      func f(a int) { return }

      func main() {
        pos, neg := adder(), adder()
        for i := 0; i < 10; i++ {
          a := pos(i)
          f(a)
          f(neg(-2 * i))
        }
      }|};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "err: mismatched func returns of created func" =
  pp
    {|

      func adder() func(int) int {
        sum := 0
        return func(x int) int {
          return sum + x
        }
      }

      func f(a string) { return }

      func main() {
        pos, neg := adder(), adder()
        for i := 0; i < 10; i++ {
          a := pos(i)
          f(4)
          f(neg(-2 * i))
        }
      }|};
  [%expect {| Typecheck error: Mismatched types: string and int |}]
;;

let%expect_test "ok: predeclared make, close & print usage" =
  pp
    {|
    func sum(c chan int) {
      sum := 1
      c <- sum
    }

    func main() {
      c := make(chan int)
      sum(c)
      x, y := <-c, <-c
      close(c)
      print(x,y)
    } |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: predeclared panic & recover" =
  pp {|
    func main() {
      defer func() { recover() }()
      panic("")
    } |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: predeclared nil, true, false" =
  pp
    {|
    func main() {
      var a chan int = nil
      var b func() = nil

      cond := true
      if cond {
        cond = false
      }
    }
    |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: untyped nil" =
  pp {|
    func main() {
      a := nil
    }
    |};
  [%expect
    {| Typecheck error: Invalid operation: Cannot assign nil in short var declaration |}]
;;

let%expect_test "ok: incorrect send after make" =
  pp
    {|
    func sum(c chan string) {
      sum := 1
      c <- sum
    }

    func main() {
      c := make(chan string)
      sum(c)
      x, y := <-c, <-c 
      print(x,y)
    } |};
  [%expect {| Typecheck error: Mismatched types: int and string |}]
;;

let%expect_test "ok: redeclaration of predeclared print" =
  pp
    {|
    func print(a int) int {
      return a
    }

    func main() {
      print(5)
    } |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: redeclaration of predeclared make" =
  pp
    {|
    func make(a int) int {
      return a
    }

    func main() {
      make(5)
    } |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: assignment to toplevel function" =
  pp {|
    func foo() {}
    func main() {
      foo = nil
    } |};
  [%expect {| CORRECT |}]
;;

let%expect_test "ok: nil assignment to global variable with a function type" =
  pp {|
    var foo = func() {}
    func main() {
      foo = nil
    } |};
  [%expect {| CORRECT |}]
;;

let%expect_test "err: trying to run make builtin func as a goroutine" =
  pp {|
    func main() {
      go make(chan int)
    } |};
  [%expect {| Typecheck error: Go discards result of make builtin function |}]
;;

let%expect_test "err: break outside for" =
  pp {|
    var foo = func() {}
    func main() {
      break
      foo = nil
    } |};
  [%expect {| Typecheck error: Unexpected operation: break |}]
;;

let%expect_test "err: continue outside for" =
  pp {|
    func main() { continue } |};
  [%expect {| Typecheck error: Unexpected operation: continue |}]
;;

let%expect_test "err: return in unreachable code" =
  pp
    {|
    func foo() {
      for true {}
      return
    }

    func main() { foo() } |};
  [%expect {| CORRECT |}]
;;
