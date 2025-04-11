  $ ../bin/REPL.exe -dinference <<EOF
  > let fac self n = if n<=1 then 1 else n * self (n-1)
  val fac : (int -> int) -> int -> int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let f x y z homka damir chtooo = x y (homka + damir) chtooo
  val f : ('𒀀 -> int -> '𒀐 -> '𒀲) -> '𒀀 -> '𒂷 -> int -> int -> '𒀐 -> '𒀲
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let fac self n = if true then 1 else n * self (n - 1)
  val fac : (int -> int) -> int -> int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let rec homka n = damir 4
  > and damir n = homka 5
  val damir : int -> '𒀀
  val homka : int -> '𒀀
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let rec homka n = damir 4 
  > and damir n = homka 5 in homka
  - : int -> '𒀀

  $ ../bin/REPL.exe -dinference <<EOF
  > let rec x = y + 3 and y = true
  Error: (SomeError
            "This kind of expression is not allowed as right-hand side of 'let rec'")

  $ ../bin/REPL.exe -dinference <<EOF
  > let homka = fun x y -> let z = x y in z + 2
  val homka : ('𒀀 -> int) -> '𒀀 -> int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let foo b = if b then fun foo -> foo + 2 else fun foo -> foo * 10
  > and homka = 122
  > and fac self n = if true then 1 else n * self (n - 1)
  val foo : bool -> int -> int
  val homka : int
  val fac : (int -> int) -> int -> int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let homka x = let y = x + 2 and z = x in z
  val homka : int -> int
  
  $ ../bin/REPL.exe -dinference <<EOF
  > let id x = x in
  > let homka = Some id in
  > match homka with
  > | Some f -> f 42, f "42"
  - : int * string

  $ ../bin/REPL.exe -dinference <<EOF
  > fun id ->
  > let homka = Some id in
  > match homka with
  > | Some f -> f 42, f "42"
  Can't unify (int) and (string)
