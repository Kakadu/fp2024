  $ ../bin/REPL.exe -dinference <<EOF
  > let fac self n = if n<=1 then 1 else n * self (n-1)
  val fac : (int -> int) -> int -> int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let f x y z homka damir chtooo = x y (homka + damir) chtooo
  val f : ('a -> int -> 'b -> 'c) -> 'a -> 'd -> int -> int -> 'b -> 'c
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let fac self n = if true then 1 else n * self (n - 1)
  val fac : (int -> int) -> int -> int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let rec homka n = damir 4
  > and damir n = homka 5
  val damir : int -> 'a
  val homka : int -> 'a
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let rec homka n = damir 4 
  > and damir n = homka 5 in homka
  - : int -> 'a

  $ ../bin/REPL.exe -dinference <<EOF
  > let rec x = y + 3 and y = true
  val x : bool
  val y : int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let homka = fun x y -> let z = x y in z + 2
  val homka : ('a -> 'b) -> 'a -> int
  

TODO: order of printed names
  $ ../bin/REPL.exe -dinference <<EOF
  > let foo b = if b then fun foo -> foo + 2 else fun foo -> foo * 10
  > and homka = 122
  > and fac self n = if true then 1 else n * self (n - 1)
  val fac : (int -> int) -> int -> int
  val foo : bool -> int -> int
  val homka : int
  

  $ ../bin/REPL.exe -dinference <<EOF
  > let homka x = let y = x + 2 and z = x in z
  val homka : int -> int
  

