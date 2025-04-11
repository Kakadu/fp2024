  $ ../bin/REPL.exe -dinferprogram < manytests/typed/001fac.ml
  val fac : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/002fac.ml
  val fac_cps : int -> (int -> '𒀀) -> '𒀀
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/004manyargs.ml
  val wrap : '𒀀 -> '𒀀
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/005fix.ml
  val fix : (('𒀀 -> '𒀐) -> '𒀀 -> '𒀐) -> '𒀀 -> '𒀐
  val fac : (int -> int) -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/006partial.ml
  val foo : int -> int
  val foo : int -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/008ascription.ml
  val addi : ('𒀀 -> bool -> int) -> ('𒀀 -> bool) -> '𒀀 -> int
  val main : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/009let_poly.ml
  val temp : int * bool
  
  $ cat manytests/typed/010sukharev.ml | ../bin/REPL.exe -dinferprogram
  val _1 : int -> int -> int * '𒀀 -> bool
  val _2 : int
  val _3 : (int * string) option
  val _4 : int -> '𒀀
  val _5 : int
  val _6 : '𒀀 option -> '𒀀
  val int_of_option : int option -> int
  val _42 : int -> bool
  val id1 : '𒀀 -> '𒀀
  val id2 : '𒀀 -> '𒀀
  

  $ ../bin/REPL.exe -dinferprogram < manytests/typed/015tuples.ml
  val fix : (('𒀀 -> '𒀐) -> '𒀀 -> '𒀐) -> '𒀀 -> '𒀐
  val map : ('𒀀 -> '𒀐) -> '𒀀 * '𒀀 -> '𒀐 * '𒀐
  val fixpoly : (('𒀀 -> '𒀐) * ('𒀀 -> '𒀐) -> '𒀀 -> '𒀐) * (('𒀀 -> '𒀐) * ('𒀀 -> '𒀐) -> '𒀀 -> '𒀐) -> ('𒀀 -> '𒀐) * ('𒀀 -> '𒀐)
  val feven : '𒀀 * (int -> int) -> int -> int
  val fodd : (int -> int) * '𒀀 -> int -> int
  val tie : (int -> int) * (int -> int)
  val modd : int -> int
  val meven : int -> int
  val main : int
  
  $ ../bin/REPL.exe -dinferprogram < manytests/typed/016lists.ml
  val length : '𒀀 list -> int
  val length_tail : '𒀀 list -> int
  val map : ('𒀀 -> '𒀐) -> '𒀀 list -> '𒀐 list
  val append : '𒀀 list -> '𒀀 list -> '𒀀 list
  val concat : ('𒀀 list) list -> '𒀀 list
  val iter : ('𒀀 -> unit) -> '𒀀 list -> unit
  val cartesian : '𒀀 list -> '𒀐 list -> ('𒀀 * '𒀐) list
  val main : int
  
  $ ../bin/REPL.exe -dinference <<EOF
  > let f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18
  > m19 m20 m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31 m32 m33 m34 m35 m36
  > m37 m38 m39 m40 m41 m42 m43 m44 m45 m46 m47 m48 m49 m50 m51 m52 m53 m54
  > m55 m56 m57 m58 m59 m60 m61 m62 m63 m64 m65 m66 m67 m68 m69 m70 m71 m72
  > m73 m74 m75 m76 m77 m78 m79 m80 m81 m82 m83 m84 m85 m86 m87 m88 m89 m90
  > m91 m92 m93 m94 m95 m96 m97 m98 m99 m100 = 1
  val f : '𒀀 -> '𒀐 -> '𒀲 -> '𒂷 -> '𒌧 -> '𒀀𒀀 -> '𒀀𒀐 -> '𒀀𒀲 -> '𒀀𒂷 -> '𒀀𒌧 -> '𒀐𒀀 -> '𒀐𒀐 -> '𒀐𒀲 -> '𒀐𒂷 -> '𒀐𒌧 -> '𒀲𒀀 -> '𒀲𒀐 -> '𒀲𒀲 -> '𒀲𒂷 -> '𒀲𒌧 -> '𒂷𒀀 -> '𒂷𒀐 -> '𒂷𒀲 -> '𒂷𒂷 -> '𒂷𒌧 -> '𒌧𒀀 -> '𒌧𒀐 -> '𒌧𒀲 -> '𒌧𒂷 -> '𒌧𒌧 -> '𒀀𒀀𒀀 -> '𒀀𒀀𒀐 -> '𒀀𒀀𒀲 -> '𒀀𒀀𒂷 -> '𒀀𒀀𒌧 -> '𒀀𒀐𒀀 -> '𒀀𒀐𒀐 -> '𒀀𒀐𒀲 -> '𒀀𒀐𒂷 -> '𒀀𒀐𒌧 -> '𒀀𒀲𒀀 -> '𒀀𒀲𒀐 -> '𒀀𒀲𒀲 -> '𒀀𒀲𒂷 -> '𒀀𒀲𒌧 -> '𒀀𒂷𒀀 -> '𒀀𒂷𒀐 -> '𒀀𒂷𒀲 -> '𒀀𒂷𒂷 -> '𒀀𒂷𒌧 -> '𒀀𒌧𒀀 -> '𒀀𒌧𒀐 -> '𒀀𒌧𒀲 -> '𒀀𒌧𒂷 -> '𒀀𒌧𒌧 -> '𒀐𒀀𒀀 -> '𒀐𒀀𒀐 -> '𒀐𒀀𒀲 -> '𒀐𒀀𒂷 -> '𒀐𒀀𒌧 -> '𒀐𒀐𒀀 -> '𒀐𒀐𒀐 -> '𒀐𒀐𒀲 -> '𒀐𒀐𒂷 -> '𒀐𒀐𒌧 -> '𒀐𒀲𒀀 -> '𒀐𒀲𒀐 -> '𒀐𒀲𒀲 -> '𒀐𒀲𒂷 -> '𒀐𒀲𒌧 -> '𒀐𒂷𒀀 -> '𒀐𒂷𒀐 -> '𒀐𒂷𒀲 -> '𒀐𒂷𒂷 -> '𒀐𒂷𒌧 -> '𒀐𒌧𒀀 -> '𒀐𒌧𒀐 -> '𒀐𒌧𒀲 -> '𒀐𒌧𒂷 -> '𒀐𒌧𒌧 -> '𒀲𒀀𒀀 -> '𒀲𒀀𒀐 -> '𒀲𒀀𒀲 -> '𒀲𒀀𒂷 -> '𒀲𒀀𒌧 -> '𒀲𒀐𒀀 -> '𒀲𒀐𒀐 -> '𒀲𒀐𒀲 -> '𒀲𒀐𒂷 -> '𒀲𒀐𒌧 -> '𒀲𒀲𒀀 -> '𒀲𒀲𒀐 -> '𒀲𒀲𒀲 -> '𒀲𒀲𒂷 -> '𒀲𒀲𒌧 -> '𒀲𒂷𒀀 -> '𒀲𒂷𒀐 -> '𒀲𒂷𒀲 -> '𒀲𒂷𒂷 -> '𒀲𒂷𒌧 -> int
  
