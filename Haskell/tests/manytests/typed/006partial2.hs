foo a b c = let () = print_int a in let () = print_int b in let () = print_int c in a + b * c

main = let foo2 = foo 1 in let foo = foo2 2 in let foo2 = foo 3 in let () = print_int foo2 in 0
