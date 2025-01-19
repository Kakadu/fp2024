foo a b c = seq (print_int a) (seq (print_int b) (seq (print_int c) (a + b * c)))

main = let foo2 = foo 1 in let foo = foo2 2 in let foo2 = foo 3 in (seq (print_int foo2) 0)
