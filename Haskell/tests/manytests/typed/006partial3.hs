foo a = seq (print_int a) (\b -> seq (print_int b) (\c -> print_int c))

main = foo 4 8 9
