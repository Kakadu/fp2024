foo a = seq (print_int a) (\b -> seq (print_int b) (\c -> print_int c))

main = let () = foo 4 8 9 in 0
