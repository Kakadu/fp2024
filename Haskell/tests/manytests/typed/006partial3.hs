foo a = let () = print_int a in \b -> let () = print_int b in \c -> print_int c

main = let () = foo 4 8 9 in 0
