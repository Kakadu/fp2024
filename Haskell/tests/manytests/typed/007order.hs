_start () () a () b _c () d __ = seq (print_int (a+b)) (seq (print_int __) (a*b `div` _c + d))


main = print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int (-1)) 10000 (-555555))
