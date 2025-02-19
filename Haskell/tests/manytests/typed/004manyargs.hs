wrap f = if 1 == 1 then f else f

test3 a b c = seq (print_int a) (seq (print_int b) (seq (print_int c) 0))

test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

main = let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in seq (print_int rez) (let temp2 = wrap test3 1 10 100 in 0)