wrap f = if 1 == 1 then f else f

test3 a b c = let a = print_int a in let b = print_int b in let c = print_int c in 0

test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

main = let rez = (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000) in let () = print_int rez in let temp2 = wrap test3 1 10 100 in 0