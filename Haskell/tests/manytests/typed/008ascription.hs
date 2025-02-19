addi = \f g x -> (f x (g x:: Bool) :: Int)

main = seq (print_int (addi (\x b -> if b then x+1 else x*2) (\ _start -> _start `div` 2 == 0) 4)) 0
