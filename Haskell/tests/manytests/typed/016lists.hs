length xs = case xs of [] -> 0; h:tl -> 1 + length tl

length_tail = let helper acc xs = case xs of [] -> acc; h:tl -> helper (acc + 1) tl in helper 0

map f xs = case xs of [] -> []; a:[] -> [f a]; a:b:[] -> [f a, f b]; a:b:c:[] -> [f a, f b, f c]; a:b:c:d:tl -> f a : f b : f c : f d : map f tl

append xs ys = case xs of [] -> ys; x:xs -> x:(append xs ys)

concat = let helper xs = case xs of [] -> []; h:tl -> append h (helper tl) in helper

iter f xs = case xs of [] -> (); h:tl -> seq (f h) (iter f tl)

cartesian xs ys = case xs of [] -> []; h:tl -> append (map (\a -> (h,a)) ys) (cartesian tl ys)

main = seq (iter print_int [1,2,3]) (seq (print_int (length (cartesian [1,2] [1,2,3,4]))) 0)
