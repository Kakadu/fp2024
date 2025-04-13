let (|A|B|C|D|) (a,b) =
  if a > 0 then (if b > 0 then A a else B a ) else (if b > 0 then C b else D)

let res = match (-3, 2) with
  | A x -> x
  | B x -> x * 10
  | C b -> b * 100
  | D -> 1
  in print_int res
