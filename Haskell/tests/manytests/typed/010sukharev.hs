_1 = \x y (a, _) -> (x + y - a) == 1
_2 = let (x, Just f) = (1, Just ( ( + ) 4 )) in f x

_3 = Just (1, True)

_4 = let (a, _, _) = (1, 2, 3) in a

int_of_option (Just x) = x 
int_of_option Nothing = 0

_5 = let f x = f 5 in f

_42  42 = True
_42 _  = False
