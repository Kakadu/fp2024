let rev_list l =
  let rec helper acc = function
    | [] -> acc
    | hd::tl -> helper (hd::acc) tl
  in 
  helper [] l

rev_list [1;2;3]
