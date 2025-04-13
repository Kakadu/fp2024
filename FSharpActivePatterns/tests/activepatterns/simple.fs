let (|MinusTwo|Not|) v =
  if v+2 = 0 then MinusTwo (v+10)
  else Not (v)
  
let res = match 1 with
  | MinusTwo val -> val
  | Not val -> val
  in print_int res
