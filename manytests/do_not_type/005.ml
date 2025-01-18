let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1