let smallest_divisor : int -> int
= fun n -> 
let rec div n i = 
  if i * i > n  then n else
    if n mod i = 0 then i else div n (i + 1) in
div n 2;;
