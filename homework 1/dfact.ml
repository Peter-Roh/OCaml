let rec dfact : int -> int
= fun n -> 
match n mod 2 with 
  |0 -> if n = 2 then 2 else n * dfact (n - 2)
  |_ -> if n = 1 then 1 else n * dfact (n - 2);;
