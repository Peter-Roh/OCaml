let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f xs ys c -> 
match xs, ys with
  |xhd::[], yhd::[] -> f xhd yhd c
  |xhd::xtl, yhd::ytl -> reduce f xtl ytl (f xhd yhd c)
  |_ -> raise (Failure "Error");;
