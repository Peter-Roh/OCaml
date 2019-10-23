type btree =
  | Empty
  | Node of (int * btree * btree)
;;

let rec mem : int -> btree -> bool
= fun n t -> 
match t with
  |Empty -> false
  |Node (k, t_1, t_2) -> if k = n then true else (mem n t_1) || (mem n t_2);;
