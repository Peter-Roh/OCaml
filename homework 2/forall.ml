let rec forall : ('a -> bool) -> 'a list -> bool
= fun f lst -> 
match lst with
  |[] -> true
  |hd::tl -> if f hd = true then forall f tl else false;;
