let rec max : int list -> int
= fun lst -> 
match lst with
  |[] -> 0
  |hd::[] -> hd
  |hd::tl -> let temp = (max tl) in
    if hd < temp then temp else hd

let rec min : int list -> int
= fun lst -> 
match lst with
  |[] -> 0
  |hd::[] -> hd
  |hd::tl -> let temp = (min tl) in
    if hd < temp then hd else temp;;
