type graph = (vertex * vertex) list
and vertex = int

let rec reach : graph * vertex -> vertex list
= fun (g, v) -> 
sort(traverse g [] [v])

and traverse g l1 l2 = 
  match l2 with
    |[] -> []
    |hd::tl -> if searchVertex g [] hd = [] then hd::traverse g (hd::l1) tl else if find l1 hd = true then traverse g l1 tl else hd::(traverse g (hd::l1) (tl@(searchVertex g [] hd)))
    
and find l a = 
  match l with
    |[] -> false
    |hd::tl -> if hd = a then true else find tl a
    
and searchVertex g lst v = 
  match g with 
    |[] -> lst
    |(v1, v2)::tl -> if v1 = v then (searchVertex tl (v2::lst) v) else (searchVertex tl lst v)
    
and sort l = 
  match l with
    |[] -> []
    |hd::tl -> insert hd (sort tl)
    
and insert a l = 
  match l with 
    |[] -> [a]
    |hd::tl -> if a < hd then a::l else hd::(insert a tl);;
