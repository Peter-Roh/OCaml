type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
match exp with
  |Const _ -> Const 0
  |Var y -> if x = y then Const 1 else Var y
  |Power (s, i) -> if s = x then Times [Const i; Power (s, (i - 1))] else Power (s, i)
  |Times explst -> (match explst with 
    |[] -> Const 0
    |hd::tl -> Sum [Times [diff (hd, "x"); Times tl]; Times [hd; diff(Times tl, "x")]])
  |Sum explst -> (match explst with
    |[] -> Const 0
    |hd::tl -> Sum[diff(hd, "x"); diff(Sum tl, "x")]);;
