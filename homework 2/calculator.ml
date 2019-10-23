type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
funcexp exp 0

and funcexp exp = 
  match exp with 
    |X -> (fun n -> n)
    |INT n -> (fun _ -> n)
    |ADD(e1, e2) -> (fun a -> ((funcexp e1) a) + ((funcexp e2) a))
    |SUB(e1, e2) -> (fun a -> ((funcexp e1) a) - ((funcexp e2) a))
    |MUL(e1, e2) -> (fun a -> ((funcexp e1) a) * ((funcexp e2) a))
    |DIV(e1, e2) -> (fun a -> (((funcexp e1) a) - ((funcexp e1) a) mod ((funcexp e2) a)) / ((funcexp e2) a))
    |SIGMA(e1, e2, e3) -> 
      let f1 = funcexp e1 in
      let f2 = funcexp e2 in
      let fx = funcexp e3 in
      (fun a -> sigma fx (f1 a) (f2 a))

and sigma f a b = 
  if a = b then f a else (f a) + sigma f (a + 1) b;;
