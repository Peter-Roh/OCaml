type mobile = branch * branch (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
match m with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if (findL (SimpleBranch(l1, w1))) * (findW (SimpleBranch(l1, w1))) = (findL (SimpleBranch(l2, w2))) * (findW (SimpleBranch(l2, w2))) then true else false
  |(SimpleBranch(l1, w1), CompoundBranch (l2, m2)) -> if (findL ((SimpleBranch(l1, w1))) * (findW (SimpleBranch(l1, w1))) = (findL (CompoundBranch (l2, m2))) * (findW (CompoundBranch (l2, m2)))) && balanced m2 then true else false
  |(CompoundBranch(l1, m1), SimpleBranch (l2, w2)) -> if (findL ((CompoundBranch(l1, m1))) * (findW (CompoundBranch(l1, m1))) = (findL (SimpleBranch (l2, w2))) * (findW (SimpleBranch (l2, w2)))) && balanced m1 then true else false
  |(CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if (findL ((CompoundBranch(l1, m1))) * (findW (CompoundBranch(l1, m1))) = (findL (CompoundBranch(l2, m2))) * (findW (CompoundBranch(l2, m2)))) && balanced m1 && balanced m2 then true else false

and findL b = 
  match b with
    |SimpleBranch(l, w) -> l
    |CompoundBranch(l, m') -> l

and findW b = 
  match b with
    |SimpleBranch(l, w) -> w
    |CompoundBranch(l, m') -> 
      match m' with
        |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1 + w2
        |(SimpleBranch(l1, w1), CompoundBranch(l2, m2)) -> w1 + findW (CompoundBranch(l2, m2))
        |(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> findW (CompoundBranch(l1, m1)) + w2
        |(CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> findW (CompoundBranch(l1, m1)) + findW (CompoundBranch(l2, m2));;
