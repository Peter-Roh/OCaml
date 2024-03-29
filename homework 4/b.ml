type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assgin to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(************************************)
(*      List utility functions      *)
(************************************)
let rec list_length : 'a list -> int
= fun lst ->
  match lst with
  | [] -> 0
  | hd::tl -> 1 + list_length tl

let rec list_exists : ('a -> bool) -> 'a list -> bool
= fun pred lst ->
  match lst with 
  | [] -> false 
  | hd::tl -> if (pred hd) then true else list_exists pred tl

let rec list_fold2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
= fun func acc lst1 lst2 ->
  match (lst1, lst2) with
  | ([], []) -> acc
  | (hd1::tl1, hd2::tl2) -> list_fold2 func (func acc hd1 hd2) tl1 tl2
  | _ -> raise (Failure "list_fold2 : two lists have different length")

let rec list_fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun func acc lst ->
  match lst with
  | [] -> acc
  | hd::tl -> list_fold func (func acc hd) tl 

(********************************)
(*     Handling environment     *)
(********************************)
let rec lookup_loc_env : id -> env -> loc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id, l) -> if (x = id) then l else lookup_loc_env x tl
    | ProcBind _ -> lookup_loc_env x tl
    end

let rec lookup_proc_env : id -> env -> proc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind _ -> lookup_proc_env x tl
    | ProcBind (id, binding) -> if (x = id) then binding else lookup_proc_env x tl
    end

let extend_env : binding -> env -> env
= fun e env -> e::env

let empty_env = []

(***************************)
(*     Handling memory     *)
(***************************)
let rec lookup_mem : loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise (Failure ("location "^(string_of_int l)^" is not included in memory"))
  | (loc, v)::tl -> if (l = loc) then v else lookup_mem l tl

let extend_mem : (loc * value) -> memory -> memory
= fun (l, v) mem -> (l, v)::mem

let empty_mem = []

let size_of_mem mem = 
  let add_if_new x l = if list_exists (fun y -> x = y) l then l else x::l in
  let dom = list_fold (fun dom loc -> add_if_new loc dom) [] mem  in
    list_length dom

(***************************)
(*     Handling record     *)
(***************************)
let rec lookup_record : id -> record -> loc
= fun id record -> 
  match record with
  | [] -> raise(Failure ("field "^ id ^" is not included in record"))
  | (x, l)::tl -> if (id = x) then l else lookup_record id tl


let extend_record : (id * loc) -> record -> record
= fun (x, l) record -> (x, l)::record

let empty_record = []

(******************)
(* Pretty printer *)
(******************)
let rec value2str : value -> string
= fun v ->
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Record r -> "{" ^ record2str r ^ "}" 

and record2str : record -> string
= fun record ->
  match record with
  | [] -> ""
  | [(x, l)] -> x ^ "->" ^ string_of_int l
  | (x, l)::tl-> x ^ "->" ^ string_of_int l ^ ", " ^ record2str tl

let mem2str : memory -> string
= fun mem -> 
  let rec aux mem =
    match mem with
    | [] -> ""
    | [(l, v)] -> string_of_int l ^ "->" ^ value2str v
    | (l, v)::tl -> string_of_int l ^ "->" ^ value2str v ^ ", " ^ aux tl
  in
  "[" ^ aux mem ^ "]"

let rec env2str : env -> string
= fun env -> 
  let rec aux env =
    match env with
    | [] -> ""
    | [binding] -> binding2str binding
    | binding::tl -> binding2str binding ^ ", " ^ aux tl
  in
  "[" ^ aux env ^ "]"

and binding2str : binding -> string
= fun binding ->
  match binding with
  | LocBind (x, l) -> x ^ "->" ^ string_of_int l
  | ProcBind (x, proc) -> x ^ "->" ^ "(" ^ proc2str proc ^ ")"

and proc2str : proc -> string
= fun (xs, e, env) ->  
  let rec args2str xs =
    match xs with
    | [] -> ""
    | [x] -> x
    | x::tl -> x ^ ", " ^ args2str tl
  in
  "(" ^ args2str xs ^ ")" ^ ", E" ^ ", " ^ env2str env

(***************************)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

let rec eval_aop : env -> memory -> exp -> exp -> (int -> int -> int) -> (value * memory)
= fun env mem e1 e2 op ->
  let (v1, mem1) = eval env mem e1 in
  let (v2, mem2) = eval env mem1 e2 in
  match (v1, v2) with
  | (Num n1, Num n2) -> (Num (op n1 n2), mem2)
  | _ -> raise (Failure "arithmetic operation type error")

and eval : env -> memory -> exp -> (value * memory) 
=fun env mem e -> 
  let mem = gc env mem in
  match e with
  | WRITE e -> 
    let (v1, mem1) = eval env mem e in
    let _ = print_endline (value2str v1) in
    (v1, mem1)
  | TRUE -> (Bool true, mem)
  | FALSE -> (Bool false, mem)
  | UNIT -> (Unit, mem)
  | NUM n -> (Num n, mem)
  | VAR id1 -> ((lookup_mem (lookup_loc_env id1 env) mem), mem)
  | ADD (e1, e2) -> eval_aop env mem e1 e2 (fun e1 e2 -> e1 + e2)
  | SUB (e1, e2) -> eval_aop env mem e1 e2 (fun e1 e2 -> e1 - e2)
  | MUL (e1, e2) -> eval_aop env mem e1 e2 (fun e1 e2 -> e1 * e2)
  | DIV (e1, e2) -> eval_aop env mem e1 e2 (fun e1 e2 -> e1 / e2)
  | EQUAL (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in
    let (v2, mem2) = eval env mem1 e2 in
    (match v1, v2 with
    | Num n1, Num n2 -> if n1 = n2 then (Bool true, mem2) else (Bool false, mem2)
    | Bool b1, Bool b2 -> if b1 = b2 then (Bool true, mem2) else (Bool false, mem2)
    | Unit, Unit -> (Bool true, mem2)
    | _ -> (Bool false, mem2))
  | LESS (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in
    let (v2, mem2) = eval env mem1 e2 in
    (match v1, v2 with
    | Num n1, Num n2 -> if n1 < n2 then (Bool true, mem2) else (Bool false, mem2)
    | _ -> raise UndefinedSemantics)
  | NOT e -> 
    let (v1, mem1) = eval env mem e in
    (match v1 with
    | Bool true -> (Bool false, mem1)
    | Bool false -> (Bool true, mem1)
    | _ -> raise UndefinedSemantics)
  | SEQ (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in
    let (v2, mem2) = eval env mem1 e2 in
    (v2, mem2)
  | IF (e1, e2, e3) -> 
    let (v1, mem1) = eval env mem e1 in
    if v1 = (Bool true) then 
    let (v2, mem2) = eval env mem1 e2 in (v2, mem2)
    else if v1 = (Bool false) then let (v3, mem2) = eval env mem1 e3 in (v3, mem2)
    else raise UndefinedSemantics
  | WHILE (e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in 
    (match v1 with
    | Bool true -> 
      (let (v2, mem2) = eval env mem1 e2 in 
      let (v3, mem3) = eval env mem2 (WHILE (e1, e2)) in (v3, mem3))
    | Bool false -> (Unit, mem1)
    | _ -> raise UndefinedSemantics)
  | LETV (id1, e1, e2) -> 
    let (v1, mem1) = eval env mem e1 in 
    let l = new_location () in 
    let (v2, mem2) = eval (extend_env (LocBind(id1, l)) env) (extend_mem (l, v1) mem) e2 in
    (v2, mem2)
  | LETF (id1, idlst, e1, e2) -> 
  let (v1, mem1) = eval (extend_env (ProcBind(id1, (idlst, e1, env))) env) mem e2 in
  (v1, mem1)
  | CALLV (id1, exlst) -> 

  | CALLR (id1, idlst) -> 

  | RECORD lst -> 
    (match lst with
    |(id1, e1)::tl -> 
      let (v1, mem1) = eval env mem e1 in 
      let l = new_location () in 
      eval (extend_env (LocBind (id1, l)) env) (extend_mem (l, v1) mem1) (RECORD tl)
    |_ -> (Unit, mem))
  | FIELD (e1, id1) -> 
    let (r1, mem1) = eval env mem e1 in 
    (match r1 with 
    | Record r2 -> ((lookup_mem (lookup_record id1 r2) mem1), mem1)
    | _ -> raise UndefinedSemantics)
  | ASSIGN (id1, e1) -> 
    let (v1, mem1) = eval env mem e1 in 
    (v1, extend_mem ((lookup_loc_env id1 env), v1) mem1)
  | ASSIGNF (e1, id1, e2) -> 
    let (r1, mem1) = eval env mem e1 in 
    let (v1, mem2) = eval env mem1 e2 in 
    (match r1 with 
    | Record r2 -> (v1, (extend_mem ((lookup_record id1 r2), v1) mem2))
    | _ -> raise UndefinedSemantics)
  | _ -> raise NotImplemented


and gc : env -> memory -> memory
= fun env mem -> mem (* TODO *)

let runb : exp -> value 
= fun exp ->
  let (v, m) = eval empty_env empty_mem exp in
  let _ = print_endline ("memory size: " ^ string_of_int (size_of_mem m)) in
    v;;
