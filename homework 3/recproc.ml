type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int
  | Bool of bool
  | List of value list
  | Procedure of var * exp * env
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * var * exp * env
and env = (var * value) list

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ List.fold_left (fun s x -> s ^ ", " ^ x) "" (List.map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env"))
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
=fun exp env -> 
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR v -> lookup_env v env
  | ADD (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v1, v2 with
    |Int n1, Int n2 -> Int (n1 + n2)
    |_ -> raise UndefinedSemantics)
  | SUB (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v1, v2 with
    |Int n1, Int n2 -> Int (n1 - n2)
    |_ -> raise UndefinedSemantics)
  | MUL (e1, e2) ->(
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v1, v2 with
    |Int n1, Int n2 -> Int (n1 * n2)
    |_ -> raise UndefinedSemantics)
  | DIV (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    if v2 = Int 0 then raise UndefinedSemantics else 
    match v1, v2 with
    |Int n1, Int n2 -> Int ((n1 - n1 mod n2) / n2)
    |_ -> raise UndefinedSemantics)
  | EQUAL (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v1, v2 with
    |Int n1, Int n2 -> if n1 = n2 then Bool true else Bool false
    |Bool b1, Bool b2 -> if b1 = b2 then Bool true else Bool false
    |_ -> raise UndefinedSemantics)
  | LESS (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v1, v2 with
    |Int n1, Int n2 -> if n1 < n2 then Bool true else Bool false
    |_ -> raise UndefinedSemantics)
  | NOT e -> if eval e env = Bool true then Bool false else if eval e env = Bool false then Bool true else raise UndefinedSemantics
  | NIL -> List []
  | CONS (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v2 with
    |List lst -> List ((v1)::lst)
    |_ -> raise UndefinedSemantics)
  | APPEND (e1, e2) -> (
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match v1, v2 with
    |List l1, List l2 -> List (l1@l2)
    |_ -> raise UndefinedSemantics)
  | HEAD e -> (
    match (eval e env) with
    | List l -> (
        match l with 
        |hd::tl -> hd
        |[] -> List [])
    | _ -> raise UndefinedSemantics)
  | TAIL e -> (
    match (eval e env) with
    | List l -> (
        match l with 
        |hd::tl -> List tl
        |[] -> List [])
    |_ -> raise UndefinedSemantics)
  | ISNIL e -> if eval e env = List [] then Bool true else if HEAD e <> NIL then Bool false else raise UndefinedSemantics
  | IF (e1, e2, e3) -> if eval e1 env = Bool true then eval e2 env else eval e3 env
  | LET (v, e1, e2) -> 
    let v1 = eval e1 env in
      eval e2 (extend_env (v, v1) env)
  | LETREC (v1, v2, e1, e2) -> eval e2 (extend_env (v1, RecProcedure (v1, v2, e1, env)) env)
  | LETMREC ((v1, v2, e1), (v3, v4, e2), e3) -> eval e3 (extend_env (v1, MRecProcedure (v1, v3, v2, e1, env)) (extend_env (v3, MRecProcedure(v3, v1, v4, e2, env)) env))
  | PROC (v1, e1) -> Procedure (v1, e1, env)
  | CALL (e1, e2) -> (
    match (eval e1 env) with
    |Procedure (v1, e3, env1) -> eval e3 (extend_env (v1, (eval e2 env)) env1)
    |RecProcedure (v1, v2, e3, env1) -> eval e3 (extend_env (v1, RecProcedure (v1,v2,e3,env1)) (extend_env (v2, eval e2 env) env1))
    |MRecProcedure (v1, v2, v3, e3, env1) -> eval e3 (extend_env (v1, MRecProcedure(v1, v2, v3, e3, env1)) (extend_env (v2, (lookup_env v2 env)) (extend_env (v3, eval e2 env) env1)))
    |_ -> raise UndefinedSemantics)
  | SEQ (e1, e2) -> (eval e1 env); (eval e2 env)

let runml : program -> value
=fun pgm -> eval pgm empty_env
