open Mp10common
open Miniocamlast

let rec applyOp (bop:binary_operation) (v1:value) (v2:value) : value =
  match bop with
     Semicolon -> failwith "not implemented"

   | Comma -> failwith "not implemented"

   | Equals -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> if i = i2 then True else False
    | StrConst(s), StrConst(s2) -> if v1 = v2 then True else False
    | True, True ->  True
    | False, False -> True
    | True, False -> False
    | False, True -> False
    | List(expl), List(expl2) -> if expl = expl2 then True else False
    | Tuple(expl), Tuple(expl2) -> if expl = expl2 then True else False
    | _ -> raise(RuntimeError "Can't compare these valuess"))

   | LessThan -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> exp_of_bool (i < i2)    
      | _ -> raise(RuntimeError "Can't compare values"))

   | GreaterThan -> applyOp LessThan v2 v1

   | NotEquals -> (match (applyOp Equals v1 v2) with
      True -> False
    | False -> True
    | _ -> raise(RuntimeError "Can't compare values"))

   | Assign -> failwith "not implemented"
 
   | And -> (match v1, v2 with
      True, True -> True
    | True, False -> False
    | False, True -> False
    | False, False -> False
    | _ -> raise(RuntimeError "not booleans!"))

   | Or -> (match v1, v2 with
      True, True -> True
    | True, False -> True
    | False, True -> True
    | False, False -> False
    | _ -> raise(RuntimeError "not booleans!"))

   | Plus -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> IntConst(i + i2)
    | _ -> raise(RuntimeError "not ints!"))

   | Minus -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> IntConst(i - i2)
    | _ -> raise(RuntimeError "not ints!"))

   | Div -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> if i2 = 0 then raise(RuntimeError "Can't divide by 0") 
                                   else IntConst(i / i2)
    | _ -> raise(RuntimeError "not ints!"))

   | Mult -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> IntConst(i * i2)
    | _ -> raise(RuntimeError "not ints!"))

   | StringAppend -> (match v1, v2 with
      StrConst(s), StrConst(s2) -> StrConst(s ^ s2)
    | _ -> raise(RuntimeError "can't append")) 

   | ListAppend -> (match v1, v2 with
      List(el), List(el2) -> List(el@el2)
    | _ -> raise(RuntimeError "not lists"))

   | Cons -> (match v2 with
      List(l) -> List(v1::l)
      | _ -> raise(RuntimeError "no."))

let rec applyUnop (bop:unary_operation) (v:value) : value =
   match bop with
     
     Not -> (match v with
        True -> False
      | False -> True
      | _ -> raise(RuntimeError "not."))

   | Head -> (match v with
      List(h::t) -> h
    | _ -> raise(RuntimeError "hd."))

   | Tail -> (match v with
      List(h::t) -> List(t)
    | _ -> raise(RuntimeError "tl."))

   | Fst -> (match v with
      Tuple([v1; v2]) -> v1
    | _ -> raise(RuntimeError (string_of_expr v)))

   | Snd -> (match v with
      Tuple([v1; v2]) -> v2
    | _ -> raise(RuntimeError "snd."))

let rec fetch (id:id) (env:environment) : value =
    match env with
      [] -> envError id
    | (s, v)::t -> if s = id then v else fetch id t

let rec extend (id:id) (v:value) (env:environment) : environment =
  match env with
     [] -> [(id, v)]@env
   | (s, v1)::t -> if s = id then [(s, v)]@t else [(s, v1)]@extend id v t

let rec eval (expr:exp) (env:environment) : exp =
  match expr,env with 
     (Operation(e1, bop, e2), env1) -> applyOp bop (eval e1 env1) (eval e2 env1)

   | (UnaryOperation(op, e1), env1) -> applyUnop op (eval e1 env1)

   | (Var(s), env1) -> fetch s env1

   | (StrConst(s), env1) -> StrConst(s)

   | (IntConst(i), env1) -> IntConst(i)

   | (True, env1) -> True

   | (False, env1) -> False

   | (List(expl), env1) -> List(evalList expl env1)

   | (Tuple(expl), env1) -> Tuple(evalList expl env1)

   | (If(e1, e2, e3), env1) -> (match eval e1 env1 with
      True -> (eval e2 env1)
    | False -> (eval e3 env1)
    | _ -> raise(RuntimeError "not boolean"))

   | (App(e1, e2), env1) -> (let v1 = (eval e2 env1) in
      match (eval e1 env1) with
         Closure(Fun(a, e3), env2) -> eval e3 (extend a v1 env2)
       | Closure(Rec(f, Fun(a,e3)), env2) -> eval e3 (extend f (eval e1 env1) (extend a v1 env2))
       | _ -> raise(RuntimeError "not a function"))

   | (Let(s, e1, e2), env1) -> (let v1 = (eval e1 env1) in
      eval e2 (extend s v1 env1)
      )

   | (Fun(s, e1), env1) -> Closure(Fun(s,e1), env1)

   | (Rec(s, e1), env1) -> Closure(Rec(s,e1), env1)
   
   | Closure(e1, env), env1 -> raise(RuntimeError "no closures!")

and evalList expl env = match expl with
   [] -> []
 | h::t -> [eval h env]@evalList t env