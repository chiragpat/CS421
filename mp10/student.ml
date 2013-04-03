open Mp10common
open Miniocamlast

let rec applyOp (bop:binary_operation) (v1:value) (v2:value) : value =
  match bop with
  | Equals -> (match v1, v2 with
               | IntConst(i1), IntConst(i2) -> if i1 = i2 then True else False
               | StrConst(s1), StrConst(s2) -> if s1 = s2 then True else False
               | True, True -> True
               | False, False -> True
               | List(el1), List(el2) | Tuple(el1), Tuple(el2) -> if el1 = el2 then True else False
               | _ -> False)
  
  | NotEquals -> (match (applyOp Equals v1 v2) with
                 | True -> False
                 | False -> True
                 | _ -> failwith "its impossible to get hereE")

  | LessThan -> (match v1, v2 with
                 | IntConst(i1), IntConst(i2) -> if i1 < i2 then True else False
                 | _ -> raise (RuntimeError "Wrong types for LessThan"))

  | GreaterThan -> (match v1, v2 with
                    | IntConst(i1), IntConst(i2) -> if i1 > i2 then True else False
                    | _ -> raise (RuntimeError "Wrong types for GreaterThan"))

  | And -> (match v1, v2 with
            | True, True -> True
            | True, False | False, True | False, False -> False
            | _ -> raise (RuntimeError "Wrong types for And"))

  | Or -> (match v1, v2 with
            | True, True | True, False | False, True -> True
            | False, False -> False
            | _ -> raise (RuntimeError "Wrong types for Or"))

  | Plus -> (match v1, v2 with
             | IntConst(i1), IntConst(i2) -> IntConst(i1+i2)
             | _ -> raise (RuntimeError "Wrong types for Plus"))

  | Minus -> (match v1, v2 with
             | IntConst(i1), IntConst(i2) -> IntConst(i1-i2)
             | _ -> raise (RuntimeError "Wrong types for Minus"))

  | Mult -> (match v1, v2 with
             | IntConst(i1), IntConst(i2) -> IntConst(i1*i2)
             | _ -> raise (RuntimeError "Wrong types for Mult"))

  | Div -> (match v1, v2 with
             | IntConst(i1), IntConst(i2) -> if i2 != 0 then IntConst(i1/i2)
                                             else raise (RuntimeError "Division by zero")
             | _ -> raise (RuntimeError "Wrong types for Div"))

  | StringAppend -> (match v1, v2 with
                     | StrConst(s1), StrConst(s2) -> StrConst(s1 ^ s2)
                     | _ -> raise (RuntimeError "Wrong types for StringAppend"))

  | ListAppend -> (match v1, v2 with
                   | List(el1), List(el2) -> List(el1@el2)
                   | _ -> raise (RuntimeError "Wrong types for ListAppend"))

  | Cons -> (match v1, v2 with
             | _, List(el) -> List(v1::el)
             | _ -> raise (RuntimeError "Wrong types for Cons"))

  | _ -> failwith "not implemented"

let rec applyUnop (bop:unary_operation) (v:value) : value =
  match bop with
  | Not -> (match v with
            | True -> False
            | False -> True
            | _ -> raise (RuntimeError "Wrong types for Not"))

  | Head -> (match v with
             | List([]) -> raise (RuntimeError "Head of empty list.")
             | List(h::t) -> h
             | _ -> raise (RuntimeError "Wrong types for Head"))

  | Tail -> (match v with
             | List([]) -> raise (RuntimeError "Tail of empty list.")
             | List(h::t) -> List(t)
             | _ -> raise (RuntimeError "Wrong types for Tail"))

  | Fst -> (match v with
            | Tuple([v1; v2]) -> v1
            | _ -> raise (RuntimeError "Bad argument to Fst"))

  | Snd -> (match v with
            | Tuple([v1; v2]) -> v2
            | _ -> raise (RuntimeError "Bad argument to Snd"))

  | _ -> failwith "not implemented"

let rec fetch (id:id) (env:environment) : value =
  match env with
  | [] -> envError id
  | (i, v)::t -> if i = id then v
                 else fetch id t

let rec extend (id:id) (v:value) (env:environment) : environment =
  match env with
  | [] -> [(id, v)]
  | (i, v1)::t -> if i = id then (id, v)::t
                  else (i, v1)::(extend id v t)

let rec eval (expr:exp) (env:environment) : exp =
  match expr with
  | Var(id) -> fetch id env
  
  | Fun(_, _) | Rec(_, _) -> Closure(expr, env)

  | Operation(e1, bop, e2) -> applyOp bop (eval e1 env) (eval e2 env)

  | UnaryOperation(uop, e) -> applyUnop uop (eval e env)

  | List(el) -> List(evalList el env)
  | Tuple(el) -> Tuple(evalList el env)

  | If(e1, e2, e3) -> (match (eval e1 env) with
                       | True -> eval e2 env
                       | False -> eval e3 env
                       | _ -> raise (TypeError "Wrong type in If"))

  | Let(a, e, e') -> let env' = extend a (eval e env) env
                     in eval e' env'

  | App(e, e') -> (match (eval e env) with
                   | Closure(Fun(a, e''), env') -> let v' = eval e' env
                                                   in let env'' = extend a v' env'
                                                   in eval e'' env''

                   | Closure(Rec(f, Fun(a, e'')), env') -> let v' = eval e' env
                                                           in let env'' = extend f (Closure(Rec(f, Fun(a, e'')), env')) 
                                                                                   (extend a v' env')
                                                           in eval e'' env''
                   | _ -> raise (TypeError "incorrect type for fun application "))
  
  | IntConst(_) | StrConst(_) | True | False -> expr 

and evalList (el:exp list) (env:environment) : exp list = 
  match el with
  | [] -> []
  | h::t -> (eval h env)::(evalList t env)
