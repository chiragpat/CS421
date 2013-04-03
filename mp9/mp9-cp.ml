open Mp9common
open Miniocamlast

let emptyStructuralEnv = []
let emptyFunctionalEnv = envError (* same as fun id -> envError id *)
let emptyEnv = emptyStructuralEnv

(* You may find it helpful to use the following function
   when defining the semantics of Equals *)

let rec zip (l1:'a list) (l2:'b list) : ('a * 'b) list =
  match (l1, l2) with
    ([], []) -> []
  | (x1 :: l1, x2 :: l2) -> (x1, x2) :: zip l1 l2
  | _ -> raise (RuntimeError ("Mismatched lists"))

(* Common between both the interpreter and reducer *)

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

(* Reducer *)

(* subst's signature below is the same as (id:id) (v:value) (exp:exp) -> exp,
   but is a big hint on how to write a helper function inside with the type
   exp -> exp (subst's id and value arguments don't change on recursive
   calls!) *)
let rec subst (id:id) (v:value) : exp -> exp =
  fun (expr:exp) ->
    match expr with
    | Var(i) -> if i = id then v
                else expr
    
    | Let(i, e, e') -> if i = id then Let(i, ((subst id v) e), e')
                       else Let(i, ((subst id v) e), ((subst id v) e'))
    | Fun(i, e) -> if i = id then expr
                   else Fun(i, ((subst id v) e))

    | Rec(i, e) -> if i = id then expr
                   else Rec(i, ((subst id v) e))

    | Operation(e1, bop, e2) -> let e1' = (subst id v) e1
                                in let e2' = (subst id v) e2
                                in Operation(e1', bop, e2')

    | UnaryOperation(uop, e) -> let e' = (subst id v) e
                                in UnaryOperation(uop, e')
    | List(h::t) -> (match ((subst id v) (List (t))) with
                     | List(el) -> List([((subst id v) h)]@el)
                     | _ -> failwith "impossible to get here unless!!")
    
    | Tuple(h::t) -> (match ((subst id v) (Tuple (t))) with
                     | Tuple(el) -> Tuple([((subst id v) h)]@el)
                     | _ -> failwith "impossible to get here unlesst!!")

    | If(e1, e2, e3) -> let e1' = (subst id v) e1
                        in let e2' = (subst id v) e2
                        in let e3' = (subst id v) e3
                        in If(e1', e2', e3')

    | App(e1, e2) -> let e1' = (subst id v) e1
                     in let e2' = (subst id v) e2
                     in App(e1', e2') 
    | _ -> expr

let rec reduce (expr:exp) : exp =
  match expr with
  | Operation(e1, bop, e2) -> applyOp bop (reduce e1) (reduce e2)

  | UnaryOperation(uop, e) -> applyUnop uop (reduce e)

  | List(el) -> evalList el

  | Tuple(el) -> evalTuple el

  | If(e1, e2, e3) -> (match (reduce e1) with
                       | True -> (reduce e2)
                       | False -> (reduce e3)
                       | _ -> raise (RuntimeError "Not boolean in if")) 

  | Let(i, e, e') -> let v = reduce e
                     in reduce ((subst i v) e') 

  | Rec(i, e) -> (match e with
                  | Fun(_,_) -> (subst i expr) e
                  | _ -> (reduce e))

  | App(e, e') -> (match (reduce e) with
                   | Fun(a, e'') -> let v' = (reduce e')
                                    in reduce ((subst a v') e'')
                   | _ -> raise (RuntimeError "Not a function"))

  | Var(i) -> envError i

  | StrConst(_) | IntConst(_) | True | False | Fun(_,_) -> expr
  | _ ->failwith "not implemented"

and evalList (el:exp list): exp =
  match el with
  | [] -> List([])
  | h::t -> match (evalList t) with
            | List(el1) -> List((reduce h)::el1)
            | _ -> failwith "impossible to get hereL"

and evalTuple (el:exp list): exp =
  match el with
  | [] -> Tuple([])
  | h::t -> match (evalTuple t) with
            | Tuple(el1) -> Tuple((reduce h)::el1)
            | _ -> failwith "impossible to get hereT"

(* Interpreter *)

let rec fetch (id:id) (env:environment) : value option =
  failwith "not implemented"

let rec extend (id:id) (v:value) (env:environment) : environment =
  failwith "not implemented"

let rec eval (expr:exp) (env:environment) : exp =
  failwith "not implemented"
