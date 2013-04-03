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
  | _ -> raise (TypeError ("Mismatched lists"))

(* Common between both the interpreter and reducer *)

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
    | _ -> raise(TypeError "Can't compare these valuess"))

   | LessThan -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> exp_of_bool (i < i2)    
      | _ -> raise(TypeError "Can't compare values"))

   | GreaterThan -> applyOp LessThan v2 v1

   | NotEquals -> (match (applyOp Equals v1 v2) with
      True -> False
    | False -> True
    | _ -> raise(TypeError "Can't compare values"))

   | Assign -> failwith "not implemented"
 
   | And -> (match v1, v2 with
      True, True -> True
    | True, False -> False
    | False, True -> False
    | False, False -> False
    | _ -> raise(TypeError "not booleans!"))

   | Or -> (match v1, v2 with
      True, True -> True
    | True, False -> True
    | False, True -> True
    | False, False -> False
    | _ -> raise(TypeError "not booleans!"))

   | Plus -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> IntConst(i + i2)
    | _ -> raise(TypeError "not ints!"))

   | Minus -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> IntConst(i - i2)
    | _ -> raise(TypeError "not ints!"))

   | Div -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> if i2 = 0 then raise(RuntimeError "Can't divide by 0") 
                                   else IntConst(i / i2)
    | _ -> raise(TypeError "not ints!"))

   | Mult -> (match v1, v2 with
      IntConst(i), IntConst(i2) -> IntConst(i * i2)
    | _ -> raise(TypeError "not ints!"))

   | StringAppend -> (match v1, v2 with
      StrConst(s), StrConst(s2) -> StrConst(s ^ s2)
    | _ -> raise(TypeError "can't append")) 

   | ListAppend -> (match v1, v2 with
      List(el), List(el2) -> List(el@el2)
    | _ -> raise(TypeError "not lists"))

   | Cons -> (match v2 with
      List(l) -> List(v1::l)
      | _ -> raise(TypeError "no."))

let rec applyUnop (bop:unary_operation) (v:value) : value =
  match bop with
     
     Not -> (match v with
        True -> False
      | False -> True
      | _ -> raise(TypeError "no."))

   | Head -> (match v with
      List(h::t) -> h
    | _ -> raise(TypeError "no."))

   | Tail -> (match v with
      List(h::t) -> List(t)
    | _ -> raise(TypeError "no."))

   | Fst -> (match v with
      Tuple([v1; v2]) -> v1
    | _ -> raise(TypeError "no."))

   | Snd -> (match v with
      Tuple([v1; v2]) -> v2
    | _ -> raise(TypeError "no."))

(* Reducer *)

(* subst's signature below is the same as (id:id) (v:value) (exp:exp) -> exp,
   but is a big hint on how to write a helper function inside with the type
   exp -> exp (subst's id and value arguments don't change on recursive
   calls!) *)
let rec subst (id:id) (v:value) : exp -> exp =
  fun (expr:exp) -> 
    match expr with
       Var(str) -> if str = id then v else Var(str)

     | Let(str, e1, e2) -> if str = id then Let(str, ((subst id v) e1), e2)
        else Let(str, ((subst id v) e1), ((subst id v) e2))

     | Fun(str, e) -> if str = id then Fun(str, e) else Fun(str, ((subst id v) e))

     | Rec(str, e) -> if str = id then Rec(str, e) else Rec(str, ((subst id v) e))

     | Operation(e1, bop, e2) -> let e3 = (subst id v) e1 in
        let e4 = (subst id v) e2 in
        Operation(e3, bop, e4)

     | UnaryOperation(op, e1) -> let e2 = (subst id v) e1 in 
        UnaryOperation(op, e2)

     | StrConst(str) -> StrConst(str)

     | IntConst(i) -> IntConst(i)

     | True -> True

     | False -> False

     | List(expl) -> List(substList expl id v)

     | Tuple(expl) -> Tuple(substList expl id v)

     | If(e1, e2, e3) -> let e4 = (subst id v) e1 in
        let e5 = (subst id v) e2 in
        let e6 = (subst id v) e3 in
        If(e4, e5, e6)

     | App(e1, e2) -> let e3 = (subst id v) e1 in
        let e4 = (subst id v) e2 in
        App(e3, e4)

and substList expl id v = match expl with
   [] -> []
 | h::t -> [(subst id v) h]@substList t id v

let rec reduce (expr:exp) : exp =
  match expr with 
     Operation(e, bop, e2) -> applyOp bop (reduce e) (reduce e2)
 
   | UnaryOperation(op, e) -> applyUnop op (reduce e)
   | Var(str) -> raise(TypeError "variable not found")

   | StrConst(str) -> StrConst(str)

   | IntConst(i) -> IntConst(i)

   | True -> True

   | False -> False

   | List(expl) -> List(reduceList expl)

   | Tuple(expl) -> Tuple(reduceList expl)

   | If(e, e1, e2) -> (match (reduce e) with
      True -> reduce e1
    | False -> reduce e2
    | _ -> raise(TypeError "If clause"))

   | App(e, e1) -> (let v2 = reduce e1 in
      match (reduce e) with
         Fun(str, e2) -> reduce ((subst str v2) e2)
       | _ -> raise(TypeError "App clause"))

   | Let(str, e, e1) -> (let v1 = reduce e in
      reduce ((subst str v1) e1))

   | Fun(str, e) -> Fun(str, e)

   | Rec(str, e) -> (match e with
      Fun(a, e1) -> (subst str (Rec(str, e))) e
    | _ -> reduce e)

and reduceList expl = match expl with
   [] -> []
 | h::t -> [reduce h]@reduceList t


(* Interpreter *)

let rec fetch (id:id) (env:environment) : value option =
  failwith "not implemented"

let rec extend (id:id) (v:value) (env:environment) : environment =
  failwith "not implemented"

let rec eval (expr:exp) (env:environment) : exp =
  failwith "not implemented"