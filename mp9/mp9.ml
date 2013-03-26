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
  failwith "not implemented"

let rec applyUnop (bop:unary_operation) (v:value) : value =
  failwith "not implemented"

(* Reducer *)

(* subst's signature below is the same as (id:id) (v:value) (exp:exp) -> exp,
   but is a big hint on how to write a helper function inside with the type
   exp -> exp (subst's id and value arguments don't change on recursive
   calls!) *)
let rec subst (id:id) (v:value) : exp -> exp =
  failwith "not implemented"

let rec reduce (expr:exp) : exp =
  failwith "not implemented"

(* Interpreter *)

let rec fetch (id:id) (env:environment) : value option =
  failwith "not implemented"

let rec extend (id:id) (v:value) (env:environment) : environment =
  failwith "not implemented"

let rec eval (expr:exp) (env:environment) : exp =
  failwith "not implemented"
