open List
open Minijavaast
open Mp6common

(* Helper methods *)
let string_of_type (v:value) : string = match v with
    IntV i -> "Int"
  | StringV s -> "String"
  | BoolV b ->  "Bool"
  | NullV -> "Null"

let rec asgn (id:id) (v:value) (sigma:state) : state =
  match sigma with
     [] -> raise (TypeError ("Assignment to unbound varialbe " ^ id))
  | (id1,v1) :: t -> if id = id1 then (id,v) :: t
                     else (id1,v1) :: asgn id v t

let rec binds (id:id) (sigma:state) : bool =
  match sigma with
    [] -> false
  | (id', _)::t -> id=id' || binds id t

let rec fetch (id:id) (sigma:state) : value =
  match sigma with
    [] -> raise (TypeError ("Unbound variable: "^id))
  | (id', v)::t -> if id=id' then v else fetch id t

let rec mklist (i:int) (v:value) : value list =
       if i=0 then [] else v :: mklist (i-1) v

let rec zip (lis1:id list) (lis2:value list) : state =
  match (lis1, lis2) with
    ([], []) -> [] | (h1::t1, h2::t2) -> (h1,h2) :: zip t1 t2
  | _ -> raise (TypeError ("Mismatched formal and actual param lists"))

let zipscalar (lis:id list) (v:value) : state =
                                zip lis (mklist (length lis) v)

let rec varnames (varlis:var_decl list) : id list =
   match varlis with
     [] -> [] | (Var(_, s))::t -> s :: varnames t

let getMethodInClass (id:id) (Class(_, _, _, methlis)) : method_decl =
  let rec aux methlis = match methlis with
      [] -> raise (TypeError ("No such method: "^id))
    | (Method(_, m, _, _, _, _) as themethod) :: t ->
        if id=m then themethod else aux t
  in aux methlis

let getMethod (id:id) (Program classes) : method_decl =
  getMethodInClass id (hd classes)


(* START HERE *)
let applyOp (bop:binary_operation) (v1:value) (v2:value) : value = 
  match bop with
  | Plus -> (match v1, v2 with
            | IntV(i1), IntV(i2) -> IntV(i1 + i2)
            | StringV(s11), StringV(s22) -> StringV(s11 ^ s22)
            | StringV(s1), IntV(_)
            | StringV(s1), BoolV(_) -> StringV(s1 ^ string_of_value v2)
            | IntV(_), StringV(s1') 
            | BoolV(_),  StringV(s1') -> StringV((string_of_value v1) ^ s1')
            | _ -> raise (TypeError "Incorrect types for the Plus operator "))

  | Minus -> (match v1, v2 with
             | IntV(i1), IntV(i2) -> IntV(i1 - i2)
             | _ -> raise (TypeError "Incorrect types for the Minus operator"))

  | Multiplication -> (match v1, v2 with
                      | IntV(i1), IntV(i2) -> IntV(i1 * i2)
                      | _ -> raise (TypeError "Incorrect types for the Multiplication operator"))

  | Division -> (match v1, v2 with
                | IntV(i1), IntV(i2) -> if i2 = 0 then raise (RuntimeError "DivisionByZero")
                                        else IntV(i1 / i2)
                | _ -> raise (TypeError "Incorrect types for the Division operator"))

  | LessThan -> (match v1, v2 with
                | IntV(i1), IntV(i2) -> BoolV(i1 < i2)
                | _ -> raise(TypeError "Incorrect types for the LessThan operator"))

  | Equal -> (match v1, v2 with
             | IntV(i1), IntV(i2) -> BoolV(i1 = i2)
             | StringV(s1), StringV(s2) -> BoolV(s1 = s2)
             | BoolV(b1), BoolV(b2) -> BoolV(b1 = b2)
             | NullV, NullV -> BoolV(true)
             | NullV, _ | _, NullV -> BoolV(false)
             | _ -> raise(TypeError "Incorrect types for the Equal operator"))

  | _ -> raise (NotImplemented "applyOp")

(* Main interpreter code *)

let rec eval (e:exp) (sigma:state) (prog:program) : value =
   match e with
     | Integer i -> IntV i
     | String s -> StringV s
     | True -> BoolV true
     | False -> BoolV false
     | Null -> NullV
     | Id(i) -> if binds i sigma then fetch i sigma
                else raise(TypeError "No such variable exists")
     | Not(e) -> (match eval e sigma prog with
                 | BoolV(b) -> BoolV(not b)
                 | _ -> raise( TypeError "Incorrect type for not"))
     
     | Operation(e1, Or, e2) -> (match eval e1 sigma prog with
                                | BoolV(b) -> if b = true then BoolV(true)
                                              else eval e2 sigma prog
                                | _ -> raise( TypeError "Incorrect type for Or"))

     | Operation(e1', And, e2') -> (match eval e1' sigma prog with
                                   | BoolV(b) -> if b = false then BoolV(false)
                                                 else eval e2' sigma prog
                                   | _ -> raise( TypeError "Incorrect type for And"))

     | Operation(e11, bop, e22) -> applyOp bop (eval e11 sigma prog) (eval e22 sigma prog)
     | MethodCall(_, id, el) -> let Method(_, _, arglis, locallis, sl, retexp) = getMethod id prog
                                in evalMethodCall sl retexp ((zip (varnames arglis) (evallist el sigma prog))@(zipscalar (varnames locallis) NullV)) prog 
     | _ -> raise (NotImplemented "eval")

and evallist (el:exp list) (sigma:state) (prog:program) : value list =
  match el with 
  | [] -> []
  | h::t -> (eval h sigma prog)::(evallist t sigma prog)

and evalMethodCall (stms:statement list) (retval:exp)
                          (sigma:state) (prog:program) : value =
  eval retval (execstmtlis stms sigma prog) prog

and execstmt (s:statement) (sigma:state) (prog:program) : state =
  match s with
    | Assignment(i, e) -> asgn i (eval e sigma prog) sigma
    | If(e1, s1, s2) -> if (eval e1 sigma prog) = BoolV(true) then execstmt s1 sigma prog
                        else execstmt s2 sigma prog
    | Block(sl) -> execstmtlis sl sigma prog
    | _ -> raise (NotImplemented "execstmt")

and execstmtlis (sl:statement list) (sigma:state) (prog:program) : state =
  match sl with
    | [] -> sigma
    | h::t -> let new_sigma = execstmt h sigma prog
              in execstmtlis t new_sigma prog

(* Run your program with these functions *)
let run_with_args (prog:program) (args:exp list) : string = 
  string_of_value (eval (MethodCall(Null, "main", args)) [] prog)

let run (prog:program) : string = run_with_args prog []

let eval_exp (prog:program) : string =
   let Program [Class(_, _, _, [meth])] = prog
   in let Method(_,_,_,_,_,retval) = meth
      in string_of_value (eval retval [] prog)


