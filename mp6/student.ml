open List
open Minijavaast
open Mp6common

(* Helper methods *)

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

let applyOp (bop:binary_operation) (v1:value) (v2:value) : value = match bop with
   Plus -> (match v1, v2 with
                    IntV(i), IntV(i2) -> IntV(i + i2)
                  | StringV(s), StringV(s2) -> StringV(s ^ s2)
                  | IntV(i), StringV(s) -> StringV(string_of_int(i) ^ s)
                  | StringV(s), IntV(i) -> StringV(s ^ string_of_int(i))
                  | BoolV(b), StringV(s) -> StringV(string_of_bool(b) ^ s)
                  | StringV(s), BoolV(b) -> StringV(s ^ string_of_bool(b))
                  | _ -> raise(TypeError "Can't add these values")
                  ) 

 | Minus -> (match v1, v2 with
              IntV(i), IntV(i2) -> IntV(i - i2)
            | _ -> raise(TypeError "Can't subtract these values"))
 
 | Multiplication -> (match v1, v2 with
                       IntV(i), IntV(i2) -> IntV(i * i2)
                     | _ -> raise(TypeError "Can't multiply these values"))

 | Division -> (match v1, v2 with
                  IntV(i), IntV(i2) -> IntV(i / i2)
                | _ -> raise(TypeError "Can't divide these values"))

 | LessThan -> (match v1, v2 with
                 IntV(i), IntV(i2) -> BoolV(i < i2)
               | _ -> raise(TypeError "Can't compare these values"))

 | Equal -> (match v1, v2 with
              IntV(i), IntV(i2) -> BoolV(i = i2)
            | StringV(s), StringV(s2) -> BoolV(s = s2)
            | BoolV(b), BoolV(b2) -> BoolV(b = b2)
            | NullV, NullV -> BoolV(true)
            | NullV, _ -> BoolV(false)
            | _, NullV -> BoolV(false)
            | _ -> raise(TypeError "Can't compare these values"))
 | _ -> raise(NotImplemented "applyOp")

 (* type checking*)
 
(* Main interpreter code *)

let rec eval (e:exp) (sigma:state) (prog:program) : value =
   match e with
       Integer i -> IntV i
     | Operation(e1, Or, e2) -> (match (eval e1 sigma prog) with
         BoolV(b) -> if b then BoolV(true) else (eval e2 sigma prog)
       | _ -> raise(TypeError "Wrong type!"))
     | Operation(e1, And, e2) -> (match (eval e1 sigma prog) with
         BoolV(b) -> if b = false then BoolV(false) else (eval e2 sigma prog)
        | _ -> raise(TypeError "Wrong type!"))
     | Operation(e1, bop, e2) -> applyOp bop (eval e1 sigma prog) (eval e2 sigma prog)
     | True -> BoolV(true)
     | False -> BoolV(false)
     | Id(i) -> fetch i sigma
     | Not(exp) -> (match (eval exp sigma prog) with
                   BoolV(b) -> BoolV(not b)
                |  _ -> raise(TypeError "Can't negate this value"))
     | Null -> NullV
     | String(s) -> StringV(s)
     | MethodCall(exp, id, expl) -> (match (getMethod id prog) with
        Method(etype, id, vl1, vl2, sl, exp) -> evalMethodCall sl exp ((zip (varnames vl1) (evallist expl sigma prog))@(zipscalar (varnames vl2) NullV)) prog)
     | _ -> raise(NotImplemented "eval")



and evallist (el:exp list) (sigma:state) (prog:program) : value list =
  match el with
  [] -> []
  | h::t -> [eval h sigma prog]@evallist t sigma prog 

and evalMethodCall (stms:statement list) (retval:exp)
                          (sigma:state) (prog:program) : value =
  eval retval (execstmtlis stms sigma prog) prog

and execstmt (s:statement) (sigma:state) (prog:program) : state =
  match s with
   | If(e, s1, s2) -> (match (eval e sigma prog) with
      BoolV(b) -> if b then execstmt s1 sigma prog else execstmt s2 sigma prog
    | _ -> raise(TypeError "Not boolean!"))
   | Assignment(id, exp) -> asgn id (eval exp sigma prog) sigma
   | Block(sl) -> execstmtlis sl sigma prog
   | _ -> raise(NotImplemented "execstmt")

and execstmtlis (sl:statement list) (sigma:state) (prog:program) : state =
  match sl with
     [] -> sigma
     | h::t -> execstmtlis t (execstmt h sigma prog) prog

(* Run your program with these functions *)
let run_with_args (prog:program) (args:exp list) : string = 
  string_of_value (eval (MethodCall(Null, "main", args)) [] prog)

let run (prog:program) : string = run_with_args prog []

let eval_exp (prog:program) : string =
   let Program [Class(_, _, _, [meth])] = prog
   in let Method(_,_,_,_,_,retval) = meth
      in string_of_value (eval retval [] prog)