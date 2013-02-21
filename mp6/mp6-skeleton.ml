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

let applyOp (bop:binary_operation) (v1:value) (v2:value) : value =
  raise (NotImplemented "applyOp")

(* Main interpreter code *)

let rec eval (e:exp) (sigma:state) (prog:program) : value =
   match e with
       Integer i -> IntV i
     | _ -> raise (NotImplemented "eval")

and evallist (el:exp list) (sigma:state) (prog:program) : value list =
  raise (NotImplemented "evallist")

and evalMethodCall (stms:statement list) (retval:exp)
                          (sigma:state) (prog:program) : value =
  raise (NotImplemented "evallist")

and execstmt (s:statement) (sigma:state) (prog:program) : state =
  raise (NotImplemented "execstmt")

and execstmtlis (sl:statement list) (sigma:state) (prog:program) : state =
  raise (NotImplemented "execstmt")

(* Run your program with these functions *)
let run_with_args (prog:program) (args:exp list) : string = 
  string_of_value (eval (MethodCall(Null, "main", args)) [] prog)

let run (prog:program) : string = run_with_args prog []

let eval_exp (prog:program) : string =
   let Program [Class(_, _, _, [meth])] = prog
   in let Method(_,_,_,_,_,retval) = meth
      in string_of_value (eval retval [] prog)
