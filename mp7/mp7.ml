open List
open Minijavaast
open Mp7common

(* MP7 interpreter - no objects, arrays, or floats; just one class;
   limited set of statements.  See MP6 write-up for details. *)

(* Utility functions *)

let rec asgn (id:id) (v:stackvalue) (env:environment) : environment =
  match env with
     [] -> raise (TypeError ("Assignment to unbound variable " ^ id))
  | (id1,v1) :: t -> if id = id1 then (id,v) :: t
                     else (id1,v1) :: asgn id v t

let rec binds (id:id) (env:environment) : bool =
  match env with
    [] -> false
  | (id', _)::t -> id=id' || binds id t

let rec fetch (id:id) (env:environment) : stackvalue =
  match env with
    [] -> raise (TypeError ("Unbound variable: "^id))
  | (id', v)::t -> if id=id' then v else fetch id t

let rec mklist (i:int) (v:stackvalue) : stackvalue list =
       if i=0 then [] else v :: mklist (i-1) v

let rec zip (lis1:id list) (lis2:stackvalue list) : environment =
  match (lis1, lis2) with
    ([], []) -> [] | (h1::t1, h2::t2) -> (h1,h2) :: zip t1 t2
  | _ -> raise (TypeError ("Mismatched formal and actual param lists"))

let zipscalar (lis:id list) (v:stackvalue) : environment =
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

let extend (st:store) (hval:heapvalue) : store = st @ [hval]

let storefetch (st:store) (loc:int) : heapvalue = List.nth st loc

let asgn_fld (obj:heapvalue) (id:varname) (sv:stackvalue) : heapvalue =
  let Object(c,flds) = obj
  in Object(c, asgn id sv flds)

let rec replace_nth i x lis = match (i, lis) with
    (0, _::t) -> x :: t
  | (n, h::t) -> h :: replace_nth (n-1) x t

let asgn_sto (sto:store) (loc:int) (obj:heapvalue) =
  replace_nth loc obj sto;;

let getClass (c:id) (Program classlis) : class_decl =
  let rec aux classlis = match classlis with
      [] -> raise (TypeError ("No such class: "^c))
    | (Class(c', _, _, _) as theclass) :: t ->
          if c=c' then theclass else aux t
  in aux classlis

(* Note: modify the following two helper functions to support inheritance *)

let getMethod (id:id) (c:id) (prog:program) : method_decl =
     getMethodInClass id (getClass c prog)

let fields (c:id) (prog:program) : string list =
  let rec aux flds = match flds with
      [] -> []
    | (_, Var(_,id))::t -> id :: aux t
  in let Class(_,_,flds,_) = getClass c prog
     in aux flds


(* START HERE *)

let applyOp (bop:binary_operation)
            (v1:stackvalue) (v2:stackvalue) : stackvalue =
match bop with
  | Plus -> (match v1, v2 with
            | IntV(i1), IntV(i2) -> IntV(i1 + i2)
            | StringV(s11), StringV(s22) -> StringV(s11 ^ s22)
            | StringV(s1), IntV(_)
            | StringV(s1), BoolV(_) -> StringV(s1 ^ string_of_stackvalue v2)
            | IntV(_), StringV(s1') 
            | BoolV(_),  StringV(s1') -> StringV((string_of_stackvalue v1) ^ s1')
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

let rec eval (e:exp) ((env,sto) as sigma:state) (prog:program)
       : stackvalue * store =
   match e with
       Integer i -> (IntV i, sto)
     | _ -> raise (NotImplemented "eval")

and evallist (el:exp list) ((env,sto) as sigma:state) (prog:program)
          : stackvalue list * store = 
  raise (NotImplemented "evallist")

and evalMethodCall (stms:statement list) (retval:exp) (sigma:state)
                 (prog:program) : stackvalue * store =
  raise (NotImplemented "evalMethodCall")

and execstmt (s:statement) ((env,sto) as sigma:state) (prog:program) : state =
  raise (NotImplemented "execstmt")

and execstmtlis (sl:statement list) (sigma:state) (prog:program) : state =
  raise (NotImplemented "execstmtlis")

let run_with_args (Program(Class(cname,_,_,_) :: _) as prog)
                  (args:exp list) : string =
   let env = [("this", Location 0)]
   and sto = [Object(cname, [])]
   in let (v,_) = eval (MethodCall(Id "this", "main", args))
                       (env,sto) prog
      in string_of_stackval v

let run (prog:program) : string = run_with_args prog []

let eval_exp (prog:program) : string =
   let Program [Class(_, _, _, [meth])] = prog
   in let Method(_,_,_,_,_,retval) = meth
      in string_of_stackval (fst (eval retval ([],[]) prog))

