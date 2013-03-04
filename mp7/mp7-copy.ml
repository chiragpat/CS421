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

let rec getMethod (id:id) (c:id) (prog:program) : method_decl =
     match c with 
     | "" -> raise(TypeError ("No such method: "^id))
     | _ -> let Class(c', inhert_c, locallis, methlis) = getClass c prog
            in (
              try
                getMethodInClass id (Class(c', inhert_c, locallis, methlis))
              with
              | TypeError(s) -> getMethod id  inhert_c prog
              | e -> raise e
            )

let rec fields (c:id) (prog:program) : string list =
  let rec aux flds = match flds with
      [] -> []
    | (_, Var(_,id))::t -> id :: aux t
  in (match c with 
    | "" -> []
    | _ -> (let Class(_,inhert_c,flds,_) = getClass c prog
            in aux flds@fields inhert_c prog))


(* START HERE *)

let applyOp (bop:binary_operation)
            (v1:stackvalue) (v2:stackvalue) : stackvalue =
match bop with
  | Plus -> (match v1, v2 with
            | IntV(i1), IntV(i2) -> IntV(i1 + i2)
            | StringV(s11), StringV(s22) -> StringV(s11 ^ s22)
            | StringV(s1), IntV(_)
            | StringV(s1), BoolV(_) -> StringV(s1 ^ string_of_stackval v2)
            | IntV(_), StringV(s1') 
            | BoolV(_),  StringV(s1') -> StringV((string_of_stackval v1) ^ s1')
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

  | _ -> raise (RuntimeError "Operator not supported")

(* Main interpreter code *)

let rec eval (e:exp) ((env,sto) as sigma:state) (prog:program)
       : stackvalue * store =
   match e with
     | Integer i -> (IntV i, sto)
     | True -> (BoolV true, sto)
     | False -> (BoolV false, sto)
     | Null -> (NullV, sto)
     | String s -> (StringV s, sto)
     
     | Id varname -> if binds varname env then (fetch varname env, sto)
                     else (
                        let Location(loc) = (fetch "this" env)
                        in let Object(id, env2) = storefetch sto loc
                        in if binds varname env2 then (fetch varname env2, sto)
                           else raise (TypeError("No variable with name: " ^ varname))  
                     )
     | This -> (fetch "this" env, sto)
     
     | NewId(id) -> let c_fields = fields id prog
                    in (Location(length sto), 
                        extend sto (Object( id, zipscalar c_fields NullV ))) 

     | Not(e1) -> (match eval e1 (env, sto) prog with
                  | BoolV(b), sto' -> (BoolV(not b), sto')
                  | _ -> raise (TypeError "Wrong type for Not"))

     | Operation(e1, Or, e2) -> (match eval e1 (env, sto) prog with
                                 | BoolV(b), sto' -> if b then (BoolV(true), sto')
                                                       else eval e2 (env, sto') prog
                                 | _ -> raise (TypeError "Wrong type for Or"))

     | Operation(e1, And, e2) -> (match eval e1 (env, sto) prog with
                                 | BoolV(b), sto' -> if not b then (BoolV(false), sto')
                                                       else eval e2 (env, sto') prog
                                 | _ -> raise (TypeError "Wrong type for And"))

     | Operation(e1, bop, e2) -> let v1, sto' = eval e1 (env, sto) prog
                                 in let v2, sto'' = eval e2 (env, sto') prog
                                 in (applyOp bop v1 v2, sto'')

     | MethodCall(e1, id, el) -> 
        (match eval e1 (env, sto) prog with
        | Location(loc), sto' -> 
            let Object(c_id, fl) = storefetch sto' loc
            in let Method(_, _, arglis, locallis, sl, retexp) = getMethod id c_id prog
            in let arg_value_list, sto'' = evallist el (env, sto') prog
            in let env' = (zip (varnames arglis) arg_value_list)
                         @(zipscalar (varnames locallis) NullV)
                         @([("this", Location loc)])
            in evalMethodCall sl retexp (env', sto'') prog

        | _ -> raise (TypeError "Calling method on an id that is not a class"))

     | _ -> raise (NotImplemented "eval")

and evallist (el:exp list) ((env,sto) as sigma:state) (prog:program)
          : stackvalue list * store = 
    match el with
    | [] -> ([], sto)
    | h::t -> let v1, sto' = eval h (env, sto) prog
              in let vl, sto'' = evallist t (env, sto') prog
              in (v1::vl, sto'')

and evalMethodCall (stms:statement list) (retval:exp) (sigma:state)
                 (prog:program) : stackvalue * store =
  eval retval (execstmtlis stms sigma prog) prog

and execstmt (s:statement) ((env,sto) as sigma:state) (prog:program) : state =
  match s with
  | Assignment(i, e) -> if binds i env then (
                          let v1, sto' = eval e sigma prog
                          in (asgn i v1 env, sto') 
                        )
                        else (
                          let Location(loc) = (fetch "this" env)
                          in let Object(id, env2) = storefetch sto loc
                          in if binds i env2 then (
                              let v1, sto' = eval e sigma prog
                              in let obj = asgn_fld (Object(id, env2)) i v1
                              in let sto'' = asgn_sto sto' loc obj
                              in (env, sto'') 
                            )
                             else raise (TypeError("No variable with name: " ^ i))
                        )

  | If(e1, s1, s2) -> (match (eval e1 sigma prog) with
                      | BoolV(b), sto' -> if b then execstmt s1 (env, sto') prog
                                          else execstmt s2 (env, sto') prog
                      | _ -> raise (TypeError "If statement type error"))

  | Block(sl) -> execstmtlis sl sigma prog
  | _ -> raise (NotImplemented "execstmt")

and execstmtlis (sl:statement list) (sigma:state) (prog:program) : state =
  match sl with
  | [] -> sigma
  | h::t -> let new_sigma = execstmt h sigma prog
            in execstmtlis t new_sigma prog

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

