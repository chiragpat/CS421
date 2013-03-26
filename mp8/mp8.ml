(* MP8 type annotator *)
open List
open Minijavaast
open Mp8common
 
let locexp (_,_,l) = l
 
let rec compile (ProgramT clslis) : mlprogram =
   (List.concat
      (map (fun (ClassT(c,s,flds,meths,objectsize)) ->
                   map (fun (MethodT(_,m,_,_,_,_,f) as meth) ->
                           let code = compileMethod meth
                           in ((m,c), (f,Array.of_list code)))
                        meths)
            clslis),
   makeSupertab (ProgramT clslis))
 
and compileMethod (MethodT(typ,f,args,locals,stmts,ret,framesize))
                  : ml_instr list  =
   let (m', code) = compileStmts stmts 0
   in let retcode = compileExp ret
      in code @ retcode @ [RETURN (locexp ret)]
 
and compileStmts (stmts:statementT list) (m:int) : int * (ml_instr list) =
   match stmts with
     [] -> (m, [])
   | h::t -> let (m1, il1) = compileStmt h m in 
             let (m2, il2) = compileStmts t m1 in
             (m2, il1@il2)
 
and compileStmt (stmt:statementT) (m:int) : int * (ml_instr list) =
  match stmt with
     BlockT(stl) -> compileStmts stl m
 
   | IfT(ann, st1, st2) -> (let il = compileExp ann in
     let m2, il2 = compileStmt st1 (m + length(il) + 1) in
     let m3, il3 = compileStmt st2 (m2+1) in
     let loc = getLocation ann in
     (m3, (il@[CJUMP(loc, (m + length(il) + 1), (m2+1))]@il2@[JUMP m3]@il3)))
     
 
   | AssignVarT(id, ann, i2) -> (let il = compileExp ann in
      let loc = getLocation ann in
      ((m + length(il) + 1), (il@[MOV(i2, loc)]))
   )
 
   | AssignFieldT(id, ann, i2) -> (let il = compileExp ann in
      let loc = getLocation ann in
      ((m + length(il) + 1), (il@[PUTFLD(i2, loc)]))
   )
 
and applyOp (bop:binary_operation) (tgt:int) (opnd1:int) (opnd2:int)
         : (ml_instr list) =
  match bop with
     Plus -> [ADD(tgt, opnd1, opnd2)]
     | Equal -> [EQUAL(tgt, opnd1, opnd2)]
     | And -> [AND(tgt, opnd1, opnd2)]
     | Or -> [OR(tgt, opnd1, opnd2)]
     | Minus -> [SUB(tgt, opnd1, opnd2)]
     | Multiplication -> [MULT(tgt, opnd1, opnd2)]
     | Division -> [DIV(tgt, opnd1, opnd2)]
     | LessThan -> [LESS(tgt, opnd1, opnd2)]
     | Strcat -> [CATSTRINGS(tgt, opnd1, opnd2)]
     | _ -> raise(NotImplemented "applyOp")
 
and compileExp ((ex,te,loc) as ae:annExpT) : (ml_instr list) =
  match ex with
    IntegerT(i) -> [LOADIMM(loc, i)]
 
    | TrueT -> [LOADIMM(loc, 1)]
   
    | FalseT -> [LOADIMM(loc, 0)]
   
    | VarT(id) -> []
   
    | OperationT(ann1, bop, ann2) ->
       (let il1 = compileExp ann1 in
        let il2 = compileExp ann2 in
        let loc1 = getLocation ann1 in
        let loc2 = getLocation ann2 in
          il1@il2@(applyOp bop loc loc1 loc2)
     )
   
    | MethodCallT(ann1, id, annlis) -> (let il = compileExpList annlis in
      let il2 = compileExp ann1 in
      il2@il@[INVOKE((getLocation ann1), id, (getLocationList annlis)); LOADRESULT(loc)]
    )
   
    | FieldT(id) -> []
   
    | FieldRef(i) -> [GETFLD(loc, i)]
   
    | ThisT -> []
   
    | NewIdT(i) -> []
   
    | NewIdAlloc(id, i) -> [NEWOBJECT(loc, id, i)]
   
    | NotT(ann) -> (let il = compileExp ann in
      let loc1 = getLocation ann in
      il@[LOADIMM(loc, 1); SUB(loc, loc, loc1)])
   
    | NullT -> [LOADIMM(loc, 0)]
   
    | StringT(s) -> [NEWSTRING(loc, s)]
   
    | CvtIntToStringT(ann) -> (let il = compileExp ann in
      let loc1 = getLocation ann in
      il@[INT2STRING(loc, loc1)]
    )
   
    | CvtBoolToStringT(ann) -> (let il = compileExp ann in
      let loc1 = getLocation ann in
      il@[BOOL2STRING(loc, loc1)]
    )
 
and compileExpList (aelis: annExpT list) : ml_instr list =
  match aelis with
     [] -> []
   | h::t -> (compileExp h)@(compileExpList t)  
 
and getLocation ((ex,te,loc) as ae:annExpT) = loc
 
and getLocationList aelis = match aelis with
   [] -> []
   | h::t -> [getLocation h]@getLocationList t
