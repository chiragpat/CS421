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
  | [] -> (m, [])
  | h::t -> let m', inst_list =  compileStmt h m
            in let m'', inst_list' = compileStmts t m'
            in ( m'', inst_list@inst_list' )

and compileStmt (stmt:statementT) (m:int) : int * (ml_instr list) =
  match stmt with
  | AssignVarT(id, ae, loc) -> let il1 = compileExp ae
                              in let inst_list = il1 @ [MOV(loc, locexp ae)]
                              in let m' = length(il1) + m + 1
                              in ( m', inst_list )

  | AssignFieldT(id, ae, loc) -> let il1 = compileExp ae
                                 in let inst_list = il1 @ [PUTFLD(loc, locexp ae)]
                                 in let m' = length(il1) + m + 1
                                 in ( m', inst_list ) 

  | IfT(ae, stmt_t, stmt_f) -> let il1 = compileExp ae
                               in let m', il2 = compileStmt stmt_t (m + length(il1) + 1)
                               in let m'', il3 = compileStmt stmt_f (m' + 1)
                               in let inst_list =  il1
                                                  @[CJUMP(locexp ae, (m + length(il1) + 1), (m'+1))]
                                                  @il2
                                                  @[JUMP(m'')]
                                                  @il3
                               in ( m'', inst_list ) 
  | BlockT(stmts) -> compileStmts stmts m
  | _ -> raise (NotImplemented "compileStmt")

and applyOp (bop:binary_operation) (tgt:int) (opnd1:int) (opnd2:int)
         : (ml_instr list) =
  match bop with
  | And -> [AND(tgt, opnd1, opnd2)]
  | Or -> [OR(tgt, opnd1, opnd2)]
  | LessThan -> [LESS(tgt, opnd1, opnd2)]
  | Plus  -> [ADD(tgt, opnd1, opnd2)]
  | Minus -> [SUB(tgt, opnd1, opnd2)]
  | Multiplication -> [MULT(tgt, opnd1, opnd2)]
  | Division -> [DIV(tgt, opnd1, opnd2)]
  | Equal -> [EQUAL(tgt, opnd1, opnd2)]
  | Strcat -> [CATSTRINGS(tgt, opnd1, opnd2)]
  | _ -> raise (NotImplemented "applyOp")

and compileExp ((ex,te,loc) as ae:annExpT) : (ml_instr list) =
  match ex with
  | IntegerT(i) -> [LOADIMM(loc, i)]
  | StringT(s) -> [NEWSTRING(loc, s)]
  | TrueT -> [LOADIMM(loc, 1)]
  | FalseT -> [LOADIMM(loc, 0)]
  | NullT -> [LOADIMM(loc, 0)]
  | VarT(id) -> []
  | ThisT -> []
  | NotT(ae1) -> let il1 = compileExp(ae1)
                 in il1@[LOADIMM(loc, 1);SUB(loc, loc, (locexp ae1))]
  | OperationT(ae1, bop, ae2) -> let il1 = compileExp ae1
                                 in let il2 = compileExp ae2
                                 in il1@il2@(applyOp bop loc (locexp ae1) (locexp ae2))
  | CvtIntToStringT(ae1) ->let il1 = compileExp ae1
                          in il1@[INT2STRING(loc, (locexp ae1))]
  | CvtBoolToStringT(ae1) ->let il1 = compileExp ae1
                           in il1@[BOOL2STRING(loc, (locexp ae1))]
  | FieldRef(n) -> [GETFLD(loc, n)]
  | NewIdAlloc(c, size) -> [NEWOBJECT(loc, c, size)]
  | MethodCallT(ae1, id, arglis) -> let il1 = compileExp(ae1)
                                    in let arg_inst_list = compileExpList arglis
                                    in let arg_locs = getAllLocs arglis
                                    in il1@arg_inst_list@[INVOKE((locexp ae1), id, arg_locs);LOADRESULT(loc)]
  | _ -> raise (NotImplemented "compileExp")

and compileExpList (aelis: annExpT list) : ml_instr list =
  match aelis with
  | [] -> []
  | h::t -> (compileExp h)@(compileExpList t)

and getAllLocs (aelis: annExpT list) : int list =
  match aelis with
  | [] -> []
  | h::t -> (locexp h)::(getAllLocs t)
