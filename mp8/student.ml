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
  raise (NotImplemented "compileStmts")

and compileStmt (stmt:statementT) (m:int) : int * (ml_instr list) =
  raise (NotImplemented "compileStmt")

and applyOp (bop:binary_operation) (tgt:int) (opnd1:int) (opnd2:int)
         : (ml_instr list) =
  raise (NotImplemented "applyOp")

and compileExp ((ex,te,loc) as ae:annExpT) : (ml_instr list) =
  raise (NotImplemented "compileExp")

and compileExpList (aelis: annExpT list) : ml_instr list =
  raise (NotImplemented "compileExpList")
