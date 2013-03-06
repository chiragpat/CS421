open List
open Minijavaast

exception TypeError of string      (* e.g. 1 + true *)
exception NotImplemented of string (* anything you haven't done yet *)
exception NoMatch of string

let rec zip lis1 lis2 =
  match (lis1, lis2) with
    ([], []) -> [] | (h1::t1, h2::t2) -> (h1,h2) :: zip t1 t2

let rec hasid id lis =
       (lis <> []) && ((fst (hd lis) = id) || (hasid id (tl lis)))
let rec get id ((a,b)::t) =
       if a = id then b else get id t

(* Type environment *)
type typeenv = (id * exp_type) list
type methodenv = (id * method_decl) list
type classmethenv = (id * methodenv) list
type classfieldenv = (id * typeenv) list
type subclasses = (id * id) list
type progdb = classfieldenv * classmethenv * subclasses

let rec removekinds (flds: (var_kind * var_decl) list)
                           : (id*exp_type) list =
    match flds with [] -> [] |
                   (_,(Var(d,v)))::flds' -> (v,d)::removekinds flds'

let rec removeVars (decls: var_decl list) : (id*exp_type) list =
    match decls with [] -> [] |
                   (Var(d,v))::decls' -> (v,d)::removeVars decls'

let rec getprogdb (prog:program) : progdb
         = (fields prog, getmethodenv prog, subclasses prog)

and subclasses (Program cl) : subclasses =
   map (fun (Class(c,s,_,_)) -> (c,s)) cl

and getsubclasses ((_,_,sc):progdb) = sc

and fields ((Program cl) as prog) : classfieldenv =
         map (fun cls -> (name cls, getFieldsCls prog cls)) cl

and name (Class(c,_,_,_)) = c

and getFieldsCls (prog:program) (Class(c,s,kdlis,_)) : typeenv =
    (if s="" then [] else (getFieldsCls prog (getClass s prog)))
    @ (removekinds kdlis)

and getClass (c:id) (Program classlis) : class_decl =
  let rec aux classlis = match classlis with
      [] -> raise (TypeError ("No such class: "^c))
    | (Class(c', _, _, _) as theclass) :: t ->
          if c=c' then theclass else aux t
  in aux classlis

and getmethodenv ((Program cl) as prog) : classmethenv =
     map (fun cls -> (name cls, getMethodsCls prog cls)) cl

and getMethodsCls (prog:program) (Class(c,s,kdlis,meths)) : methodenv =
    (methlist meths) @
    (if s="" then [] else (getMethodsCls prog (getClass s prog)))

and methlist (meths:method_decl list) : methodenv
   = map (fun (Method(_,m,_,_,_,_) as themethod) -> (m, themethod)) meths

and getfields (c:id) ((flds,_,_):progdb) : typeenv = get c flds

and getmethods (c:id) ((_,meths,_):progdb) : methodenv = get c meths

and getmethod (c:id) (m:id) ((_,meths,_):progdb) : method_decl =
      if not (hasid c meths)
      then raise (TypeError ("Class "^c^" does not exist"))
      else let cmeths = get c meths
           in if not (hasid m cmeths)
              then raise (TypeError
                            ("Method "^m^" does not exist in class "^c))
              else get m cmeths

and getargs (m:id) (c:id) (db:progdb) : typeenv =
      let Method(_,_,vars,_,_,_) = getmethod c m db
      in removeVars vars

and getlocals (m:id) (c:id) (db:progdb) : typeenv =
      let Method(_,_,_,vars,_,_) = getmethod c m db
      in removeVars vars

and getvars (m:id) (c:id) (db:progdb) : typeenv =
   (getlocals m c db) @ (getargs m c db)

and getallvars (m:id) (c:id) (db:progdb) : typeenv =
   (getvars m c db) @ (getfields c db)

(* Type-annotated programs *)
type programT = ProgramT of (class_declT list)

and class_declT = ClassT of id * id   
        * ((var_kind * var_decl) list) 
        * (method_declT list)
        * int (* number of fields *)

and method_declT = MethodT of exp_type
        * id 
        * (var_decl list) 
        * (var_decl list) 
        * (statementT list) 
        * annExpT
        * int (* size of stack frame: # of arg + # of locals
                                      + max # of temporaries *)

and statementT = BlockT of (statementT list)
    | IfT of annExpT * statementT * statementT
    | WhileT of annExpT * statementT
    | PrintlnT of annExpT
    | AssignVarT of id * annExpT * int
    | AssignFieldT of id * annExpT * int
    | ArrayAssignmentT of id * annExpT * annExpT
    | BreakT
    | ContinueT
    | SwitchT of annExpT 
        * ((int * (statementT list)) list)   (* cases *)
        * (statementT list)   (* default *)

and annExpT = expT * exp_type * int

and expT = OperationT of annExpT * binary_operation * annExpT
    | MethodCallT of annExpT * id * (annExpT list)
    | IntegerT of int
    | TrueT
    | FalseT
    | VarT of id
    | FieldT of id
    | FieldRef of int
    | ThisT
    | NewIdT of id
    | NewIdAlloc of id * int
    | NotT of annExpT
    | NullT
    | StringT of string
    | CvtIntToStringT of annExpT
    | CvtBoolToStringT of annExpT
    | NewArrayT of exp_type * annExpT
    | FloatT of float
    | ArrayT of annExpT * annExpT
    | LengthT of annExpT

(* asgn_compat te te' tests whether it is legal to assign
   a value of type te' to a variable of type te.  The rule
   is that either te=te' or they are both object types
   and te' is a subclass of te. *)
    
let asgn_compat (te:exp_type) (te':exp_type) (db:progdb) : bool
    = true
(* This is the actual definition of asgn_compat.  However, it is
   not complete, and until it is completed, it is better to just
   assume assignments are legal.
    = let rec isdescendant (c:id) (b:id) =
          let s = get c (getsubclasses db)
          in s = b || (s <> "" && isdescendant s b)
      in te = te' ||
         (match (te,te') with
           (ObjectType c, ObjectType c') -> isdescendant c' c
          | _ -> false)
*)

let rec annotateProg (Program cl) =
   let db = getprogdb (Program cl)
   in let typedProg = annotateClassList cl db
      in ProgramT (addLocations typedProg db)

and annotateClassList (cl:class_decl list) (db:progdb)
           : class_declT list = match cl with
     [] -> []
   | c::cl' -> annotateClass c db :: annotateClassList cl' db

and annotateClass (Class(c,s,flds,methods)) (db:progdb) : class_declT =
    ClassT(c, s, flds, annotateMethods c db methods, length flds) 

and annotateMethods (c:id) (db:progdb) (methods:method_decl list)
          : method_declT list = match methods with
     [] -> []
   | mdef :: mdefs -> annotateMethod c db mdef ::
                         annotateMethods c db mdefs

and annotateMethod (c:id) (db:progdb) (Method(t,m,args,locals,body,ret))
   = MethodT(t,m,args,locals,annotateStmts c m db body,
                             annotateExpr c m db ret, 0)

and annotateStmt (c:id) (m:id) (db:progdb) (s:statement) : statementT =
    match s with
      Block sl -> BlockT (annotateStmts c m db sl)
    | If (e, s1, s2) ->
          let (ex,te,loc) as ae = annotateExpr c m db e
          in if te = BoolType
             then IfT (ae, annotateStmt c m db s1,
                           annotateStmt c m db s2)
             else raise (TypeError "Non-boolean as condition")
    | Assignment (x, e) ->
          let (ex,te,loc) as ae = annotateExpr c m db e
          in if hasid x (getvars m c db)
             then let te' = get x (getvars m c db)
                  in if asgn_compat te' te db
                     then AssignVarT(x, ae, 0)
                     else raise (TypeError "Wrong type in assignment")
             else if hasid x (getfields c db)
                  then let te' = get x (getfields c db)
                       in if asgn_compat te' te db
                          then AssignFieldT(x, ae, 0)
                          else raise (TypeError "Wrong type in assignment")
                  else raise (TypeError ("Variable "^x^" not declared"))
    | While (e, s1) ->
          let (ex,te,loc) as ae = annotateExpr c m db e
          in if te = BoolType
             then WhileT (ae, annotateStmt c m db s1)
             else raise (TypeError "Non-boolean as condition")
    | Break -> BreakT
    | Continue -> ContinueT
    | _ -> raise (NotImplemented "Unimplemented stmt")

and annotateStmts (c:id) (m:id) (db:progdb) (sl:statement list)
       : statementT list = match sl with
      [] -> []
    | h::t -> annotateStmt c m db h :: annotateStmts c m db t

and annotateExpr (c:id) (m:id) (db:progdb) (e:exp)
       : annExpT = match e with
      True -> (TrueT, BoolType, 0)
    | False -> (FalseT, BoolType, 0)
    | Integer i -> (IntegerT i, IntType, 0)
    | String s -> (StringT s, StringType, 0)
    | Null -> (NullT, ObjectType "whatever", 0)
    | Id x -> if hasid x (getvars m c db)
              then (VarT x, get x (getvars m c db), 0)
              else if hasid x (getfields c db)
                   then (FieldT x, get x (getfields c db), 0)
                   else raise (TypeError ("Variable "^x^" not declared"))
    | Not e -> let (ex,te,loc) as ae = annotateExpr c m db e
               in if te = BoolType
                  then (NotT ae, BoolType, 0)
                  else raise (TypeError "! applied to non-boolean")
    | Operation(e1,And,e2) ->
               let (ex1,te1,loc1) as ae1 = annotateExpr c m db e1
               and (ex2,te2,loc2) as ae2 = annotateExpr c m db e2
               in if te1 <> BoolType || te2 <> BoolType
                  then raise (TypeError "And applied to non-booleans")
                  else (OperationT(ae1, And, ae2), BoolType, 0)
    | Operation(e1,Or,e2) ->
               let (ex1,te1,loc1) as ae1 = annotateExpr c m db e1
               and (ex2,te2,loc2) as ae2 = annotateExpr c m db e2
               in if te1 <> BoolType || te2 <> BoolType
                  then raise (TypeError "Or applied to non-booleans")
                  else (OperationT(ae1, Or, ae2), BoolType, 0)
    | Operation(e1,bop,e2) ->
               let (ex1,te1,loc1) as ae1 = annotateExpr c m db e1
               and (ex2,te2,loc2) as ae2 = annotateExpr c m db e2
               in let t = valtype te1 te2 bop
                  in if t=StringType && bop=Plus
                     then let ae1' = convert ae1 t
                          and ae2' = convert ae2 t
                          in (OperationT(ae1', Strcat, ae2'), t, 0)
                     else (OperationT(ae1, bop, ae2), t, 0)
    | This -> (ThisT, ObjectType c, 0)
    | NewId d -> (NewIdT d, ObjectType d, 0)
    | MethodCall(e0, id, args) ->
          let (ex,te,loc) as ae0 = annotateExpr c m db e0
          and aargs = annotateExprs c m db args
(* STUB: *) in (MethodCallT(ae0, id, aargs), return_type id te db, 0)          
(*
          in if matches te aargs id db
             then (MethodCallT(ae0, id, aargs), return_type id te db, 0)
             else raise (TypeError
                         ("Call to method "^id^" does not match definition"))
*)
    | _ -> raise (NotImplemented "Unimplemented expression")

and annotateExprs (c:id) (m:id) (db:progdb) (el:exp list)
       : annExpT list = match el with
      [] -> []
    | h::t -> annotateExpr c m db h :: annotateExprs c m db t

and valtype (te1:exp_type) (te2:exp_type) (bop:binary_operation) =
    match bop with
      And|Or -> if te1 = BoolType && te2 = BoolType
                then BoolType
                else raise (TypeError "Operands of && not both boolean")
    | LessThan -> if te1 = IntType && te2 = IntType
                  then BoolType
                  else raise (TypeError "Operands of < not both int")
    | Minus | Multiplication | Division
               -> if te1 = IntType && te2 = IntType
                  then IntType
                  else raise (TypeError
                               "Operands of arithmetic operator not both int")
    | Equal -> BoolType (* STUB: (because not dealing with null)
                if te1 = te2
                then BoolType
                else raise (TypeError "Operands of equal not the same type") *)
    | Plus -> if te1 = StringType or te2 = StringType
              then StringType
              else if te1 = IntType && te2 = IntType
                   then IntType
        (* STUB: *)
                   else IntType
(*
                   else raise (TypeError "Type error applying Plus")
*)

and matches (te:exp_type) (aargs:annExpT list) (id:id) (db:progdb)
    : bool =
    true

and return_type (m:id) (typ:exp_type) (db:progdb) : exp_type
    = match typ with
        ObjectType c -> let Method(t,_,_,_,_,_) = getmethod c m db
                        in t
      | _ -> raise (TypeError ("Method "^m^" called on non-class object"))

(* convert annotated Exp to string if necessary.
   typ is type of result.  If it is String, that means ae may
   need to be converted to string; otherwise, do nothing.  *)
and convert ((ex,te,loc) as ae:annExpT) (typ:exp_type) : annExpT =
     if typ <> StringType then ae
     else ((match te with
                StringType -> ex
              | IntType -> CvtIntToStringT ae
              | BoolType -> CvtBoolToStringT ae
              | _ -> raise (TypeError "Can cvt only int or bool to string")),
           StringType, 0)

and addLocations (cl:class_declT list) (db:progdb) : class_declT list =
    map (addLocationsCls db) cl

and addLocationsCls (db:progdb) (ClassT(c,s,flds,meths,n):class_declT)
                       : class_declT =
   let fldlocs = getLocationsFlds flds
   in ClassT(c,s,flds,addLocationsMeths db fldlocs meths, n)

and getLocationsFlds (flds: (var_kind * var_decl) list)
            : (id * int) list =
    let idlist = map fst (removekinds flds)
    in zip idlist (genrange 0 ((length idlist)-1))

and getLocationsVars (vars: var_decl list) : (id * int) list =
    let idlist = map fst (removeVars vars)
    in zip idlist (genrange 1 (length idlist))  (* "this" at loc 0 *)

and genrange i k = if i > k then [] else i :: (genrange (i+1) k)

and addLocationsMeths (db:progdb) (fldlocs:(id * int) list)
          (meths: method_declT list) : method_declT list =
   map (addLocationsMeth db fldlocs) meths

(* addLocationsMeth annotates all expressions in a method with
   locations, and fills in the last field of MethodT with
   the size of the environment for this method.
   Note that stacktop is atop parameters and locals *and* this. *)
and addLocationsMeth (db:progdb) (fldlocs:(id * int) list)
          (MethodT(t,m,args,locals,sl,res,_)) : method_declT =
    let varlocs = getLocationsVars (args @ locals)
    and stacktop = length(args) + length(locals) + 1
    in let (i1,sl') = addLocationsStmtlis db fldlocs varlocs stacktop sl
       and (i2,res') = addLocationsExp db fldlocs varlocs stacktop res
       in MethodT(t,m,args,locals,sl',res', max i1 i2)

and maxlis n lis =
   if lis=[] then n else max (hd lis) (maxlis n (tl lis))

and addLocationsStmtlis (db:progdb) (fldlocs: (id*int) list)
          (varlocs: (id*int) list) (nextloc:int) (sl: statementT list)
                 : int * (statementT list) =
   let stms = map (addLocationsStmt db fldlocs varlocs nextloc) sl
   in (maxlis nextloc (map fst stms), map snd stms)

and addLocationsStmt (db:progdb) (fldlocs: (id*int) list)
           (varlocs: (id*int) list) (nextloc:int) (s: statementT)
             : int * statementT =
   match s with
      BlockT sl ->
         let (loc,sl') = addLocationsStmtlis db fldlocs varlocs nextloc sl
         in (loc, BlockT sl')
    | IfT ((exp,typ,_) as ae, st1, st2) ->
         let (loc,ae') = addLocationsExp db fldlocs varlocs nextloc ae
         and (loc',st1') = addLocationsStmt db fldlocs varlocs nextloc st1
         and (loc'',st2') = addLocationsStmt db fldlocs varlocs nextloc st2
         in (max loc (max loc' loc''), IfT(ae', st1', st2'))
    | AssignVarT (v, ae, _) ->
         let (loc,ae') = addLocationsExp db fldlocs varlocs nextloc ae
         in (loc, AssignVarT(v, ae', get v varlocs))
    | AssignFieldT (f, ae, _) ->
         let (loc,ae') = addLocationsExp db fldlocs varlocs nextloc ae
         in (loc, AssignFieldT(f, ae', get f fldlocs))
    | WhileT ((exp,typ,_) as ae, st1) ->
         let (loc,ae') = addLocationsExp db fldlocs varlocs nextloc ae
         and (loc',st1') = addLocationsStmt db fldlocs varlocs nextloc st1
         in (max loc loc', WhileT(ae', st1'))
    | BreakT -> (nextloc, BreakT)
    | ContinueT -> (nextloc, ContinueT)
    | _ -> raise (NotImplemented "stmt")

and addLocationsExp (db:progdb) (fldlocs: (id*int) list)
       (varlocs: (id*int) list) (nextloc:int) ((exp,exptyp,_) as ae:annExpT)
             : int * annExpT =
   match exp with
      TrueT -> (nextloc+1, (TrueT, BoolType, nextloc))
    | FalseT -> (nextloc+1, (FalseT, BoolType, nextloc))
    | NullT -> (nextloc+1, (NullT, BoolType, nextloc))
    | IntegerT i -> (nextloc+1, (IntegerT i, IntType, nextloc))
    | StringT s -> (nextloc+1, (StringT s, StringType, nextloc))
    | VarT v -> (nextloc, (exp, exptyp, get v varlocs))
    | FieldT f -> (nextloc+1, (FieldRef (get f fldlocs), exptyp, nextloc))
    | NotT e -> let (loc,e') = addLocationsExp db fldlocs varlocs nextloc e
                in (loc+1, (NotT e', BoolType, loc))
    | OperationT(e1,bop,e2) ->
               let (loc,e1') = addLocationsExp db fldlocs varlocs nextloc e1
               in let (loc',e2') = addLocationsExp db fldlocs varlocs loc e2
                  in (loc'+1, (OperationT(e1',bop,e2'),exptyp,loc'))
    | ThisT -> (nextloc, (exp, exptyp, 0))
    | NewIdT d -> let sz = List.length(getfields d db)
                  in (nextloc+1, (NewIdAlloc(d,sz), ObjectType d, nextloc))
    | MethodCallT(e0, id, args) ->
          let (loc,e0') = addLocationsExp db fldlocs varlocs nextloc e0
          in let (loc',args') = addLocationsExps db fldlocs varlocs loc args
             in (loc'+1, (MethodCallT(e0', id, args'),exptyp,loc'))
    | CvtIntToStringT e0 ->
          let (loc,e0') = addLocationsExp db fldlocs varlocs nextloc e0
          in (loc+1, (CvtIntToStringT e0', exptyp,loc))
    | CvtBoolToStringT e0 ->
          let (loc,e0') = addLocationsExp db fldlocs varlocs nextloc e0
          in (loc+1, (CvtBoolToStringT e0', exptyp,loc))
    | _ -> raise (NotImplemented "Unimplemented expression")

and addLocationsExps (db:progdb) (fldlocs: (id*int) list)
       (varlocs: (id*int) list) (nextloc:int) (aelis:annExpT list)
               : int * (annExpT list) =
    match aelis with
      [] -> (nextloc, [])
    | h::t -> let (loc,h') = addLocationsExp db fldlocs varlocs nextloc h 
              in let (loc',t') = addLocationsExps db fldlocs varlocs loc t 
                 in (loc', h'::t')

(* Here are type of data structures that need to exist at run time: *)

type stackloc = int
and  instrloc = int
and  classname = string
and  methodname = string

type ml_instr =
     MOV of stackloc * stackloc
   | LOADIMM of stackloc * int
   | ADD of stackloc * stackloc * stackloc
   | SUB of stackloc * stackloc * stackloc
   | MULT of stackloc * stackloc * stackloc
   | DIV of stackloc * stackloc * stackloc
   | LESS of stackloc * stackloc * stackloc
   | AND of stackloc * stackloc * stackloc
   | OR of stackloc * stackloc * stackloc
   | EQUAL of stackloc * stackloc * stackloc
   | JUMP of instrloc
   | CJUMP of stackloc *  instrloc *  instrloc
   | INT2STRING of stackloc * stackloc
   | BOOL2STRING of stackloc * stackloc
   | GETFLD of stackloc * int
   | PUTFLD of int *  stackloc
   | NEWSTRING of stackloc * string
   | CATSTRINGS of stackloc *  stackloc *  stackloc
   | NEWOBJECT of stackloc * classname * int
   | NEWARRAY of stackloc * stackloc
   | RETURN of stackloc
   | LOADRESULT of stackloc
   | INVOKE of stackloc * methodname * (stackloc list)
   | JUMPIND of stackloc
   | ARRAYREF of stackloc * stackloc * stackloc

type ('a,'b) mapping = ('a * 'b) list

type funcode = ml_instr array
type environment = int array
type frame = environment * instrloc * funcode
type stack = frame list
type heapvalue = Str of string | Obj of string * environment |
                                 Arr of int array | Unallocated
type heap = heapvalue array
type heaptop = int
type reg0 = int
type pc = int

type method_table = (methodname * classname, int * funcode) mapping
type super_table = (classname, (methodname, classname) mapping) mapping
type mlprogram = method_table * super_table
type machine_state = pc ref * funcode ref * stack ref * heap
                   * heaptop ref * reg0 ref
                   
type inputvalue = InputInt of int | InputString of string

let rec binds (a:'a) (m: ('a,'b) mapping) : bool =
       (m <> []) && ((fst (hd m) = a) || (binds a (tl m)))

let rec fetch (a:'a) (m: ('a,'b) mapping) : 'b option =
    match m with [] -> None
               | ((a',b')::m') -> if a'=a then Some b' else fetch a m'

(* join two maps, with the second one overriding the first if
   there are conflicts *)
let rec joinmaps (map1: ('a,'b) mapping) (map2: ('a,'b) mapping)
                 : ('a,'b) mapping =
    let keys = List.map fst map2
    in let rec addmap map = match map with
                  [] -> []
                | ((k,v)::map') -> if List.mem k keys
                                   then addmap map'
                                   else (k,v)::addmap map'
       in (addmap map1) @ map2

let method_table ((mt,_):mlprogram) = mt
and super_table ((_,st):mlprogram) = st

let rec getClassT (c:id) (ProgramT classlis) : class_declT =
  let rec aux classlis = match classlis with
      [] -> raise (TypeError ("No such class: "^c))
    | (ClassT(c', _, _, _, _) as theclass) :: t ->
          if c=c' then theclass else aux t
  in aux classlis

and makeSupertab ((ProgramT clslis) as prog:programT) : super_table
  = List.map (makeSupertabCls prog) clslis

and makeSupertabCls (prog:programT) (ClassT(c,s,_,meths,_)) =
   (c, methodnames c prog)

and methodnames (c:id) (prog:programT) =
   let ClassT(c,s,_,meths,_) = getClassT c prog
   in joinmaps (if s="" then [] else methodnames s prog)
               (List.map (fun (MethodT(_,f,_,_,_,_,_)) -> (f,c)) meths)


let rec range (m:int) (n:int) : int list
       = if m>n then [] else m :: range (m+1) n;;

open Array

let pc ((pC,_,_,_,_,_):machine_state) = pC
let funcode ((_,funcodE,_,_,_,_):machine_state) = funcodE
let stack ((_,_,stacK,_,_,_):machine_state) = stacK
let heap ((_,_,_,heaP,_,_):machine_state) = heaP
let heaptop ((_,_,_,_,heaptoP,_):machine_state) = heaptoP
let reg0 ((_,_,_,_,_,reG0):machine_state) = reG0
let env ((enV,_,_):frame) = enV
let return_addr ((_,return_addR,_):frame) = return_addR
let return_code ((_,return_codE,_):frame) = return_codE
let curr_frame state = env (hd (!(stack state)))
let incrpc (st:machine_state) = pc st := (!(pc st))+1
let setpc (st:machine_state) (iloc:int) = pc st := iloc
let incrheaptop (st:machine_state) = heaptop st := (!(heaptop st))+1
let setreg0 (st:machine_state) (i:int) = reg0 st := i
let pushframe (st:machine_state) (frame:frame) = 
          (stack st) := frame :: (!(stack st))
let popframe (st:machine_state) = (stack st) := tl (!(stack st))
let setcode (st:machine_state) (code:funcode) = (funcode st) := code

let emulate (state: machine_state) (prog: mlprogram) : machine_state =
   (* stack is guaranteed not to be empty; pc is guaranteed
      to point to actual instruction (i.e. not be out-of-bounds) *)
   let one_step ((p,f,s,h,ht,r) as state) : unit =
       let csf = curr_frame state
       and nextinstr = get (!(funcode state)) (!(pc state))
       in (match nextinstr with
            MOV(tgt,src) ->
                   set csf tgt (get csf src)
          | LOADIMM(tgt,i) ->
                   set csf tgt i
          | ADD(tgt,src1,src2) ->
                   set csf tgt ((get csf src1) + (get csf src2))
          | SUB(tgt,src1,src2) ->
                   set csf tgt ((get csf src1) - (get csf src2))
          | MULT(tgt,src1,src2) ->
                   set csf tgt ((get csf src1) * (get csf src2))
          | DIV(tgt,src1,src2) ->
                   set csf tgt ((get csf src1) / (get csf src2))
          | LESS(tgt,src1,src2) ->
                   set csf tgt (if (get csf src1) < (get csf src2)
                                then 1 else 0)
          | AND(tgt,src1,src2) ->
                   set csf tgt (if (get csf src1) = 0
                                then 0 else get csf src2)
          | OR(tgt,src1,src2) ->
                   set csf tgt (if (get csf src1) = 1
                                then 1 else get csf src2)
          | EQUAL(tgt,src1,src2) ->
                   set csf tgt (if (get csf src1) = (get csf src2)
                                then 1 else 0)
          | JUMP(iloc) ->
                   setpc state iloc
          | JUMPIND src ->
                   setpc state (get csf src)
          | CJUMP(loc, iloc_t, iloc_f) ->
                   setpc state (let b = get csf loc
                                in if b=1 then iloc_t else iloc_f)
          | INT2STRING(tgt,src) ->
                   let s = string_of_int (get csf src)
                   and ht = !(heaptop state)
                   in (set (heap state) ht (Str s);
                       set csf tgt ht;
                       incrheaptop state)
          | BOOL2STRING(tgt,src) ->
                   let s = if get csf src = 0 then "false" else "true"
                   and ht = !(heaptop state)
                   in (set (heap state) ht (Str s);
                       set csf tgt ht;
                       incrheaptop state)
          | NEWSTRING(tgt,strlit) ->
                   let ht = !(heaptop state)
                   in (set (heap state) ht (Str strlit);
                       set csf tgt ht;
                       incrheaptop state)
          | CATSTRINGS(tgt,src1,src2) ->
                   let hloc1 = (get csf src1)
                   and hloc2 = (get csf src2)
                   and ht = !(heaptop state)
                   in let Str s1 = get (heap state) hloc1
                      and Str s2 = get (heap state) hloc2
                      in (set (heap state) ht (Str (s1^s2));
                          set csf tgt ht;
                          incrheaptop state)
          | GETFLD(tgt,srcfld) ->
                   let hloc = get csf 0  (* this *)
                   in let Obj(_,flds) = get (heap state) hloc
                      in set csf tgt (get flds srcfld)
          | ARRAYREF(tgt,arr,idx) ->
                   let hloc = get csf arr
                   and index = get csf idx
                   in let Arr(elts) = get (heap state) hloc
                      in set csf tgt (get elts index)
          | PUTFLD(tgtfld, loc) ->
                   let hloc = get csf 0  (* this *)
                   in let Obj(_,flds) = get (heap state) hloc
                      in set flds tgtfld (get csf loc)
          | NEWOBJECT(tgt,cls,object_sz) ->
                   let obj = Obj(cls, make object_sz 0)
                   and ht = !(heaptop state)
                   in (set (heap state) ht obj;
                       set csf tgt ht;
                       incrheaptop state)
          | NEWARRAY(tgt,sizesrc) ->
                   let sz = get csf sizesrc
                   in let arr = Arr (make sz 0)
                      in let ht = !(heaptop state)
                         in (set (heap state) ht arr;
                             set csf tgt ht;
                             incrheaptop state)
          | RETURN(src) ->
                   let frame::_ = !(stack state)
                   in let (env,loc,code) = frame
                      in (setreg0 state (get csf src);
                          popframe state;
                          setpc state loc;
                          setcode state code)
          | LOADRESULT(tgt) ->
                   set csf tgt (!(reg0 state))
          | INVOKE(rcvr,m,args) ->
                   let hloc = get csf rcvr
                   in let Obj(c,flds) = get (heap state) hloc
                   in let Some supermap = fetch c (super_table prog)
                   in let Some cls = fetch m supermap
                   in let Some (framesize,code) =
                             fetch (m,cls) (method_table prog)
                   in let env = make framesize 0
                      in (set env 0 hloc;
                          List.map
                              (fun i -> set env i (get csf (nth args (i-1))))
                              (range 1 (List.length args));
                          pushframe state (env, !(pc state)+1,
                                                !(funcode state));
                          setpc state 0;
                          setcode state code))
          ;
          (match nextinstr with
          | JUMP(iloc) -> ()
          | JUMPIND(loc) -> ()
          | CJUMP(loc, iloc_t, iloc_f) -> ()
          | INVOKE(rcvr,methodname,args) -> ()
          | RETURN(src) -> ()
          | _ -> incrpc state)
      
   in let rec run (state: machine_state) : machine_state =
             if !(stack state) = []
             then state
             else (one_step state; run state)

      in run state

(*
let frame0 = (make 10 0, 0, make 0 (RETURN 1))
let stack0 = [frame0]
let heap0 = make 5 Unallocated;;
let il0 = make 10 (RETURN 1);;
set il0 0 (LOADIMM(1,5));;
set il0 1 (LOADIMM(2,4));;
set il0 2 (LESS(4,1,2));;
set il0 3 (BOOL2STRING(2,4));;
set il0 4 (RETURN 2);;
*)

let string_of_heapvalue (v:heapvalue) = match v with
    Str s -> "\"" ^ s ^ "\"" | Obj(c,_) -> c | Arr _ -> "array"
  | Unallocated -> "empty"

let print_heap n heap =
    List.map (fun i -> (print_int i; print_string ":  ";
                        print_string ((string_of_heapvalue (get heap i))
                                      ^ "\n")))
        (range 1 n)

let initstate ((mt,st) as prog:mlprogram)
              (args: inputvalue list) (heapsz:int) =
   let Some (framesz,instrs) = fetch ("main", "Main") mt
   in let frame = make framesz 0
      (* make heap start from 1; then 0 is not a valid address
         and can be used for null *)
      and heap = make (heapsz+1) Unallocated
          (* create rcvr object for Main *)
      in let _ = set heap 1 (Obj("Main", make 0 0))
         and _ = set frame 0 1 (* "this" point to heap loc 1 *)
         and heaptop = ref 2
         in let _ = List.map (fun (loc,inval) -> (match inval with
                        InputInt i -> set frame loc i
                      | InputString s -> set heap (!heaptop) (Str s);
                                         set frame loc (!heaptop);
                                         heaptop := (!heaptop)+1))
                    (zip (range 1 (List.length args)) args)  (* locations
                               of arguments start at 1 *)
            in (ref 0,   (* initial pac *)
                ref instrs,  (* start in method main of class Main *)
                ref [(frame, 0, make 0 (RETURN 0))],  (* frame has size
                               framesz; return address/code don't matter *)
                heap,
                heaptop,
                ref 0)

let execute (prog:mlprogram) (args: inputvalue list) (heapsz:int)
            (heaptoprint:int) (outputtype:bool) =
   let st = emulate (initstate prog args heapsz) prog
   in print_string ("Exit\nOutput: reg0="^(string_of_int (!(reg0 st))) ^ "\n");
      print_heap heaptoprint (heap st);
      (if outputtype (* int *)
       then (print_int (!(reg0 st)); print_string "\n")
       else let (Str strval) = get (heap st) (!(reg0 st))
            in print_string strval; print_string "\n")

let string_of_type typ = match typ with
      BoolType  -> "booltype"
    | IntType -> "inttype"
    | ObjectType id  -> "objtype"
    | StringType -> "stringtype"

let string_of_op bop = match bop with
      And -> "and"
    | Or -> "or"
    | LessThan -> "less"
    | Plus -> "plus"
    | Minus -> "minus"
    | Multiplication -> "mult"
    | Division -> "div"
    | Equal -> "equal"
    | Strcat -> "strcat"

let string_of_st st =
   let rec string_of_meths (m,c) = "   " ^ m ^ " " ^ c ^ "\n"
   and string_of_cls (c, meths) =
          ("class " ^ c ^ "\n" ^ (String.concat ""
                                   (List.map string_of_meths meths)))
   in String.concat "" (List.map string_of_cls st)

let string_of_instr (instr:ml_instr) =
   match instr with
     MOV(tgt,src) ->
             "MOV        "^(string_of_int tgt)^","^(string_of_int src)
   | LOADIMM(tgt,i) ->
             "LOADIMM    "^(string_of_int tgt)^","^(string_of_int i)
   | ADD(tgt,src1,src2) ->
             "ADD        "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | SUB(tgt,src1,src2) ->
             "SUB        "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | MULT(tgt,src1,src2) ->
             "MULT       "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | DIV(tgt,src1,src2) ->
             "DIV        "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | LESS(tgt,src1,src2) ->
             "LESS       "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | AND(tgt,src1,src2) ->
             "AND        "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | OR(tgt,src1,src2) ->
             "OR         "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | EQUAL(tgt,src1,src2) ->
             "EQUAL      "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | JUMP(iloc) ->
             "JUMP       "^(string_of_int iloc)
   | JUMPIND(src) ->
             "JUMPIND    "^(string_of_int src)
   | CJUMP(loc, iloc_t, iloc_f) ->
             "CJUMP      "^(string_of_int loc)^","^(string_of_int iloc_t)
                                         ^","^(string_of_int iloc_f)
   | INT2STRING(tgt,src) ->
             "INT2STRING "^(string_of_int tgt)^","^(string_of_int src)
   | BOOL2STRING(tgt,src) ->
             "BOOL2STRING "^(string_of_int tgt)^","^(string_of_int src)
   | NEWSTRING(tgt,strlit) ->
             "NEWSTRING "^(string_of_int tgt)^","^"\""^strlit^"\""
   | CATSTRINGS(tgt,src1,src2) ->
             "CATSTRINGS "^(string_of_int tgt)^","^(string_of_int src1)
                                         ^","^(string_of_int src2)
   | ARRAYREF(tgt,arr,idx) ->
             "ARRAYREF   "^(string_of_int tgt)^","^(string_of_int arr)
                                         ^","^(string_of_int idx)
   | GETFLD(tgt,srcfld) ->
             "GETFLD     "^(string_of_int tgt)^","^(string_of_int srcfld)
   | PUTFLD(tgtfld, loc) ->
             "PUTFLD     "^(string_of_int tgtfld)^","^(string_of_int loc)
   | NEWOBJECT(tgt,cls,object_sz) ->
             "NEWOBJECT  "^(string_of_int tgt)^",\""^cls^"\"," ^
                                         (string_of_int object_sz)
   | NEWARRAY(tgt,szsrc) ->
             "NEWOBJECT  "^(string_of_int tgt)^","^ (string_of_int szsrc)
   | RETURN(src) ->
             "RETURN     "^(string_of_int src)
   | LOADRESULT(tgt) ->
             "LOADRESULT "^(string_of_int tgt)
   | INVOKE(rcvr,m,args) ->
             "INVOKE     "^(string_of_int rcvr)^","^m^","^
                 (String.concat "," (List.map string_of_int args))

let string_of_code (code:funcode) : string =
   let codelocs = genrange 0 ((length code)-1)
   and jumptargets = make (length code) false
   in let _ = set jumptargets 0 true
      and _ = List.map
                 (fun i -> match get code i with
                    JUMP i -> set jumptargets i true
                  | CJUMP(_,i1,i2) -> set jumptargets i1 true;
                                      set jumptargets i2 true
                  | _ -> ()) codelocs
      and printlabel i = if i<10 then (string_of_int i)^":   "
                         else if i<100 then (string_of_int i)^":  "
                         else (string_of_int i)^": "
      in let instrs =
             List.map
              (fun i -> (if (get jumptargets i)
                         then printlabel i
                         else "     ")
                        ^ (string_of_instr (get code i))
                        ^ "\n")
              codelocs
         in String.concat "" instrs

let string_of_funcode ((m,c),(i,code)) =
   "method " ^ m ^ " in " ^ c ^ " (" ^ (string_of_int i) ^ ")\n"
   ^ (string_of_code code)

let string_of_mt mt = String.concat "\n" (List.map string_of_funcode mt)

let string_of_prog ((mt,st) as prog:mlprogram) : string =
   string_of_st st ^ "\n" ^ string_of_mt mt

