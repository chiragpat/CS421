open Miniocamlast
open Mp10common

(* After initially parsing input, following transformations are
   applied before evaluation: redoApps, tuple2Tuple, list2List,
   let2Fun2, fun2Fun *)

(* redoApps: change occurrences of App(e1,App(e2,e3)) to
   App(App(e1,e2),e3).  Dont handle List, Tuple, Let, or Rec
   because they have not be introduced yet. *)
let rec redoApps e =
   (* redoApp has argument of the form
      App(e, App(App(App(...(App(e1, e2), e3)...,en)))).  Change this
      to App(App(App(...(App(e, e1), e2)...,en)))). *)
   let rec redoApp a =
          match a with
            | App(e1,App(e2, e3)) -> App(redoApp(App(e1, e2)), e3)
            | _ -> a
   in match e with
     Operation(e1,bop,e2) -> Operation(redoApps e1, bop, redoApps e2)
   | UnaryOperation(uop,e1) -> UnaryOperation(uop, redoApps e1)
   | App(e1,App(e2,e3)) -> redoApp e
   | App(e1,e2) -> App(redoApp e1, redoApp e2)
   | If(e1,e2,e3) -> If(redoApps e1, redoApps e2, redoApps e3) 
   | Fun(arg,e1) -> Fun(arg, redoApps e1)
   | List2(e1) -> List2(redoApps e1)
   | Tuple2(e1) -> Tuple2(redoApps e1)
   | Let2(s,sl,e1,e2) -> Let2(s,sl,redoApps e1, redoApps e2)
   | Rec2(s,sl,e1,e2) -> Rec2(s,sl,redoApps e1, redoApps e2)
   | _ -> e

(* Parser produces terms of the form "Tuple2 e", where e may
   contain the comma operator.  Transform these to Tuple [e1; ...],
   where e1, ..., are the exps separate by commas in e *)
let rec tuple2Tuple e =
   let rec remCommas e = match e with
       Operation(e1,Comma,e2) -> e1 :: remCommas e2
     | _ -> [e]
   in match e with
     Operation(e1,bop,e2) -> Operation(tuple2Tuple e1, bop, tuple2Tuple e2)
   | UnaryOperation(uop,e1) -> UnaryOperation(uop, tuple2Tuple e1)
   | App(e1,e2) -> App(tuple2Tuple e1, tuple2Tuple e2)
   | If(e1,e2,e3) -> If(tuple2Tuple e1, tuple2Tuple e2, tuple2Tuple e3) 
   | Fun(arg,e1) -> Fun(arg, tuple2Tuple e1)
   | List2(e1) -> List2(tuple2Tuple e1)
   | Tuple2(e1) -> let e1' = remCommas e1 in
                      let e1'' = List.map tuple2Tuple e1'
                         in if List.length e1'' < 2
                         then List.hd e1'' else Tuple e1''
   | Let2(s,sl,e1,e2) -> Let2(s,sl,tuple2Tuple e1, tuple2Tuple e2)
   | Rec2(s,sl,e1,e2) -> Rec2(s,sl,tuple2Tuple e1, tuple2Tuple e2)
   | _ -> e

(* Parser produces terms of the form "List2 e", where e may
   contain the semicolon operator.  Transform these to List [e1; ...],
   where e1, ..., are the exps separate by semicolons in e *)
let rec list2List e =
   let rec remSemicolons e = match e with
       Operation(e',Semicolon,e'') -> e' :: remSemicolons e''
     | _ -> [e]
   in match e with
     Operation(e1,bop,e2) -> Operation(list2List e1, bop, list2List e2)
   | UnaryOperation(uop,e1) -> UnaryOperation(uop, list2List e1)
   | App(e1,e2) -> App(list2List e1, list2List e2)
   | If(e1,e2,e3) -> If(list2List e1, list2List e2, list2List e3) 
   | Fun(arg,e1) -> Fun(arg, list2List e1)
   | List2(e1) -> let e1' = remSemicolons e1 in
                       List (List.map list2List e1')
   | Tuple(el) -> Tuple(List.map list2List el)
   | Let2(s,sl,e1,e2) -> Let2(s,sl,list2List e1, list2List e2)
   | Rec2(s,sl,e1,e2) -> Rec2(s,sl,list2List e1, list2List e2)
   | _ -> e

(* Parser produces terms of the form "Let(f,args,e,e^)" and
   "Rec(f,args,e,e^)". Change these to "Let(f, Fun2(args,e), e^)"
   and "Let(f, Rec(f, Fun2(args,e)), e^)" *)
let rec let2Fun2 e =
   match e with
     Operation(e1,bop,e2) -> Operation(let2Fun2 e1, bop, let2Fun2 e2)
   | UnaryOperation(uop,e1) -> UnaryOperation(uop, let2Fun2 e1)
   | App(e1,e2) -> App(let2Fun2 e1, let2Fun2 e2)
   | If(e1,e2,e3) -> If(let2Fun2 e1, let2Fun2 e2, let2Fun2 e3) 
   | Fun(arg,e1) -> Fun(arg, let2Fun2 e1)
   | List(el) -> List(List.map let2Fun2 el)
   | Tuple(el) -> Tuple(List.map let2Fun2 el)
   | Let2(s,[],e1,e2) -> Let(s, let2Fun2 e1, let2Fun2 e2)
   | Let2(s,sl,e1,e2) -> Let(s,Fun2(sl, let2Fun2 e1), let2Fun2 e2)
   | Rec2(s,sl,e1,e2) -> Let(s,Rec(s,Fun2(sl, let2Fun2 e1)), let2Fun2 e2)
   | _ -> e

(* Transformations above introduce terms of the form "Fun2(args, e)".
   Transform these to "Fun(arg1, Fun(arg2, ..., e)...))" *)
let rec fun2Fun e =
   let rec remFun2 (args, e) = match args with
          [] -> e
        | h::t -> Fun(h, remFun2(t,e))
   in match e with
     Operation(e1,bop,e2) -> Operation(fun2Fun e1, bop, fun2Fun e2)
   | UnaryOperation(uop,e1) -> UnaryOperation(uop, fun2Fun e1)
   | App(e1,e2) -> App(fun2Fun e1, fun2Fun e2)
   | If(e1,e2,e3) -> If(fun2Fun e1, fun2Fun e2, fun2Fun e3) 
   | Fun(arg,e1) -> Fun(arg, fun2Fun e1)
   | List(el) -> List(List.map fun2Fun el)
   | Tuple(el) -> Tuple(List.map fun2Fun el)
   | Let(s,e1,e2) -> Let(s, fun2Fun e1, fun2Fun e2)
   | Rec(s,e) -> Rec(s, fun2Fun e)
   | Fun2(args,e1) -> remFun2(args, fun2Fun e1)
   | _ -> e

let parse s = 
   let rawast = Miniocamlparse.program Miniocamllex.tokenize (Lexing.from_string s)
   in fun2Fun (let2Fun2 (list2List (tuple2Tuple (redoApps rawast))))
