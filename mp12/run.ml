open Miniocamlast
open Mp12common

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
   | App(e1,App(e2,e3)) -> redoApp e
   | App(e1,e2) -> App(redoApp e1, redoApp e2)
   | Fun(arg,t,e1) -> Fun(arg,t, redoApps e1)
   | Let(s,t,e1,e2) -> Let(s,t,redoApps e1, redoApps e2)
   | _ -> e

let parse s = 
   let rawast = Miniocamlparse.program Miniocamllex.tokenize (Lexing.from_string s)
   in redoApps rawast
