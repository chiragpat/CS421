
open Mp2common

(* TODO: complete cases for each listed expression construct.
   You will need a function to process a list of expressions. *)

let rec alldeclaredExp vars e = match e with
          Operation(e1,_,e2) -> true
        | Subscript(e1,e2) -> true
        | Length(e1) -> true
        | MethodCall(e1,_,el) -> true
        | FieldRef(e1,_) -> true
        | NewArray(_,e1) -> true
        | Not(e1) -> true
        | Id(s) -> true
        | _ -> true ;;

let rec alldeclaredSt vars st = true ;;

let rec alldeclaredClass (Class(c,s,vdl,mdl)) = true ;;

