
open Mp2common
open List
open String

(* TODO: complete cases for each listed expression construct.
   You will need a function to process a list of expressions. *)

let rec listhasString str lis = match lis with
        | [] -> false
        | h::tt -> if h = str then true
                   else listhasString str tt

let rec alldeclaredExp vars e = match e with
          Operation(e1,_,e2) -> alldeclaredExp vars e1 && alldeclaredExp vars e2 
        | Subscript(e1,e2) -> alldeclaredExp vars e1 && alldeclaredExp vars e2
        | Length(e1) -> alldeclaredExp vars e1
        | MethodCall(e1,_,el) -> alldeclaredExp vars e1 && alldeclaredExpList vars el
        | FieldRef(e1,_) -> alldeclaredExp vars e1
        | NewArray(_,e1) -> alldeclaredExp vars e1
        | Not(e1) -> alldeclaredExp vars e1
        | Id(s) -> if listhasString s vars then true
                   else false
        | _ -> true

and alldeclaredExpList vars el = match el with
        | [] -> true
        | h::tt -> if alldeclaredExp vars h then alldeclaredExpList vars tt
                   else false

let rec alldeclaredSt vars st = match st with
        | Block sl -> alldeclaredStList vars sl
        | If (e, s1, s2) -> alldeclaredExp vars e && alldeclaredSt vars s1 && alldeclaredSt vars s2
        | While (e, s1) -> alldeclaredExp vars e && alldeclaredSt vars s1
        | Println (e) -> alldeclaredExp vars e
        | Assignment (s, e) -> listhasString s vars && alldeclaredExp vars e
        | ArrayAssignment (s, e1, e2) -> listhasString s vars && alldeclaredExp vars e1 && alldeclaredExp vars e2
        | _ -> true

and alldeclaredStList vars sl = match sl with 
        | [] -> true
        | h::tt -> if alldeclaredSt vars h then alldeclaredStList vars tt
                   else false

let rec getvarIds var_decList = match var_decList with
    | [] -> []
    | h::tt -> match h with
                Var (e_t, s) -> [s] @ getvarIds tt

let rec alldeclaredClass (Class(c,s,vdl,mdl)) = 
    let var_typeList, var_decList = split vdl in
        let var_idList = getvarIds var_decList in
            alldeclaredMDL var_idList mdl

and alldeclaredMDL vars mdl = match mdl with
    | [] -> true
    | h::tt -> match h with
                Method (e_t, s, vdl1, vdl2, sl, e) -> 
                    let complete_vars_list = vars @ (getvarIds vdl1) @ (getvarIds vdl2) in
                        if alldeclaredMethod h complete_vars_list 
                        then alldeclaredMDL vars tt
                        else false

and alldeclaredMethod md vars = match md with
    Method (e_t, s, vdl1, vdl2, sl, e) ->  
        if alldeclaredStList vars sl && alldeclaredExp vars e then true
        else false


