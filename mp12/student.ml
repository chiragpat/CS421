open List;;
open Miniocamlast;;
open Mp12common;;

(* union set1 set2 returns set1@set2 without repetitions, assuming
   set1 and set2 do not have repetitions *)
let union set1 set2 = 
   fold_right (fun x s -> if mem x set2 then s else x::s) set1 set2;;

(* string_of_typeterm returns a "pretty-printed" version of an exp *)
let rec string_of_typeterm (t:typeterm) : string = match t with
     IntType -> "int"
   | BoolType -> "bool"
   | Typevar s -> "var " ^ s
   | FunType (t1, t2) -> "(" ^ (string_of_typeterm t1) ^ "->"
                             ^ (string_of_typeterm t2) ^ ")"

(* Return all the free type variables occurring in a typeterm,
   without repetitions *)
let rec getvars (t:typeterm) : string list = match t with
     IntType | BoolType -> []
   | Typevar s -> [s]
   | FunType(t1,t2) -> union (getvars t1) (getvars t2)

(* typeenv is a map from program variables to type schemes *)
type typeenv = (string * typescheme) list
and typescheme = typeterm * (string list)

(* Printable version of typescheme - for debugging *)
let string_of_typescheme ((t,lis):typescheme) : string =
   (string_of_typeterm t) ^ "[" ^
        fold_right (fun i s -> i ^ " " ^ s) lis "]"

(* Basic operations on type environments.  lookup throws a
   Not_found exception if the variable is absent. *)
let empty_te = []

let lookup (x:string) (gamma:typeenv) : typescheme =
   snd (find (fun (x',_) -> x=x') gamma)

let extend (s:string) (ts:typescheme) (gamma:typeenv) : typeenv
     = (s,ts)::gamma

(* Return the list of type variables free in a type scheme -
   for type scheme (tau,bndvars), this is just the list of
   free variables return by getvars, minus variables in bndvars. *)
let rec freevars ((tau,bndvars):typescheme) : string list =
    let tauvars = getvars tau
    in filter (fun s -> not (mem s bndvars)) tauvars

(* instanceOf tau' ts - determine if tau' is an instance of ts,
   i.e. tau' is the same as ts except that bound variables in ts
   are replaced by types.  In addition, every occurrence of a
   single type variable must be replaced by the same type. *)
let instanceOf (tau':typeterm) ((tau,bndvars):typescheme) : bool =
  let rec aux_instanceOf tau' (tau,bndvars) sub : bool =
    match tau', tau with
    | IntType, IntType -> true
    | BoolType, BoolType -> true
    | t', Typevar(id) -> 
        if (exists (fun a -> if a = id then true else false) bndvars) then 
        (
          if (exists (fun (a,t) -> (a = id) && (t' <> t) ) sub) 
          then false
          else true
        )
        else raise (TypeError "Invalid instantation not a bound var")

    | FunType(t1', t2'), FunType(t1, t2) -> 
        if (aux_instanceOf t1' (t1, bndvars) sub) then
        ( 
          (match t1 with
           | Typevar(id) -> aux_instanceOf t2' (t2, bndvars) ((id, t1')::sub)
           | _ -> aux_instanceOf t2' (t2, bndvars) sub
          )
        )
        else false 
    | _ -> raise (TypeError "Invalid instantation")
    
  in aux_instanceOf tau' (tau, bndvars) []

(* generalize tau in gamma = i.e. find variables in tau that
   are not free in gamma and return a type scheme that binds them *)
let generalize (tau:typeterm) (gamma:typeenv) : typescheme =
  let rec aux_gen tau gamma bndvars = 
    match gamma with
    | [] -> bndvars
    | (i, ts)::t -> let fvs = freevars ts
                    in let bvars = filter (fun v -> not (mem v fvs)) bndvars
                    in aux_gen tau t bvars 
  in (tau, (aux_gen tau gamma (getvars tau)))

let opTypes (bop:binary_operation) (tau1:typeterm) (tau2:typeterm) : typeterm =
  match bop with
  | Plus | Minus
  | Mult | Div -> (match tau1, tau2 with
                   | IntType, IntType -> IntType
                   | _ -> raise (TypeError "Wrong type for bop of ints return int"))

  | GreaterThan | LessThan -> (match tau1, tau2 with
                               | IntType, IntType -> BoolType
                               | _ -> raise (TypeError "Wrong type for bop of ints return bool"))
  | And | Or -> (match tau1, tau2 with
                 | BoolType, BoolType -> BoolType
                 | _ -> raise (TypeError "Wrong type for bop of bools"))
  | _ -> raise (TypeError "Invalid values for bop")


let rec tcheck (e:exp) (gamma:typeenv) : typeterm =
  match e with
  | IntConst(i) -> IntType
  | True | False -> BoolType
  | Var(id) -> fst (lookup id gamma)
  | App(e, e') -> let type1 = tcheck e gamma
                  in let type2 = tcheck e' gamma
                  in (match type1 with
                      | FunType(type1', type1'') -> if type2 = type1' then type1''
                                                    else raise (TypeError "Wrong type for app of a fun")
                      | _ -> raise (TypeError "Wrong type for app"))

  | Fun(id, t, e) -> FunType(t, (tcheck e (extend id (t, []) gamma)))
  | Operation(e1, bop, e2) -> opTypes bop (tcheck e1 gamma) (tcheck e2 gamma)
  | PolyVar(id, t) -> if (instanceOf t (lookup id gamma)) then t 
                      else raise (TypeError "Wrong type in PolyVar")  

  | Let(id, t, e, e') -> if t = tcheck e gamma
                         then tcheck e' (extend id (generalize t gamma) gamma)
                         else raise (TypeError "Wrong type in e of Let") 

