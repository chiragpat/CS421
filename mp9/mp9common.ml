open Miniocamlast

exception RuntimeError of string
exception TypeError of string

let envError id = raise (TypeError ("Unbound variable: " ^ id))

(* convert exp boolean types to ocaml booleans *)
let bool_of_exp e = match e with True -> true | _ -> false
let exp_of_bool b = if b then True else False

let rec string_of_expr e = match e with
    Operation(e1, op, e2) -> (string_of_expr e1)^(string_of_bop op)
                            ^ (string_of_expr e2)
  | UnaryOperation(op, e) -> (string_of_uop op)^" "^(string_of_expr e)
  | Var x -> x
  | StrConst s -> "\"" ^ s ^ "\""
  | IntConst int -> string_of_int int
  | True -> "true"
  | False -> "false"
  | List l -> "[" ^ (List.fold_right
                      (fun x y -> (string_of_expr x) ^ "; " ^ y) l "") ^ "]"
                                                      
  | Tuple l -> "(" ^ (List.fold_right
                      (fun x y -> (string_of_expr x) ^ ", " ^ y) l "") ^ ")"
  | If(e1, e2, e3) -> "if " ^ (string_of_expr e1) ^
                      " then " ^ (string_of_expr e2) ^
                      " else " ^ (string_of_expr e3)
  | Let(x, e0, e) -> "let " ^ x ^ " = " ^ (string_of_expr e0) ^
                      " in " ^ (string_of_expr e)
  | Rec(x, e) -> "rec " ^ x ^ " = " ^ (string_of_expr e)
  | Fun(x, e) -> "fun " ^ x ^ " -> " ^ (string_of_expr e)
  | App(e1, e2) -> "(" ^ (string_of_expr e1)
                  ^ " " ^ (string_of_expr e2) ^ ")"
  | _ -> raise (TypeError "unknown exp")
  
and string_of_env env =
  "{" ^ (List.fold_right (fun (i,v) y -> i ^ "->" ^ (string_of_expr v) ^", " ^ y) env "") ^ "}"
  (* "<fun>" *)

and string_of_bop bop = match bop with
    Semicolon -> ";"
  | Comma -> ","
  | Equals -> "="
  | LessThan -> "<"
  | GreaterThan -> ">"
  | NotEquals -> "!="
  | Assign -> ":="
  | And -> "&&"
  | Or -> "||"
  | Plus -> "+"
  | Minus -> "-"
  | Div -> "/"
  | Mult -> "*"
  | StringAppend -> "^"
  | ListAppend -> "@"
  | Cons -> "::"

and string_of_uop uop = match uop with 
    Ref -> "ref"
  | Deref -> "!" 
  | Not -> "not"
  | Head -> "hd"
  | Tail -> "tl"
  | Fst -> "fst"
  | Snd -> "snd"
