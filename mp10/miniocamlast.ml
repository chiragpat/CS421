type exp =
    Operation of exp * binary_operation * exp
  | UnaryOperation of unary_operation * exp
  | Var of id
  | StrConst of string
  | IntConst of int
  | True | False
  | List of exp list
  | Tuple of exp list
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * exp
  | Rec of id * exp
  | App of exp * exp
  | Closure of exp * environment
  (* The following constructors are used only temporarily, and
    are translated to constructor above before evaluation *)
  | Fun2 of string list * exp
  | List2 of exp
  | Tuple2 of exp
  | Let2 of string * (string list) * exp * exp
  | Rec2 of string * (string list) * exp * exp

and value = exp

and binary_operation = Semicolon | Comma | Equals | LessThan
  | GreaterThan | NotEquals | Assign | And | Or
  | Plus | Minus | Div | Mult
  | StringAppend | ListAppend | Cons

and unary_operation = Ref | Deref | Not | Head | Tail | Fst | Snd

and environment =
  (id * value) list
  (* (id -> value) *)

and id = string;;
