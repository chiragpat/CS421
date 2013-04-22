type exp =
    Operation of exp * binary_operation * exp
  | Var of id
  | PolyVar of id * typeterm
  | IntConst of int
  | True | False
  | Let of id * typeterm * exp * exp
  | Fun of id * typeterm * exp
  | App of exp * exp

and typeterm =
    BoolType
  | IntType
  | FunType of typeterm * typeterm
  | Typevar of id

and binary_operation = LessThan | GreaterThan
  | And | Or
  | Plus | Minus | Div | Mult

and id = string;;
