(* This is the type returned by calls to eval *)
type value = IntV of int | StringV of string | BoolV of bool | NullV

(* Representation of variable state *)
type varname = string
and binding = (varname * value)
and state = binding list

exception TypeError of string      (* e.g. 1 + true *)
exception RuntimeError of string   (* e.g. division by zero *)
exception NotImplemented of string (* anything you haven't done yet *)

let string_of_value (v:value) : string = match v with
    IntV i -> string_of_int i
  | StringV s -> s
  | BoolV b ->  string_of_bool b
  | NullV -> "null"
