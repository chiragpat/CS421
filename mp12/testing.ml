#load "miniocamlast.cmo";;
#load "miniocamlparse.cmo";;
#load "miniocamllex.cmo";;
#load "mp12common.cmo";;
#load "solution.cmo";;
#use "student.ml";;
#use "run.ml";;
#use "tests.ml";;


(* Example test of tcheck - the first argument is an exp (the result of running parse on a program; ident is defined in tests.ml - and the second argument is a starting typeenv (empty in this case). *)

tcheck (parse ident) [];;
Solution.tcheck (parse ident) [];;