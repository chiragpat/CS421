#load "miniocamlast.cmo";;
#load "miniocamlparse.cmo";;
#load "miniocamllex.cmo";;
#load "mp9common.cmo";;
#load "solution.cmo";;
#use "student.ml";;
#use "run.ml";;

(* Use this function to enable writing your programs in MiniOCaml syntax *)
let lex_and_parse s = 
   let rawast = Miniocamlparse.program Miniocamllex.tokenize (Lexing.from_string s)
   in fun2Fun (let2Fun2 (list2List (tuple2Tuple (redoApps rawast))))

let fact = "let rec fact n = if n = 0 then 1 else let r = fact (n - 1) in n * r in fact 5";;

(* Compare your implementation to the solution like this *)
(eval (lex_and_parse fact) emptyEnv,
 Solution.eval (lex_and_parse fact) emptyEnv);;
