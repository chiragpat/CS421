#load "mp6common.cmo";;
#load "solution.cmo";;
#load "minijavaast.cmo";;
#load "minijavaparse.cmo";;
#load "minijavalex.cmo";;
#use "student.ml";;
open Minijavaast
open Mp6common

(* Run make before loading this file in ocaml *)

(* Use this function to enable writing your programs in MiniJava syntax *)
let lex_and_parse s = Minijavaparse.program Minijavalex.tokenize (Lexing.from_string s)

let nop = "class Main { public int main() { return null; } }"
and fact = "class Main { public int main(int n) { int r; if (n == 0) { r = 1; } else { r = n * main.main(n - 1); } return r; } }";;

(* Compare your implementation to the solution like this *)
(* (run (lex_and_parse nop),
 Solution.run (lex_and_parse nop));;
(run_with_args (lex_and_parse fact) [Integer(5)],
 Solution.run_with_args (lex_and_parse fact) [Integer(5)]);; *)

(* You can check for failure *)
(* try run (lex_and_parse "class Main { public int main() { return 1 == true; } }") with
   (TypeError ex) -> "success" | _ -> "fail" *)


(* Before you implement statements, use this function to check your implementation of expressions by running evaluate on expressions *that do not contain variables*. *)
let evaluate e =
     let p = " class Main { public int main() { return " ^ e ^ "; } }"
     in run (lex_and_parse p);;

