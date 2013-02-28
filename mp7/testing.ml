#load "mp7common.cmo";;
#load "solution.cmo";;
#load "minijavaast.cmo";;
#load "minijavaparse.cmo";;
#load "minijavalex.cmo";;
#use "student.ml";;
open Minijavaast
open Mp7common

(* Use this function to enable writing your programs in MiniJava syntax *)
let lex_and_parse s = Minijavaparse.program Minijavalex.tokenize (Lexing.from_string s)

let sigma1 = [("x", IntV 4); ("y", IntV 5); ("mycar", Location 0); ("z", BoolV true)]
(*let store1 = [Object("Main",[("color", StringV "green"); ("tiresize", IntV 16); ("functional", BoolV false)])]*)
let store1 = [Object("Main",[])]
let factP = Program
   [Class ("Main", "", [],
     [Method (IntType, "main",
       [Var (IntType, "n")],
       [Var (IntType, "r")],
       [If
         (Operation (Id "n", Equal,
           Integer 0),
         Block
          [Assignment ("r", Integer 1)],
         Block
          [Assignment ("r",
            Operation (Id "n",
             Multiplication,
             MethodCall (Id "this", "main",
              [Operation (Id "n", Minus,
                Integer 1)])))])],
       Id "r")])];;


let nop = "class Main { public int main() { return null; } }"
and fact = "class Main { public int main(int n) { int r; if (n == 0) { r = 1; } else { r = n * this.main(n - 1); } return r; } }"
;;

(*
((eval (Id "x") (sigma1,store1) factP),
(eval (MethodCall (Id "mycar", "main", [Integer 4])) (sigma1,store1) factP),
(eval (Operation (Id "x", LessThan, Id "y")) (sigma1,store1) factP),
(eval (Operation (Id "z",And, False)) (sigma1,store1) factP),
(eval (Operation (Id "x", Plus, Id "y")) (sigma1,store1) factP)
);;
*)
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
     in eval_exp (lex_and_parse p);;


