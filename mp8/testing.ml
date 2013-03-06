#load "mp8common.cmo";;
#load "solution.cmo";;
#load "minijavaast.cmo";;
#load "solution.cmo";;
#load "minijavaparse.cmo";;
#load "minijavalex.cmo";;
#use "student.ml";;

open Minijavaast
open Mp8common
open List


let lex_and_parse s = Minijavaparse.program Minijavalex.tokenize (Lexing.from_string s);;

let test_cases = [
"class Main { public int main() { return 1 == 0; } }";
"class Main { public int main() { return \"Hello world!\"; } }";
"class Main { public int main() { return 5; } }";
"class Main { public int main() { boolean x; x=false; return !x; } }";
"class Main { public int main() { return false; } }";
"class Main { public int main() { return (5<6) & (8<7) ; } }";
"class Main { public int main(int a) {return a;} }";
"class Main { public int main(int a) {return a+\"a\";} }";
"class Main { public int main(int a) {int b; a=1;\
                                      if (a==1)\
                                      b=3;\
                                      else b=5;\
                                      return b;} }";
"class Main { public int main(int n) { int r; if (n == 0) r = 1; else r = n * this.main(n - 1); return r; } }";
];;

print_string "Annotating\n";;
map (fun i ->
annotateProg (lex_and_parse (List.nth test_cases i))) [6];;
print_string "Compiling\n";;
map (fun i ->
compile (annotateProg (lex_and_parse (List.nth test_cases i)))) [6];;
print_string "Executing\n";;
map (fun i ->
execute (compile (annotateProg (lex_and_parse (List.nth test_cases i))))
        [InputInt 10] 1000 10 false) [7];;
print_string "Printing code\n";;
map (fun i ->
print_string (string_of_prog (compile (annotateProg (lex_and_parse (List.nth test_cases i)))))) [6];;

(*
let prog1 = "class Main { \
   public int main() {\
      int x;\
      int y;\
      x = new ListNode();\
      y = x.setval(3);\
      return x.getval();\
   }\
}\
\
class ListNode {\
   int val;\
   public int setval (int v) {\
      val = v;\
      return v;\
   }\
   public int getval () {\
      return val;\
   }\
}";;

let prog3 = "class Main { \
   public int main() {\
      int x;\
      int y;\
      x = new C();\
      y = new D();\
      return y.f();\
   }\
}\
\
class C {\
   public int f () {\
      return this.g();\
   }\
   public int g () {\
      return \"C\";\
   }\
\
class D extends C {\
   public int g () {\
      return \"D\";\
   }\
}";;
*)
(*
let prog2 = "class Main { \
   public int main() {\
      int x;\
      int y;\
      int z;
      x = new C();\
      y = new D();\
      z = y.f();\
      return z+z;\
   }\
}\
\
class C {\
   public int f () {\
      return this.g();\
   }\
   public int g () {\
      return 5;\
   }\
}\
\
class D extends C {\
   public int g () {\
      return 6;\
   }\
}";;

print_string "Parsing\n";;
lex_and_parse prog2;;
print_string "Annotating\n";;
annotateProg (lex_and_parse prog2);;
print_string "Compiling\n";;
compile (annotateProg (lex_and_parse prog2));;
print_string "Executing\n";;
execute (compile (annotateProg (lex_and_parse prog2)));;
print_string "Printing code\n";;
print_string (string_of_prog
               (compile (annotateProg (lex_and_parse prog2))));;

*)
let prog4 = "\
class Main {\
  public int main () {\
     List l;
     List l2;
     l = new List().init(20,null);\
     l2 = new List().init(10,l);\
     return l2.leng();\
  }\
}\
\
class List {\
   List head;\
   List tail;\
\
   public List init(int hd, int tl) {\
      head = hd; tail = tl;\
      return this;\
   }\
\
   public List hd () { return head; }\
   public List tl () { return tail; }\
   public int hdtl () { return tail.hd(); }\
   public int leng () { int r; if (tail==null) r = 7;
                        else r = 51+tail.leng();
                        return r; }
}\
";;

print_string "Parsing\n";;
lex_and_parse prog4;;
print_string "Annotating\n";;
annotateProg (lex_and_parse prog4);;
print_string "Compiling\n";;
compile (annotateProg (lex_and_parse prog4));;
print_string "Printing code\n";;
print_string (string_of_prog
               (compile (annotateProg (lex_and_parse prog4))));;
print_string "Executing\n";;
execute (compile (annotateProg (lex_and_parse prog4))) [] 100 10 true;;

let fact = "class Main { public int main(int n) { int r; if (n == 0) r = 1; else r = n * this.main(n - 1); return r; } }";;

print_string "Parsing\n";;
lex_and_parse fact;;
print_string "Annotating\n";;
annotateProg (lex_and_parse fact);;
print_string "Compiling\n";;
compile (annotateProg (lex_and_parse fact));;
print_string "Printing code\n";;
print_string (string_of_prog
               (compile (annotateProg (lex_and_parse fact))));;
print_string "Executing\n";;
execute (compile (annotateProg (lex_and_parse fact))) [InputInt(5)] 100 10 true;;


let odd2 = "class Main { public boolean main(int n) { return this.isOdd(n); } public boolean isOdd(int n)  { boolean b; if (n == 0) b = false; else b = this.isEven(n - 1); return b; } public boolean isEven(int n) { boolean b; if (n == 0) b = true; else b = this.isOdd(n - 1); return b; } }";;

print_string "Parsing\n";;
lex_and_parse odd2;;
print_string "Annotating\n";;
annotateProg (lex_and_parse odd2);;
print_string "Compiling\n";;
compile (annotateProg (lex_and_parse odd2));;
print_string "Printing code\n";;
print_string (string_of_prog
               (compile (annotateProg (lex_and_parse odd2))));;
print_string "Executing\n";;
execute (compile (annotateProg (lex_and_parse odd2))) [InputInt(4)] 100 10 true;;
