#load "str.cma";;
#load "mp2common.cmo";;
#load "minijavaparse.cmo";;
#load "minijavalex.cmo";;
#load "solution.cmo";;
#load "student.cmo";;

let parseClass s = Minijavaparse.classdecl Minijavalex.tokenize (Lexing.from_string s);;
let parseStmt s = Minijavaparse.stmt Minijavalex.tokenize (Lexing.from_string s);;
let parseExp s = Minijavaparse.expression Minijavalex.tokenize (Lexing.from_string s);;

let p1_solution e vars = Solution.alldeclaredExp vars (parseExp e);;
let p1_student e vars = Student.alldeclaredExp vars (parseExp e);;

let p2_solution s vars = Solution.alldeclaredSt vars (parseStmt s);;
let p2_student s vars = Student.alldeclaredSt vars (parseStmt s);;

let p3_solution c = Solution.alldeclaredClass (parseClass c);;
let p3_student c = Student.alldeclaredClass (parseClass c);;

