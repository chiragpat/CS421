let ident = "fun a:alpha -> a";;
let ident2 = "(fun a:int -> a) 3";;
let ident3 = "(fun a:int -> a) (1 + 1)";;
let let1 = "let f : (alpha -> alpha) = fun a : alpha -> a in f[bool -> bool] true";;
let let2 = "let f : (alpha -> alpha) = fun a : alpha -> a in f[(int -> int) -> (int -> int)] f[int -> int]";;
let let3 = "let f : ((int -> alpha) -> alpha) = fun x : (int -> alpha) -> x 0 in f[(int -> int) -> int] fun y : int -> y + 1";;
let let4 = "let f : (int -> int) = fun x : int -> (let g : ((int -> int) -> int) = fun y : (int -> int) -> y x in g (fun a : int -> a + 1)) in f 5";;
