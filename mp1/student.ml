(* CS421 - Spring 2013
 * MP1
 *)

open List
(* Problem 1 *)
let pair_to_list (x, y) = [y; x]

(* Problem 2 *)
let dist ((x0,y0), (x1,y1)) = sqrt(((x0-.x1)**2.0) +. ((y0-.y1)**2.0))

(* Problem 3 *)
let sort_first_two l = match l with
  | [] -> []
  | [one] -> [one]
  | one::two::others -> if one > two then [two] @ [one] @ others
                        else l

(* Problem 4 *)
let rec concat_odd l = match l with
  | [] -> ""
  | [one] -> one
  | one::two::others -> let rec_string = concat_odd(others) in (one ^ rec_string)

(* Problem 5 *)
let rec is_sorted l = match l with
  | []        -> true
  | [one]     -> true
  | one::two::others -> if one > two then false
                        else is_sorted others

(* Problem 6 *)
let rec group_ascending lis = match lis with
  | []          -> [[]]
  | [one]       -> [[one]]
  | one::others -> let rec_list = group_ascending(others) in  
                    if one < hd(others) then [ [one] @ hd(rec_list) ] @ tl(rec_list)
                    else [[one]] @ rec_list

(* Problem 7 *)
let rec split_list lis = match lis with
  | [] -> ([],[])
  | [one] -> ([one], [])
  | one::two::others -> let rec_tuple = split_list (others) 
                        in ( [one]@fst(rec_tuple), [two]@snd(rec_tuple) )

(* Problem 8 *)
let rec merge l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | ([],one::others) -> [one] @ merge l1 others
  | (one::others, []) -> [one] @ merge others l2
  | (one_1::others_1, one_2::others_2) -> if one_1 < one_2 then [one_1] @ merge others_1 l2
                                          else [one_2] @ merge l1 others_2

(* Problem 9 *)
let rec mergesort lis = match lis with
  | [] | [_] -> lis
  | _        -> let (l1, l2) = split_list lis in 
                let left = mergesort l1 in
                let right = mergesort l2  in
                merge left right

