open Mp3common;;

open String;;
open List;;

let label_of_state st = Error (* replace with solution *)

let transition ch st = st (* replace with solution *)
   
let action st tok = [] (* replace with solution *)

let rec range i = if i=0 then [0] else i :: range (i-1);;

let list_of_string s =
    (map (fun i -> get s i) (rev (range ((String.length s) - 1))))
     @ ['\000'] ;;

let rec get_all_tokens s = get_all_tokens' (list_of_string s)

and get_all_tokens' input = 
    if hd input = '\000'
    then [EOF]
    else let (st, input', thistoken) = get_next_token input
         in (action st thistoken) @ get_all_tokens' input'

and get_next_token input = get_next_token' input Start ""

and get_next_token' input st thistoken =
    let ch = hd input
    in if ch='\000'
       then (st, input, thistoken)
       else let next_state = transition ch st
            in if next_state = NoMove
               then (st, input, thistoken)
               else get_next_token' (tl input)
                                    next_state
                                    (thistoken ^ (String.make 1 ch)) ;;

