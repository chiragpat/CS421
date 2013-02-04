open Mp3common;;

open String;;
open List;;

let label_of_state st = match st with
|  Int  | Lbracket | Rbracket 
| Ident | Float | Slash -> Token

| Whitespace | CPPcommentEnd | CcommentEnd -> Discard

| _      -> Error

let transition ch st = match st with
| Start        -> (match ch with
                  | '{'        -> Lbracket
                  | '}'        -> Rbracket
                  | ' ' | '\n' -> Whitespace
                  | '/'        -> Slash 
                  | 'a'..'z'   -> Ident   
                  | '0'..'9'   -> Int
                  | _          -> BadChar)

| Whitespace   -> (match ch with
                  | ' ' | '\n' -> Whitespace
                  | _          -> NoMove)

| Ident        -> (match ch with
                  | 'a'..'z' 
                  | '0'..'9'   -> Ident
                  | _          -> NoMove)

| Int          -> (match ch with
                  | '0'..'9'   -> Int
                  | '.'        -> IntDot
                  | _          -> NoMove)

| IntDot       -> (match ch with
                  | '0'..'9'   -> Float
                  | _          -> NoMove)

| Float        -> (match ch with
                  | '0'..'9'   -> Float
                  | _          -> NoMove)

| Slash        -> (match ch with
                  | '/'        -> CPPcomment
                  | '*'        -> Ccomment
                  | _          -> NoMove)

| CPPcomment   -> (match ch with
                  | '\n'       -> CPPcommentEnd
                  | _          -> CPPcomment)

| Ccomment     -> (match ch with
                  | '*'        -> CcommentStar
                  | _          -> Ccomment )

| CcommentStar -> (match ch with
                  | '*'        -> CcommentStar
                  | '/'        -> CcommentEnd
                  | _          -> Ccomment )

| Lbracket | Rbracket | CPPcommentEnd 
| BadChar  | CcommentEnd | NoMove -> NoMove 
   
let action st tok = match label_of_state st with
  Discard -> []
| Error   -> [Errtok]
| Token   -> (match st with
              Int -> [ Intlit(tok) ]
            | Lbracket -> [ Lbrack ]
            | Rbracket -> [ Rbrack ]
            | Ident    -> [ Id(tok) ]
            | Float    -> [ Floatlit(tok) ]
            | Slash    -> [ Divide ]
            | _   -> [])



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

