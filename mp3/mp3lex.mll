{
  open Mp3common
}

(* definitions for letters and alpha-numeric chars *)
let jletter = ['A' - 'Z'] | ['a' - 'z']
let jletterdigit = jletter | ['0' - '9']

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

(* these are useful for line comments *)
let LineTerminator = (['\r' '\n'] | "\r\n")
let InputCharacter = [^'\r' '\n']

(* whitespace *)
let WhiteSpace = LineTerminator | ['\t' ' ']

(* string characters *)
let StringCharacter = [^'\r' '\n' '\"' '\\']

rule tokenize = parse
  (* your rules go here *)
  (* we've filled in one keyword and one operator to get you started *)
  | "boolean"       { BOOLEAN }
  (* rest of keywords go here *)

  | "("             { LPAREN }
  (* rest of symbols go here *)

  (* whitespace *)
  | WhiteSpace             { tokenize lexbuf (* ignore *) }


  | eof                    { EOF } 

  (* error fallback *)
  | _ as c                 { failwith ("Illegal character \"" ^ String.make 1 c) }

and ccomment = parse
  (* ccomment rules go here *)

  | eof                    { failwith "unterminated comment" }

and ocamlcomment depth = parse
  (* ocamlcomment rules go here *)

  | eof                    { failwith "unterminated comment" }

(* do not modify this function: *)
{ let lextest s = tokenize (Lexing.from_string s)

  let get_all_tokens s =
      let b = Lexing.from_string (s^"\n") in
      let rec g () = 
      match tokenize b with EOF -> []
      | t -> t :: g () in
      g ()
  let get_all_tokens_from_file path =
  	  let b = Lexing.from_channel (open_in path) in
	  let rec g () =
	  match tokenize b with EOF -> []
	  | t -> t :: g () in
	  g ()
 }

