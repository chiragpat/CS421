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
let a = ['"']

(* Integer Literal *)
let IntegerLiteral = ['0' - '9']+

(* floating point literals *)
let Exponent = ['e' 'E'] ['+' '-']? ['0' - '9']+
let FloatLiteral = ['0' - '9']+ '.' ['0' - '9']* Exponent? 
                  | '.' ['0' - '9']+ Exponent?

(* identifier *)
let Identifier = jletter jletterdigit*

(* Single Line Comment *)
let SingleComment = "//" InputCharacter* LineTerminator

rule tokenize = parse
  
  (* your rules go here *)
  (* we've filled in one keyword and one operator to get you started *)
  | "boolean"       { BOOLEAN }
  
  (* rest of keywords go here *)
  | "break"         { BREAK }
  | "class"         { CLASS }
  | "continue"      { CONTINUE }
  | "else"          { ELSE }
  | "extends"       { EXTENDS }
  | "float"         { FLOAT }
  | "default"       { DEFAULT }
  | "int"           { INT }
  | "new"           { NEW }
  | "if"            { IF }
  | "public"        { PUBLIC }
  | "return"        { RETURN }
  | "static"        { STATIC }
  | "while"         { WHILE }
  | "this"          { THIS }
  | "null"          { NULL_LITERAL }

  | "("             { LPAREN }
  
  (* rest of symbols go here *)
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACK }
  | "]"             { RBRACK }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "."             { DOT }
  | "="             { EQ }
  | ">"             { GT }
  | "<"             { LT }
  | "!"             { NOT }
  | ":"             { COLON }
  | "=="            { EQEQ }
  | "<="            { LTEQ }
  | ">="            { GTEQ }
  | "&&"            { ANDAND }
  | "||"            { OROR }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
  | "&"             { AND }
  | "|"             { OR }

  (* Integer *)
  | IntegerLiteral as str { INTEGER_LITERAL( int_of_string str )}

  (* FLoating Point *)
  | FloatLiteral as str   { FLOAT_LITERAL( float_of_string str )}

  (* Boolean Literals *)
  | "true"          { BOOLEAN_LITERAL true }
  | "false"         { BOOLEAN_LITERAL false }

  (* Identifier *)
  | Identifier as str     { IDENTIFIER str }

  (* String Literal *)
  | '"' (StringCharacter* as str) '"' { STRING_LITERAL str }

  (* Single Line Comments *)
  | SingleComment          { tokenize lexbuf (* ignore *) }

  (* Multi Line Comments *)
  | "/*"                   { ccomment lexbuf}

  (* whitespace *)
  | WhiteSpace             { tokenize lexbuf (* ignore *) }


  | eof                    { EOF } 

  (* error fallback *)
  | _ as c                 { failwith ("Illegal character \"" ^ String.make 1 c) }

and ccomment = parse

  (* ccomment rules go here *)
  | _       { ccomment lexbuf }
  | "*/"    { tokenize lexbuf (* move to the next one end of comment *)}
  | eof     { failwith "unterminated comment" }

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

