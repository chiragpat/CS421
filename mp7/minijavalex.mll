{
  open Minijavaparse
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
let Space = ['\t' ' ']

(* comments *)
let EndOfLineComment = "//" InputCharacter* LineTerminator

(* JavaDoc contents *)
let JavaDocContent = ( [^'*' '\r' '\n'] | '*'+[^'*' '/' '\r' '\n'] )*

(* identifiers *)
let Identifier = jletter jletterdigit*

(* integer literals *)
let DecIntegerLiteral = ['0' - '9']['0' - '9']*

(* floating point literals *)
let Exponent = ['e' 'E'] ['+' '-']? ['0' - '9']+

let FLit1 = ['0' - '9']+ '.' ['0' - '9']* Exponent?
let FLit2 = '.' ['0' - '9']+ Exponent?

let FloatLiteral = FLit1 | FLit2

(* string characters *)
let StringCharacter = [^'\r' '\n' '\"' '\\']

rule tokenize = parse
  (* your rules go here *)
  | "boolean"       { BOOLEAN }
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

  | "true"          { BOOLEAN_LITERAL true }
  | "false"         { BOOLEAN_LITERAL false }

  | "null"          { NULL_LITERAL }

  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACK }
  | "]"             { RBRACK }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "."             { DOT }

  | "="             { EQ }
  | "=="            { EQEQ }
  | "<"             { LT }
  | "<="            { LTEQ }
  | ">"             { GT }
  | ">="            { GTEQ }
  | "!"             { NOT }
  | ":"             { COLON }
  | "&&"            { ANDAND }
  | "||"            { OROR }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
  | "&"             { AND }
  | "|"             { OR }

  (* numeric literals *)
  | DecIntegerLiteral as s { INTEGER_LITERAL(int_of_string s) }

  | (FloatLiteral as s)    { FLOAT_LITERAL(float_of_string s) }

  (* end-of-line comments *)
  | EndOfLineComment       { tokenize lexbuf (* ignore *) }

  (* traditional C comments *)
  | "/*"                   { ccomment lexbuf (* ignore *) }

  (* nested comments *)
  | "(*"                   { ocamlcomment 1 lexbuf (* ignore *) }

  (* javadoc comments *)
  | "/**"                  { JAVADOC(javadoc lexbuf) }

  (* whitespace *)
  | WhiteSpace             { tokenize lexbuf (* ignore *) }

  (* identifiers *)
  | Identifier as s        { IDENTIFIER s }

  | eof                    { EOF } 

  (* string literals *)
  | '"' (StringCharacter* as s)'"' { STRING_LITERAL s }

  (* error fallback *)
  | _ as c                 { failwith ("Illegal character \"" ^ String.make 1 c) }

and ccomment = parse
  | _                      { ccomment lexbuf }
  | "*/"                   { tokenize lexbuf }
  | eof                    { failwith "unterminated comment" }

and ocamlcomment depth = parse
  | _                      { ocamlcomment depth lexbuf }
  | "(*"                   { ocamlcomment (depth + 1) lexbuf  }
  | "*)"                   { if depth <= 1 then tokenize lexbuf
                             else ocamlcomment (depth - 1) lexbuf }
  | eof                    { failwith "unterminated comment" }

and javadoc = parse
  | (Space* as s) "*/"     { s }
  (* empty line *)
  | LineTerminator         { "\n" ^ javadoc lexbuf }
  (* if a line starting with any number of whitespaces followed by * is detected *)
  | Space* "*"             { javadocline lexbuf }
  (* else *)
  | _ as c                 { (String.make 1 c) ^ javadocline lexbuf }
  | eof                    { failwith "unterminated comment" }

and javadocline = parse
  | "*/"                   { "" }
  (* keep characters up to end of line *)
  | LineTerminator         { "\n" ^ javadoc lexbuf }
  | _ as c                 { (String.make 1 c) ^ javadocline lexbuf }
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

