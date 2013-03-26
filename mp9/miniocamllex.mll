{
    open Miniocamlparse
}

(* definitions for letters and alpha-numeric chars *)
let letter = ['A' - 'Z' 'a' - 'z' '_']
let lcletter = ['a' - 'z']
let letterdigit = letter | ['0' - '9']

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

(* identifiers *)
let Identifier = lcletter letterdigit*

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
  | "let"           { LET }
  | "in"            { IN }
  | "rec"           { REC }
  | "fun"           { FUN }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "ref"           { REF }
  | "not"           { NOT }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "hd"            { HD }
  | "tl"            { TL }
  | "fst"           { FST }
  | "snd"           { SND }
  | "::"            { CONS }

  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "["             { LBRACK }
  | "]"             { RBRACK }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "->"            { RIGHTARROW }

  | "="             { EQ }
  | "<"             { LT }
  | ">"             { GT }
  | "<>"            { NEQ }
  | "!"             { DEREF }
  | ":="            { ASSIGN }
  | "&&"            { ANDAND }
  | "||"            { OROR }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
(*  | "+."            { FPLUS }
  | "-."            { FMINUS }
  | "*."            { FMULT }
  | "/."            { FDIV } *)
  | "^"             { CARET }
  | "@"             { AT }

  (* numeric literals *)
  | DecIntegerLiteral as s { INTEGER_LITERAL(int_of_string s) }

  | FloatLiteral as s    { FLOAT_LITERAL(float_of_string s) }

  (* end-of-line comments *)
  | EndOfLineComment       { tokenize lexbuf (* ignore *) }

  (* nested comments *)
  | "(*"                   { ocamlcomment 1 lexbuf (* ignore *) }

  (* whitespace *)
  | WhiteSpace             { tokenize lexbuf (* ignore *) }

  (* identifiers *)
  | Identifier as s        { IDENT s }

  | eof                    { EOF } 

  (* string literals *)
  | '"' (StringCharacter* as s)'"' { STRING_LITERAL s }

  (* error fallback *)
  | _ as c                 { failwith ("Illegal character \"" ^ String.make 1 c) }

and ocamlcomment depth = parse
  | _                      { ocamlcomment depth lexbuf }
  | "(*"                   { ocamlcomment (depth + 1) lexbuf  }
  | "*)"                   { if depth <= 1 then tokenize lexbuf
                             else ocamlcomment (depth - 1) lexbuf }
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

