{
  open Minijavaparse
}

(* definitions for letters and alpha-numeric chars *)
let jletter = ['A' - 'Z'] | ['a' - 'z']
let jletterdigit = jletter | ['0' - '9']

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

(* main character classes *)
let LineTerminator = (['\r' '\n'] | "\r\n")
let InputCharacter = [^'\r' '\n']

let WhiteSpace = LineTerminator | ['\t' ' ']

(* comments *)
let EndOfLineComment = "//" InputCharacter* LineTerminator

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
  | "abstract"      { ABSTRACT }
  | "boolean"       { BOOLEAN }
  | "break"         { BREAK }
  | "byte"          { BYTE }
  | "case"          { CASE }
  | "catch"         { CATCH }
  | "char"          { CHAR }
  | "class"         { CLASS }
  | "const"         { CONST }
  | "continue"      { CONTINUE }
  | "do"            { DO }
  | "double"        { DOUBLE }
  | "else"          { ELSE }
  | "extends"       { EXTENDS }
  | "final"         { FINAL }
  | "finally"       { FINALLY }
  | "float"         { FLOAT }
  | "for"           { FOR }
  | "default"       { DEFAULT }
  | "implements"    { IMPLEMENTS }
  | "import"        { IMPORT }
  | "instanceof"    { INSTANCEOF }
  | "int"           { INT }
  | "interface"     { INTERFACE }
  | "long"          { LONG }
  | "native"        { NATIVE }
  | "new"           { NEW }
  | "goto"          { GOTO }
  | "if"            { IF }
  | "public"        { PUBLIC }
  | "short"         { SHORT }
  | "super"         { SUPER }
  | "switch"        { SWITCH }
  | "synchronized"  { SYNCHRONIZED }
  | "package"       { PACKAGE }
  | "private"       { PRIVATE }
  | "protected"     { PROTECTED }
  | "transient"     { TRANSIENT }
  | "return"        { RETURN }
  | "void"          { VOID }
  | "static"        { STATIC }
  | "while"         { WHILE }
  | "this"          { THIS }
  | "throw"         { THROW }
  | "throws"        { THROWS }
  | "try"           { TRY }
  | "volatile"      { VOLATILE }
  | "strictfp"      { STRICTFP }

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
  | ">"             { GT }
  | "<"             { LT }
  | "!"             { NOT }
  | "~"             { COMP }
  | "?"             { QUESTION }
  | ":"             { COLON }
  | "=="            { EQEQ }
  | "<="            { LTEQ }
  | ">="            { GTEQ }
  | "!="            { NOTEQ }
  | "&&"            { ANDAND }
  | "||"            { OROR }
  | "++"            { PLUSPLUS }
  | "--"            { MINUSMINUS }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { MULT }
  | "/"             { DIV }
  | "&"             { AND }
  | "|"             { OR }
  | "^"             { XOR }
  | "%"             { MOD }
  | "<<"            { LSHIFT }
  | ">>"            { RSHIFT }
  | ">>>"           { URSHIFT }
  | "+="            { PLUSEQ }
  | "-="            { MINUSEQ }
  | "*="            { MULTEQ }
  | "/="            { DIVEQ }
  | "&="            { ANDEQ }
  | "|="            { OREQ }
  | "^="            { XOREQ }
  | "%="            { MODEQ }
  | "<<="           { LSHIFTEQ }
  | ">>="           { RSHIFTEQ }
  | ">>>="          { URSHIFTEQ }

  (* numeric literals *)
  | DecIntegerLiteral as s { INTEGER_LITERAL(int_of_string s) }

  | (FloatLiteral as s)    { FLOAT_LITERAL(float_of_string s) }

  (* end-of-line comments *)
  | EndOfLineComment       { tokenize lexbuf (* ignore *) }

  (* traditional C comments *)
  | "/*"                   { ccomment lexbuf (* ignore *) }

  (* nested comments *)
  | "(*"                   { ocamlcomment 1 lexbuf (* ignore *) }

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

(* do not modify this function: *)
{ let lextest s = tokenize (Lexing.from_string s)

  let get_all_tokens s =
      let b = Lexing.from_string (s^"\n") in
      let rec g () = 
      match tokenize b with EOF -> []
      | t -> t :: g () in
      g ()
 }

