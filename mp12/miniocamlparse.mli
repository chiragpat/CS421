type token =
  | INTEGER_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | IDENT of (string)
  | EOF
  | LET
  | IN
  | REC
  | FUN
  | TRUE
  | FALSE
  | REF
  | IF
  | THEN
  | ELSE
  | HD
  | TL
  | FST
  | SND
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | SEMICOLON
  | COMMA
  | RIGHTARROW
  | EQ
  | LT
  | GT
  | NEQ
  | DEREF
  | ASSIGN
  | CONS
  | ANDAND
  | OROR
  | PLUS
  | MINUS
  | MULT
  | DIV
  | CARET
  | AT
  | NOT
  | APP
  | COLON

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Miniocamlast.exp
