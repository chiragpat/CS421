type token =
  | INTEGER_LITERAL of (int)
  | LONG_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | DOUBLE_LITERAL of (float)
  | BOOLEAN_LITERAL of (bool)
  | CHARACTER_LITERAL of (char)
  | STRING_LITERAL of (string)
  | IDENTIFIER of (string)
  | JAVADOC of (string)
  | EOF
  | BOOLEAN
  | BREAK
  | CLASS
  | CONTINUE
  | ELSE
  | EXTENDS
  | FLOAT
  | DEFAULT
  | INT
  | NEW
  | IF
  | PUBLIC
  | RETURN
  | STATIC
  | WHILE
  | THIS
  | NULL_LITERAL
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMICOLON
  | COMMA
  | DOT
  | EQ
  | LT
  | GT
  | EQEQ
  | LTEQ
  | GTEQ
  | NOT
  | COLON
  | ANDAND
  | OROR
  | PLUS
  | MINUS
  | MULT
  | DIV
  | AND
  | OR

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Minijavaast.program
