type token =
  | INTEGER_LITERAL of (int)
  | LONG_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | DOUBLE_LITERAL of (float)
  | BOOLEAN_LITERAL of (bool)
  | CHARACTER_LITERAL of (char)
  | STRING_LITERAL of (string)
  | IDENTIFIER of (string)
  | EOF
  | ABSTRACT
  | BOOLEAN
  | BREAK
  | BYTE
  | CASE
  | CATCH
  | CHAR
  | CLASS
  | CONST
  | CONTINUE
  | DO
  | DOUBLE
  | ELSE
  | EXTENDS
  | FINAL
  | FINALLY
  | FLOAT
  | FOR
  | DEFAULT
  | IMPLEMENTS
  | IMPORT
  | INSTANCEOF
  | INT
  | INTERFACE
  | LONG
  | NATIVE
  | NEW
  | GOTO
  | IF
  | PUBLIC
  | SHORT
  | SUPER
  | SWITCH
  | SYNCHRONIZED
  | PACKAGE
  | PRIVATE
  | PROTECTED
  | TRANSIENT
  | RETURN
  | VOID
  | STATIC
  | WHILE
  | THIS
  | THROW
  | THROWS
  | TRY
  | VOLATILE
  | STRICTFP
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
  | GT
  | LT
  | NOT
  | COMP
  | QUESTION
  | COLON
  | EQEQ
  | LTEQ
  | GTEQ
  | NOTEQ
  | ANDAND
  | OROR
  | PLUSPLUS
  | MINUSMINUS
  | PLUS
  | MINUS
  | MULT
  | DIV
  | AND
  | OR
  | XOR
  | MOD
  | LSHIFT
  | RSHIFT
  | URSHIFT
  | PLUSEQ
  | MINUSEQ
  | MULTEQ
  | DIVEQ
  | ANDEQ
  | OREQ
  | XOREQ
  | MODEQ
  | LSHIFTEQ
  | RSHIFTEQ
  | URSHIFTEQ

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mp2common.program
val classdecl :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mp2common.class_decl
val stmt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mp2common.statement
val expression :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mp2common.exp
