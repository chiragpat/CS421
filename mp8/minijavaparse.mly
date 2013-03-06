
%{
    open Minijavaast
%}

/* DO NOT CHANGE TOKEN DEFINITIONS */

%token <int> INTEGER_LITERAL LONG_LITERAL
%token <float> FLOAT_LITERAL DOUBLE_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <char> CHARACTER_LITERAL
%token <string> STRING_LITERAL IDENTIFIER
%token <int> INTEGER_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <string> STRING_LITERAL IDENTIFIER JAVADOC
%token EOF
  BOOLEAN BREAK CLASS CONTINUE
  ELSE EXTENDS FLOAT DEFAULT
  INT NEW IF PUBLIC
  RETURN STATIC WHILE THIS
  NULL_LITERAL LPAREN RPAREN LBRACE RBRACE
  LBRACK RBRACK SEMICOLON COMMA DOT
  EQ LT GT EQEQ LTEQ GTEQ
  NOT COLON ANDAND OROR
  PLUS MINUS MULT DIV AND OR

/* DO NOT CHANGE THE NAME OR THE TYPE OF THE MAIN NONTERMINAL */
%start program
%type <Minijavaast.program> program
%right ELSE /* Keep this directive, or you'll get a conflict */

/* ADD PRECEDENCES AND ASSOCIATIVITY HERE */
%left OR
%left AND 
%nonassoc LT LTEQ GT GTEQ EQEQ
%left PLUS MINUS
%left MULT DIV
%nonassoc NOT
%nonassoc LBRACK DOT

%%

program:
    classdecls		{ Program $1 }

classdecls:
    classdecl				{ [$1] }
  | classdecls classdecl	{ $1 @ [$2] }

classdecl:
    CLASS IDENTIFIER LBRACE staticvardecls methoddecls RBRACE 					{ Class($2, "", $4, $5) }
  | CLASS IDENTIFIER EXTENDS IDENTIFIER LBRACE staticvardecls methoddecls RBRACE 	{ Class($2, $4, $6, $7) }

staticvardecls:
    /* epsilon */               { [] }
  | staticvardecls staticvardecl { $1 @ [$2] }

staticvardecl:
   vardecl         { (NonStatic, $1) }
 | STATIC vardecl  { (Static, $2) }

vardecls:
    /* epsilon */		{ [] }
  | vardecls vardecl	{ $1 @ [$2] }

vardecl:
    exp_type IDENTIFIER SEMICOLON			{ Var($1, $2) } 

methoddecls:
    /* epsilon */			{ [] }
  | methoddecls methoddecl	{ $1 @ [$2] }

methoddecl:
    PUBLIC exp_type IDENTIFIER LPAREN params RPAREN LBRACE vardecls RETURN expression SEMICOLON RBRACE	{ Method($2, $3, $5, $8, [], $10) }
  | PUBLIC exp_type IDENTIFIER LPAREN params RPAREN LBRACE vardecls stmts1 RETURN expression SEMICOLON RBRACE	{ Method($2, $3, $5, $8, $9, $11) }

params:
    /* epsilon */					{ [] }
  | exp_type IDENTIFIER more_params	{ Var($1, $2) :: $3 }

more_params:
    /* epsilon */							{ [] }
  | COMMA exp_type IDENTIFIER more_params	{ Var($2, $3) :: $4 }

exp_type:
    BOOLEAN					{ BoolType }
  | FLOAT					{ FloatType }
  | INT						{ IntType }
  | IDENTIFIER                                  { ObjectType($1) }

stmts1:
    stmt		{ [$1] }
  | stmts1 stmt		{ $1 @ [$2] }

stmts0:
    /* epsilon */	{ [] }
  | stmts0 stmt		{ $1 @ [$2] }

stmt:
    LBRACE stmts0 RBRACE		{ Block $2 }
  | IDENTIFIER EQ expression SEMICOLON		{ Assignment($1, $3) }
  | IF LPAREN expression RPAREN stmt ELSE stmt	{ If($3, $5, $7) }
  | IF LPAREN expression RPAREN stmt %prec ELSE { If($3, $5, Block []) }
  | WHILE LPAREN expression RPAREN stmt		{ While ($3, $5) }
  | IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER LPAREN expression RPAREN SEMICOLON
{
	if $1="System" && $3="out" && $5="println" then Println $7
	else raise Parsing.Parse_error
  	}
  | BREAK SEMICOLON		{ Break }
  | CONTINUE SEMICOLON	{ Continue }
  | IDENTIFIER LBRACK expression RBRACK EQ expression SEMICOLON	{ ArrayAssignment($1, $3, $6) }

expression:
    expression AND expression	{ Operation($1, And, $3) }
  | expression OR expression	{ Operation($1, Or, $3) }
  | expression LT expression	{ Operation($1, LessThan, $3) }
  | expression GT expression    { Operation($1, GreaterThan, $3) }
  | expression EQEQ expression  { Operation($1, Equal, $3) }
  | expression LTEQ expression  { Operation($1, LessThanEq, $3) }
  | expression GTEQ expression  { Operation($1, GreaterThanEq, $3) }
  | expression PLUS expression	{ Operation($1, Plus, $3) }
  | expression MINUS expression	{ Operation($1, Minus, $3) }
  | expression MULT expression	{ Operation($1, Multiplication, $3) }
  | expression DIV expression	{ Operation($1, Division, $3) }
  | expression DOT IDENTIFIER	{ if $3="length" then Length $1 else FieldRef($1, $3) }
  | expression DOT IDENTIFIER LPAREN args RPAREN { MethodCall($1, $3, $5) }
  | INTEGER_LITERAL		{ Integer $1 }
  | FLOAT_LITERAL		{ Float $1 }
  | STRING_LITERAL		{ String $1 }
  | NULL_LITERAL		{ Null }
  | BOOLEAN_LITERAL 	        { if $1 then True else False }
  | IDENTIFIER			{ Id $1 }
  | THIS			{ This }
  | NEW IDENTIFIER LPAREN RPAREN 			{ NewId $2 }
  | NOT expression 				{ Not $2 } 
  | LPAREN expression RPAREN	{ $2 }

args:
    /* epsilon */			{ [] }
  | expression more_args	{ $1 :: $2 }

more_args:
    /* epsilon */				{ [] }
  | COMMA expression more_args	{ $2 :: $3 }

  /* Do NOT remove the %prec directive below 
     Otherwise you'll get a conflict and lose points
  */


