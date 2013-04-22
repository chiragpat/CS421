%{
  open Miniocamlast
%}

/* DO NOT CHANGE TOKEN DEFINITIONS */

%token <int> INTEGER_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL IDENT
%token     EOF
%token    LET IN REC FUN TRUE FALSE REF
%token    IF THEN ELSE
%token    HD TL FST SND
%token    LPAREN RPAREN LBRACK RBRACK SEMICOLON COMMA
%token    RIGHTARROW EQ LT GT NEQ DEREF ASSIGN CONS
%token    ANDAND OROR PLUS MINUS MULT DIV
%token    CARET AT NOT
%token    APP COLON

/* DO NOT CHANGE THE NAME OR THE TYPE OF THE MAIN NONTERMINAL */
%start program
%type <Miniocamlast.exp> program

/* ADD PRECEDENCES AND ASSOCIATIVITY HERE */
%right APP
%nonassoc IN ELSE RIGHTARROW
%nonassoc INTEGER_LITERAL FLOAT_LITERAL STRING_LITERAL IDENT
%nonassoc LET FUN TRUE FALSE IF LPAREN LBRACK
%right SEMICOLON COMMA ASSIGN
%left OROR
%left ANDAND
%nonassoc LT GT EQ NEQ
%left PLUS MINUS FPLUS FMINUS
%left MULT DIV FMULT FDIV
%left CARET AT
%nonassoc REF NOT DEREF HD TL FST SND

%%

program:
    exp EOF                      { $1 }

exp:
    exp LT exp                   { Operation($1,LessThan,$3) }
  | exp GT exp                   { Operation($1,GreaterThan,$3) }
  | exp ANDAND exp               { Operation($1,And,$3) }
  | exp OROR exp                 { Operation($1,Or,$3) }
  | exp PLUS exp                 { Operation($1,Plus,$3) }
  | exp MINUS exp                { Operation($1,Minus,$3) }
  | exp MULT exp                 { Operation($1,Mult,$3) }
  | exp DIV exp                  { Operation($1,Div,$3) }
  | IDENT                        { Var $1 }
  | IDENT LBRACK exp_type RBRACK { PolyVar($1,$3) }
  | IDENT LBRACK exp_type RIGHTARROW exp_type RBRACK
                                 { PolyVar($1, FunType($3, $5)) }
  | INTEGER_LITERAL              { IntConst $1 }
  | TRUE                         { True }
  | FALSE                        { False }
  | LET IDENT COLON exp_type EQ exp IN exp
                                 { Let($2,$4,$6,$8) }
  | FUN IDENT COLON exp_type RIGHTARROW exp
                                 { Fun($2,$4,$6) }
  | exp exp %prec APP            { App($1,$2) }
  | LPAREN exp RPAREN            { $2 }

exp_type:
    IDENT                        { if $1 = "bool" then BoolType else if $1 = "int" then IntType else Typevar($1) }
  | LPAREN exp_type RIGHTARROW exp_type RPAREN { FunType($2, $4) }
