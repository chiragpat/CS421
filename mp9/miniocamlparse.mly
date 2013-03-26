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
%token    APP

/* DO NOT CHANGE THE NAME OR THE TYPE OF THE MAIN NONTERMINAL */
%start program
%type <Miniocamlast.exp> program

/* ADD PRECEDENCES AND ASSOCIATIVITY HERE */
%right APP
%nonassoc IN RIGHTARROW ELSE
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
    exp EOF                     { $1 }

exp:
    exp SEMICOLON exp           { Operation($1,Semicolon,$3) }
  | exp COMMA exp               { Operation($1,Comma,$3) }
  | exp EQ exp                  { Operation($1,Equals,$3) }
  | exp LT exp                  { Operation($1,LessThan,$3) }
  | exp GT exp                  { Operation($1,GreaterThan,$3) }
  | exp NEQ exp                 { Operation($1,NotEquals,$3) }
  | exp ASSIGN exp              { Operation($1,Assign,$3) }
  | exp ANDAND exp              { Operation($1,And,$3) }
  | exp OROR exp                { Operation($1,Or,$3) }
  | exp PLUS exp                { Operation($1,Plus,$3) }
  | exp MINUS exp               { Operation($1,Minus,$3) }
  | exp MULT exp                { Operation($1,Mult,$3) }
  | exp DIV exp                 { Operation($1,Div,$3) }
/*  | exp PLUS exp                { Operation($1,IntPlus,$3) }
  | exp MINUS exp               { Operation($1,IntMinus,$3) }
  | exp MULT exp                { Operation($1,IntMult,$3) }
  | exp DIV exp                 { Operation($1,IntDiv,$3) }
  | exp FPLUS exp               { Operation($1,FloatPlus,$3) }
  | exp FMINUS exp              { Operation($1,FloatMinus,$3) }
  | exp FMULT exp               { Operation($1,FloatMult,$3) }
  | exp FDIV exp                { Operation($1,FloatDiv,$3) } */
  | exp CARET exp               { Operation($1,StringAppend,$3) }
  | exp AT exp                  { Operation($1,ListAppend,$3) }
  | exp CONS exp                { Operation($1,Cons,$3) }
  | REF exp                     { UnaryOperation(Ref,$2) }
  | DEREF exp                   { UnaryOperation(Deref,$2) }
  | NOT exp                     { UnaryOperation(Not,$2) }
  | HD exp                      { UnaryOperation(Head,$2) }
  | TL exp                      { UnaryOperation(Tail,$2) }
  | FST exp                     { UnaryOperation(Fst,$2) }
  | SND exp                     { UnaryOperation(Snd,$2) }
  | IDENT                       { Var $1 }
  | INTEGER_LITERAL             { IntConst $1 }
 /* | FLOAT_LITERAL               { FloatConst $1 } */
  | STRING_LITERAL              { StrConst $1 }
  | TRUE                        { True }
  | FALSE                       { False }
  | LBRACK exp RBRACK           { List2 $2 }
  | LBRACK RBRACK               { List [] }
  | LPAREN exp RPAREN           { Tuple2 $2 }
  | LET def IN exp              { let (id,args,e) = $2 in
                                  Let2(id,args,e,$4) }
  | LET REC def IN exp          { let (id,args,e) = $3 in
                                  Rec2(id,args,e,$5) }
  | FUN IDENT RIGHTARROW exp    { Fun($2,$4) }
  | exp exp %prec APP           { App($1,$2) }
  | IF exp THEN exp ELSE exp    { If($2,$4,$6) }

def:
  IDENT args EQ exp             { ($1,$2,$4) }

args:
                                { [] }
  | args IDENT                  { $1 @ [$2] }
