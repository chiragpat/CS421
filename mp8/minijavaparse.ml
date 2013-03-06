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

open Parsing;;
let _ = parse_error;;
# 2 "minijavaparse.mly"

    open Minijavaast
# 61 "minijavaparse.ml"
let yytransl_const = [|
    0 (* EOF *);
  266 (* BOOLEAN *);
  267 (* BREAK *);
  268 (* CLASS *);
  269 (* CONTINUE *);
  270 (* ELSE *);
  271 (* EXTENDS *);
  272 (* FLOAT *);
  273 (* DEFAULT *);
  274 (* INT *);
  275 (* NEW *);
  276 (* IF *);
  277 (* PUBLIC *);
  278 (* RETURN *);
  279 (* STATIC *);
  280 (* WHILE *);
  281 (* THIS *);
  282 (* NULL_LITERAL *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* LBRACE *);
  286 (* RBRACE *);
  287 (* LBRACK *);
  288 (* RBRACK *);
  289 (* SEMICOLON *);
  290 (* COMMA *);
  291 (* DOT *);
  292 (* EQ *);
  293 (* LT *);
  294 (* GT *);
  295 (* EQEQ *);
  296 (* LTEQ *);
  297 (* GTEQ *);
  298 (* NOT *);
  299 (* COLON *);
  300 (* ANDAND *);
  301 (* OROR *);
  302 (* PLUS *);
  303 (* MINUS *);
  304 (* MULT *);
  305 (* DIV *);
  306 (* AND *);
  307 (* OR *);
    0|]

let yytransl_block = [|
  257 (* INTEGER_LITERAL *);
  258 (* LONG_LITERAL *);
  259 (* FLOAT_LITERAL *);
  260 (* DOUBLE_LITERAL *);
  261 (* BOOLEAN_LITERAL *);
  262 (* CHARACTER_LITERAL *);
  263 (* STRING_LITERAL *);
  264 (* IDENTIFIER *);
  265 (* JAVADOC *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\006\000\008\000\008\000\007\000\005\000\005\000\010\000\010\000\
\011\000\011\000\014\000\014\000\009\000\009\000\009\000\009\000\
\013\000\013\000\016\000\016\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\017\000\017\000\018\000\018\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\006\000\008\000\000\000\002\000\001\000\
\002\000\000\000\002\000\003\000\000\000\002\000\012\000\013\000\
\000\000\003\000\000\000\004\000\001\000\001\000\001\000\001\000\
\001\000\002\000\000\000\002\000\003\000\004\000\007\000\005\000\
\005\000\009\000\002\000\002\000\007\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\006\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\004\000\002\000\003\000\000\000\002\000\000\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\065\000\000\000\002\000\000\000\003\000\
\000\000\006\000\000\000\000\000\006\000\024\000\021\000\022\000\
\023\000\000\000\000\000\007\000\008\000\000\000\000\000\009\000\
\000\000\004\000\014\000\000\000\000\000\000\000\012\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\011\000\000\000\025\000\020\000\000\000\
\000\000\000\000\035\000\036\000\000\000\051\000\052\000\055\000\
\053\000\056\000\000\000\057\000\054\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\028\000\000\000\000\000\000\000\030\000\
\000\000\000\000\060\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\058\000\000\000\033\000\
\016\000\000\000\000\000\000\000\000\000\000\000\037\000\000\000\
\031\000\000\000\062\000\050\000\000\000\000\000\034\000\064\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\019\000\020\000\021\000\043\000\
\022\000\027\000\036\000\072\000\053\000\040\000\054\000\074\000\
\134\000\139\000"

let yysindex = "\005\000\
\004\255\000\000\010\255\000\000\004\255\000\000\029\255\000\000\
\013\255\000\000\014\255\041\255\000\000\000\000\000\000\000\000\
\000\000\038\000\091\255\000\000\000\000\037\255\041\255\000\000\
\038\000\000\000\000\000\030\255\116\255\038\255\000\000\000\000\
\020\255\038\000\057\255\040\255\036\255\048\255\038\000\000\000\
\000\000\068\255\075\000\036\255\085\000\050\255\074\255\081\255\
\047\255\086\255\000\000\000\000\090\000\000\000\000\000\047\255\
\122\255\047\255\000\000\000\000\047\255\000\000\000\000\000\000\
\000\000\000\000\135\255\000\000\000\000\047\255\047\255\110\000\
\047\255\060\000\085\000\047\255\000\000\091\000\097\255\129\000\
\142\255\117\255\159\255\120\255\126\255\152\255\047\255\047\255\
\047\255\047\255\047\255\047\255\047\255\047\255\047\255\047\255\
\047\255\176\255\000\000\000\000\148\000\131\255\153\255\000\000\
\098\000\140\255\000\000\000\000\151\255\049\255\049\255\049\255\
\049\255\049\255\071\255\071\255\120\255\120\255\218\000\202\000\
\098\000\139\255\047\255\157\255\171\255\000\000\047\255\000\000\
\000\000\167\000\047\255\098\000\185\000\158\255\000\000\193\255\
\000\000\047\255\000\000\000\000\162\255\185\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\201\000\000\000\000\000\000\000\
\000\000\000\000\000\000\124\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\124\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\174\255\000\000\000\000\175\255\000\000\000\000\000\000\
\000\000\000\000\000\000\175\255\210\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\077\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\053\255\232\254\235\254\028\255\
\001\000\008\000\217\255\237\255\101\255\125\255\011\000\247\254\
\000\000\000\000\000\000\000\000\052\000\000\000\184\255\000\000\
\000\000\000\000\000\000\000\000\191\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\191\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\215\000\216\000\212\000\000\000\241\255\000\000\
\097\000\000\000\000\000\200\255\000\000\192\000\204\255\000\000\
\000\000\095\000"

let yytablesize = 523
let yytable = "\078\000\
\077\000\080\000\024\000\040\000\081\000\001\000\041\000\040\000\
\040\000\040\000\041\000\041\000\041\000\083\000\084\000\003\000\
\098\000\007\000\039\000\101\000\011\000\100\000\039\000\039\000\
\039\000\040\000\040\000\052\000\041\000\041\000\110\000\111\000\
\112\000\113\000\114\000\115\000\116\000\117\000\118\000\119\000\
\120\000\039\000\013\000\009\000\028\000\033\000\034\000\062\000\
\014\000\063\000\015\000\064\000\125\000\065\000\066\000\042\000\
\016\000\010\000\017\000\042\000\042\000\042\000\031\000\018\000\
\037\000\067\000\130\000\038\000\128\000\039\000\133\000\068\000\
\069\000\070\000\136\000\044\000\041\000\042\000\042\000\137\000\
\049\000\142\000\059\000\086\000\049\000\049\000\049\000\049\000\
\071\000\049\000\049\000\049\000\049\000\049\000\092\000\093\000\
\094\000\095\000\049\000\049\000\049\000\049\000\049\000\049\000\
\059\000\086\000\060\000\061\000\059\000\059\000\059\000\025\000\
\073\000\059\000\059\000\059\000\059\000\059\000\094\000\095\000\
\026\000\030\000\059\000\059\000\059\000\059\000\059\000\059\000\
\047\000\079\000\035\000\103\000\047\000\047\000\047\000\042\000\
\025\000\047\000\047\000\047\000\047\000\047\000\082\000\106\000\
\013\000\032\000\047\000\047\000\047\000\047\000\047\000\047\000\
\048\000\013\000\086\000\108\000\048\000\048\000\048\000\109\000\
\124\000\048\000\048\000\048\000\048\000\048\000\123\000\126\000\
\129\000\105\000\048\000\048\000\048\000\048\000\048\000\048\000\
\086\000\127\000\087\000\088\000\089\000\090\000\091\000\131\000\
\132\000\140\000\107\000\092\000\093\000\094\000\095\000\096\000\
\097\000\086\000\143\000\087\000\088\000\089\000\090\000\091\000\
\001\000\017\000\019\000\121\000\092\000\093\000\094\000\095\000\
\096\000\097\000\086\000\061\000\087\000\088\000\089\000\090\000\
\091\000\024\000\063\000\008\000\141\000\092\000\093\000\094\000\
\095\000\096\000\097\000\086\000\023\000\087\000\088\000\089\000\
\090\000\091\000\029\000\055\000\144\000\000\000\092\000\093\000\
\094\000\095\000\096\000\097\000\045\000\000\000\000\000\000\000\
\045\000\045\000\045\000\000\000\000\000\045\000\045\000\045\000\
\045\000\045\000\000\000\000\000\000\000\000\000\045\000\045\000\
\046\000\000\000\045\000\045\000\046\000\046\000\046\000\000\000\
\000\000\046\000\046\000\046\000\046\000\046\000\000\000\000\000\
\000\000\000\000\046\000\046\000\043\000\000\000\046\000\046\000\
\043\000\043\000\043\000\044\000\000\000\000\000\038\000\044\000\
\044\000\044\000\038\000\038\000\038\000\014\000\000\000\015\000\
\000\000\000\000\043\000\043\000\000\000\016\000\000\000\017\000\
\000\000\044\000\044\000\032\000\038\000\038\000\032\000\000\000\
\032\000\000\000\000\000\075\000\000\000\000\000\046\000\032\000\
\047\000\032\000\000\000\032\000\000\000\000\000\000\000\048\000\
\032\000\032\000\045\000\050\000\015\000\046\000\000\000\047\000\
\051\000\099\000\016\000\000\000\017\000\000\000\048\000\000\000\
\049\000\075\000\050\000\000\000\046\000\000\000\047\000\051\000\
\000\000\075\000\000\000\000\000\046\000\048\000\047\000\076\000\
\000\000\050\000\000\000\056\000\000\000\048\000\051\000\057\000\
\058\000\050\000\102\000\000\000\000\000\086\000\051\000\087\000\
\088\000\089\000\090\000\091\000\000\000\000\000\000\000\000\000\
\092\000\093\000\094\000\095\000\096\000\097\000\085\000\000\000\
\086\000\000\000\087\000\088\000\089\000\090\000\091\000\000\000\
\000\000\000\000\000\000\092\000\093\000\094\000\095\000\096\000\
\097\000\104\000\000\000\086\000\000\000\087\000\088\000\089\000\
\090\000\091\000\000\000\000\000\000\000\000\000\092\000\093\000\
\094\000\095\000\096\000\097\000\122\000\000\000\086\000\000\000\
\087\000\088\000\089\000\090\000\091\000\000\000\000\000\000\000\
\000\000\092\000\093\000\094\000\095\000\096\000\097\000\135\000\
\000\000\086\000\000\000\087\000\088\000\089\000\090\000\091\000\
\000\000\000\000\000\000\000\000\092\000\093\000\094\000\095\000\
\096\000\097\000\138\000\086\000\000\000\087\000\088\000\089\000\
\090\000\091\000\000\000\000\000\000\000\000\000\092\000\093\000\
\094\000\095\000\096\000\097\000\086\000\000\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000\096\000\086\000\000\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000"

let yycheck = "\056\000\
\053\000\058\000\018\000\028\001\061\000\001\000\028\001\032\001\
\033\001\034\001\032\001\033\001\034\001\070\000\071\000\012\001\
\073\000\008\001\028\001\076\000\008\001\074\000\032\001\033\001\
\034\001\050\001\051\001\043\000\050\001\051\001\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\051\001\029\001\015\001\008\001\008\001\027\001\001\001\
\008\001\003\001\010\001\005\001\105\000\007\001\008\001\028\001\
\016\001\029\001\018\001\032\001\033\001\034\001\033\001\023\001\
\008\001\019\001\123\000\028\001\121\000\034\001\127\000\025\001\
\026\001\027\001\131\000\008\001\029\001\050\001\051\001\132\000\
\028\001\138\000\033\001\035\001\032\001\033\001\034\001\035\001\
\042\001\037\001\038\001\039\001\040\001\041\001\046\001\047\001\
\048\001\049\001\046\001\047\001\048\001\049\001\050\001\051\001\
\028\001\035\001\033\001\027\001\032\001\033\001\034\001\021\001\
\027\001\037\001\038\001\039\001\040\001\041\001\048\001\049\001\
\030\001\025\000\046\001\047\001\048\001\049\001\050\001\051\001\
\028\001\008\001\034\000\035\001\032\001\033\001\034\001\039\000\
\021\001\037\001\038\001\039\001\040\001\041\001\008\001\027\001\
\021\001\030\001\046\001\047\001\048\001\049\001\050\001\051\001\
\028\001\030\001\035\001\030\001\032\001\033\001\034\001\008\001\
\008\001\037\001\038\001\039\001\040\001\041\001\036\001\028\001\
\030\001\028\001\046\001\047\001\048\001\049\001\050\001\051\001\
\035\001\027\001\037\001\038\001\039\001\040\001\041\001\027\001\
\014\001\028\001\028\001\046\001\047\001\048\001\049\001\050\001\
\051\001\035\001\033\001\037\001\038\001\039\001\040\001\041\001\
\000\000\028\001\028\001\028\001\046\001\047\001\048\001\049\001\
\050\001\051\001\035\001\028\001\037\001\038\001\039\001\040\001\
\041\001\008\001\028\001\005\000\028\001\046\001\047\001\048\001\
\049\001\050\001\051\001\035\001\013\000\037\001\038\001\039\001\
\040\001\041\001\023\000\044\000\142\000\255\255\046\001\047\001\
\048\001\049\001\050\001\051\001\028\001\255\255\255\255\255\255\
\032\001\033\001\034\001\255\255\255\255\037\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\255\255\046\001\047\001\
\028\001\255\255\050\001\051\001\032\001\033\001\034\001\255\255\
\255\255\037\001\038\001\039\001\040\001\041\001\255\255\255\255\
\255\255\255\255\046\001\047\001\028\001\255\255\050\001\051\001\
\032\001\033\001\034\001\028\001\255\255\255\255\028\001\032\001\
\033\001\034\001\032\001\033\001\034\001\008\001\255\255\010\001\
\255\255\255\255\050\001\051\001\255\255\016\001\255\255\018\001\
\255\255\050\001\051\001\008\001\050\001\051\001\011\001\255\255\
\013\001\255\255\255\255\008\001\255\255\255\255\011\001\020\001\
\013\001\022\001\255\255\024\001\255\255\255\255\255\255\020\001\
\029\001\030\001\008\001\024\001\010\001\011\001\255\255\013\001\
\029\001\030\001\016\001\255\255\018\001\255\255\020\001\255\255\
\022\001\008\001\024\001\255\255\011\001\255\255\013\001\029\001\
\255\255\008\001\255\255\255\255\011\001\020\001\013\001\022\001\
\255\255\024\001\255\255\031\001\255\255\020\001\029\001\035\001\
\036\001\024\001\032\001\255\255\255\255\035\001\029\001\037\001\
\038\001\039\001\040\001\041\001\255\255\255\255\255\255\255\255\
\046\001\047\001\048\001\049\001\050\001\051\001\033\001\255\255\
\035\001\255\255\037\001\038\001\039\001\040\001\041\001\255\255\
\255\255\255\255\255\255\046\001\047\001\048\001\049\001\050\001\
\051\001\033\001\255\255\035\001\255\255\037\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\255\255\046\001\047\001\
\048\001\049\001\050\001\051\001\033\001\255\255\035\001\255\255\
\037\001\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\046\001\047\001\048\001\049\001\050\001\051\001\033\001\
\255\255\035\001\255\255\037\001\038\001\039\001\040\001\041\001\
\255\255\255\255\255\255\255\255\046\001\047\001\048\001\049\001\
\050\001\051\001\034\001\035\001\255\255\037\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\255\255\046\001\047\001\
\048\001\049\001\050\001\051\001\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001\050\001\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001"

let yynames_const = "\
  EOF\000\
  BOOLEAN\000\
  BREAK\000\
  CLASS\000\
  CONTINUE\000\
  ELSE\000\
  EXTENDS\000\
  FLOAT\000\
  DEFAULT\000\
  INT\000\
  NEW\000\
  IF\000\
  PUBLIC\000\
  RETURN\000\
  STATIC\000\
  WHILE\000\
  THIS\000\
  NULL_LITERAL\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  SEMICOLON\000\
  COMMA\000\
  DOT\000\
  EQ\000\
  LT\000\
  GT\000\
  EQEQ\000\
  LTEQ\000\
  GTEQ\000\
  NOT\000\
  COLON\000\
  ANDAND\000\
  OROR\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  AND\000\
  OR\000\
  "

let yynames_block = "\
  INTEGER_LITERAL\000\
  LONG_LITERAL\000\
  FLOAT_LITERAL\000\
  DOUBLE_LITERAL\000\
  BOOLEAN_LITERAL\000\
  CHARACTER_LITERAL\000\
  STRING_LITERAL\000\
  IDENTIFIER\000\
  JAVADOC\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'classdecls) in
    Obj.repr(
# 45 "minijavaparse.mly"
                ( Program _1 )
# 414 "minijavaparse.ml"
               : Minijavaast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'classdecl) in
    Obj.repr(
# 48 "minijavaparse.mly"
                 ( [_1] )
# 421 "minijavaparse.ml"
               : 'classdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'classdecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classdecl) in
    Obj.repr(
# 49 "minijavaparse.mly"
                         ( _1 @ [_2] )
# 429 "minijavaparse.ml"
               : 'classdecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'staticvardecls) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    Obj.repr(
# 52 "minijavaparse.mly"
                                                                   ( Class(_2, "", _4, _5) )
# 438 "minijavaparse.ml"
               : 'classdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'staticvardecls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    Obj.repr(
# 53 "minijavaparse.mly"
                                                                                  ( Class(_2, _4, _6, _7) )
# 448 "minijavaparse.ml"
               : 'classdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "minijavaparse.mly"
                                ( [] )
# 454 "minijavaparse.ml"
               : 'staticvardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'staticvardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'staticvardecl) in
    Obj.repr(
# 57 "minijavaparse.mly"
                                 ( _1 @ [_2] )
# 462 "minijavaparse.ml"
               : 'staticvardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 60 "minijavaparse.mly"
                   ( (NonStatic, _1) )
# 469 "minijavaparse.ml"
               : 'staticvardecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 61 "minijavaparse.mly"
                   ( (Static, _2) )
# 476 "minijavaparse.ml"
               : 'staticvardecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "minijavaparse.mly"
                   ( [] )
# 482 "minijavaparse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 65 "minijavaparse.mly"
                     ( _1 @ [_2] )
# 490 "minijavaparse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 68 "minijavaparse.mly"
                                    ( Var(_1, _2) )
# 498 "minijavaparse.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "minijavaparse.mly"
                    ( [] )
# 504 "minijavaparse.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methoddecl) in
    Obj.repr(
# 72 "minijavaparse.mly"
                           ( _1 @ [_2] )
# 512 "minijavaparse.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 7 : 'params) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'vardecls) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 75 "minijavaparse.mly"
                                                                                                       ( Method(_2, _3, _5, _8, [], _10) )
# 523 "minijavaparse.ml"
               : 'methoddecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 11 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : 'params) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'vardecls) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : 'stmts1) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 76 "minijavaparse.mly"
                                                                                                              ( Method(_2, _3, _5, _8, _9, _11) )
# 535 "minijavaparse.ml"
               : 'methoddecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "minijavaparse.mly"
                      ( [] )
# 541 "minijavaparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 80 "minijavaparse.mly"
                                    ( Var(_1, _2) :: _3 )
# 550 "minijavaparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "minijavaparse.mly"
                        ( [] )
# 556 "minijavaparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 84 "minijavaparse.mly"
                                          ( Var(_2, _3) :: _4 )
# 565 "minijavaparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "minijavaparse.mly"
                ( BoolType )
# 571 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "minijavaparse.mly"
              ( FloatType )
# 577 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "minijavaparse.mly"
             ( IntType )
# 583 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "minijavaparse.mly"
                                                ( ObjectType(_1) )
# 590 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "minijavaparse.mly"
          ( [_1] )
# 597 "minijavaparse.ml"
               : 'stmts1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts1) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "minijavaparse.mly"
                 ( _1 @ [_2] )
# 605 "minijavaparse.ml"
               : 'stmts1))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "minijavaparse.mly"
                  ( [] )
# 611 "minijavaparse.ml"
               : 'stmts0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts0) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 98 "minijavaparse.mly"
                 ( _1 @ [_2] )
# 619 "minijavaparse.ml"
               : 'stmts0))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts0) in
    Obj.repr(
# 101 "minijavaparse.mly"
                          ( Block _2 )
# 626 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 102 "minijavaparse.mly"
                                        ( Assignment(_1, _3) )
# 634 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "minijavaparse.mly"
                                               ( If(_3, _5, _7) )
# 643 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "minijavaparse.mly"
                                                ( If(_3, _5, Block []) )
# 651 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "minijavaparse.mly"
                                         ( While (_3, _5) )
# 659 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 107 "minijavaparse.mly"
(
	if _1="System" && _3="out" && _5="println" then Println _7
	else raise Parsing.Parse_error
  	)
# 672 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "minijavaparse.mly"
                     ( Break )
# 678 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "minijavaparse.mly"
                       ( Continue )
# 684 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 113 "minijavaparse.mly"
                                                                ( ArrayAssignment(_1, _3, _6) )
# 693 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 116 "minijavaparse.mly"
                              ( Operation(_1, And, _3) )
# 701 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 117 "minijavaparse.mly"
                             ( Operation(_1, Or, _3) )
# 709 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 118 "minijavaparse.mly"
                             ( Operation(_1, LessThan, _3) )
# 717 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 119 "minijavaparse.mly"
                                ( Operation(_1, GreaterThan, _3) )
# 725 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 120 "minijavaparse.mly"
                                ( Operation(_1, Equal, _3) )
# 733 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 121 "minijavaparse.mly"
                                ( Operation(_1, LessThanEq, _3) )
# 741 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 122 "minijavaparse.mly"
                                ( Operation(_1, GreaterThanEq, _3) )
# 749 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 123 "minijavaparse.mly"
                               ( Operation(_1, Plus, _3) )
# 757 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 124 "minijavaparse.mly"
                                ( Operation(_1, Minus, _3) )
# 765 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 125 "minijavaparse.mly"
                               ( Operation(_1, Multiplication, _3) )
# 773 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 126 "minijavaparse.mly"
                              ( Operation(_1, Division, _3) )
# 781 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 127 "minijavaparse.mly"
                              ( if _3="length" then Length _1 else FieldRef(_1, _3) )
# 789 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 128 "minijavaparse.mly"
                                                 ( MethodCall(_1, _3, _5) )
# 798 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "minijavaparse.mly"
                     ( Integer _1 )
# 805 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 130 "minijavaparse.mly"
                   ( Float _1 )
# 812 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "minijavaparse.mly"
                    ( String _1 )
# 819 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "minijavaparse.mly"
                  ( Null )
# 825 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 133 "minijavaparse.mly"
                             ( if _1 then True else False )
# 832 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "minijavaparse.mly"
                 ( Id _1 )
# 839 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "minijavaparse.mly"
           ( This )
# 845 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 136 "minijavaparse.mly"
                                    ( NewId _2 )
# 852 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 137 "minijavaparse.mly"
                       ( Not _2 )
# 859 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 138 "minijavaparse.mly"
                             ( _2 )
# 866 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "minijavaparse.mly"
                    ( [] )
# 872 "minijavaparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 142 "minijavaparse.mly"
                         ( _1 :: _2 )
# 880 "minijavaparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "minijavaparse.mly"
                     ( [] )
# 886 "minijavaparse.ml"
               : 'more_args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 146 "minijavaparse.mly"
                               ( _2 :: _3 )
# 894 "minijavaparse.ml"
               : 'more_args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Minijavaast.program)
