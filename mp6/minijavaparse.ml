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
\006\000\008\000\008\000\007\000\005\000\005\000\010\000\011\000\
\011\000\014\000\014\000\009\000\009\000\009\000\009\000\012\000\
\012\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\016\000\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\006\000\008\000\000\000\002\000\001\000\
\002\000\000\000\002\000\003\000\000\000\002\000\013\000\000\000\
\003\000\000\000\004\000\003\000\001\000\001\000\001\000\000\000\
\002\000\003\000\004\000\007\000\005\000\005\000\009\000\002\000\
\002\000\007\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\003\000\006\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\005\000\
\004\000\002\000\003\000\000\000\002\000\000\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\064\000\000\000\002\000\000\000\003\000\
\000\000\006\000\000\000\000\000\006\000\021\000\022\000\023\000\
\000\000\000\000\007\000\008\000\000\000\000\000\009\000\000\000\
\004\000\014\000\000\000\000\000\000\000\000\000\012\000\020\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\010\000\000\000\000\000\000\000\011\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\025\000\
\000\000\000\000\000\000\032\000\033\000\000\000\049\000\050\000\
\053\000\051\000\054\000\000\000\055\000\052\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\027\000\000\000\000\000\
\000\000\059\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\000\000\000\046\000\000\000\
\030\000\000\000\000\000\000\000\056\000\000\000\000\000\034\000\
\000\000\028\000\000\000\061\000\048\000\000\000\000\000\031\000\
\063\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\018\000\019\000\020\000\044\000\
\021\000\026\000\037\000\047\000\073\000\041\000\056\000\135\000\
\140\000"

let yysindex = "\018\000\
\253\254\000\000\017\255\000\000\253\254\000\000\249\254\000\000\
\052\255\000\000\034\255\063\255\000\000\000\000\000\000\000\000\
\107\255\239\254\000\000\000\000\251\254\063\255\000\000\107\255\
\000\000\000\000\029\255\040\255\025\255\013\255\000\000\000\000\
\000\000\060\255\107\255\033\255\071\255\066\255\072\255\107\255\
\000\000\000\000\049\255\107\255\066\255\000\000\129\000\000\000\
\176\255\069\255\077\255\084\255\042\255\085\255\000\000\000\000\
\042\255\108\255\042\255\000\000\000\000\042\255\000\000\000\000\
\000\000\000\000\000\000\179\255\000\000\000\000\042\255\042\255\
\144\000\042\255\115\000\165\000\089\255\186\000\153\255\099\255\
\103\255\178\255\241\254\042\255\106\255\127\255\042\255\042\255\
\042\255\042\255\042\255\042\255\042\255\042\255\042\255\042\255\
\042\255\202\255\000\000\104\255\133\255\000\000\149\000\119\255\
\051\255\000\000\207\000\000\000\121\255\074\001\074\001\074\001\
\074\001\074\001\008\000\008\000\241\254\241\254\055\001\035\001\
\149\000\042\255\122\255\136\255\000\000\228\000\000\000\042\255\
\000\000\249\000\042\255\149\000\000\000\014\001\130\255\000\000\
\226\255\000\000\042\255\000\000\000\000\126\255\014\001\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\160\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\137\255\000\000\000\000\143\255\000\000\000\000\
\000\000\000\000\000\000\130\000\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\081\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\255\034\000\041\000\044\000\
\054\000\065\000\250\255\014\000\105\255\129\255\068\000\234\254\
\000\000\000\000\000\000\100\000\000\000\000\000\000\000\144\255\
\000\000\000\000\000\000\000\000\000\000\145\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\145\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\159\000\161\000\163\000\000\000\240\255\000\000\
\239\255\000\000\000\000\127\000\199\255\138\000\177\255\000\000\
\043\000"

let yytablesize = 635
let yytable = "\076\000\
\023\000\078\000\027\000\024\000\079\000\036\000\030\000\009\000\
\003\000\036\000\036\000\036\000\025\000\082\000\083\000\084\000\
\098\000\036\000\001\000\086\000\034\000\010\000\043\000\124\000\
\007\000\028\000\107\000\046\000\036\000\110\000\111\000\112\000\
\113\000\114\000\115\000\116\000\117\000\118\000\119\000\120\000\
\038\000\129\000\063\000\028\000\064\000\024\000\065\000\126\000\
\066\000\067\000\081\000\063\000\138\000\064\000\033\000\065\000\
\045\000\066\000\067\000\011\000\068\000\031\000\013\000\028\000\
\130\000\013\000\069\000\070\000\071\000\068\000\134\000\032\000\
\014\000\137\000\013\000\069\000\070\000\071\000\015\000\028\000\
\016\000\143\000\032\000\072\000\047\000\017\000\035\000\047\000\
\047\000\047\000\047\000\047\000\072\000\047\000\047\000\047\000\
\047\000\047\000\039\000\040\000\042\000\060\000\047\000\047\000\
\047\000\047\000\047\000\047\000\058\000\061\000\062\000\074\000\
\058\000\058\000\058\000\077\000\014\000\058\000\058\000\058\000\
\058\000\058\000\015\000\101\000\016\000\104\000\058\000\058\000\
\058\000\058\000\058\000\058\000\044\000\105\000\109\000\108\000\
\044\000\044\000\044\000\122\000\123\000\044\000\044\000\044\000\
\044\000\044\000\125\000\128\000\131\000\132\000\044\000\044\000\
\044\000\044\000\044\000\044\000\045\000\141\000\144\000\001\000\
\045\000\045\000\045\000\008\000\016\000\045\000\045\000\045\000\
\045\000\045\000\018\000\060\000\062\000\022\000\045\000\045\000\
\045\000\045\000\045\000\045\000\103\000\075\000\048\000\084\000\
\029\000\145\000\080\000\086\000\014\000\087\000\088\000\089\000\
\090\000\091\000\015\000\000\000\016\000\000\000\092\000\093\000\
\094\000\095\000\096\000\097\000\000\000\106\000\057\000\000\000\
\084\000\000\000\058\000\059\000\086\000\000\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000\096\000\097\000\121\000\000\000\000\000\
\084\000\000\000\000\000\000\000\086\000\000\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000\096\000\097\000\142\000\000\000\000\000\
\084\000\000\000\000\000\000\000\086\000\000\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000\096\000\097\000\042\000\000\000\000\000\
\000\000\042\000\042\000\042\000\000\000\000\000\042\000\042\000\
\042\000\042\000\042\000\000\000\000\000\000\000\084\000\042\000\
\042\000\043\000\086\000\042\000\042\000\043\000\043\000\043\000\
\000\000\000\000\043\000\043\000\043\000\043\000\043\000\094\000\
\095\000\000\000\000\000\043\000\043\000\037\000\000\000\043\000\
\043\000\037\000\037\000\037\000\038\000\000\000\000\000\039\000\
\038\000\038\000\038\000\039\000\039\000\039\000\000\000\000\000\
\000\000\040\000\000\000\037\000\037\000\040\000\040\000\040\000\
\000\000\000\000\038\000\038\000\041\000\039\000\039\000\035\000\
\041\000\041\000\041\000\035\000\035\000\035\000\000\000\040\000\
\040\000\000\000\000\000\029\000\000\000\000\000\029\000\000\000\
\029\000\000\000\041\000\041\000\000\000\035\000\035\000\029\000\
\000\000\029\000\049\000\029\000\000\000\050\000\000\000\051\000\
\029\000\029\000\000\000\000\000\000\000\000\000\052\000\000\000\
\049\000\024\000\054\000\050\000\024\000\051\000\024\000\055\000\
\099\000\000\000\000\000\000\000\052\000\024\000\053\000\024\000\
\054\000\024\000\000\000\000\000\049\000\055\000\024\000\050\000\
\000\000\051\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\000\000\000\000\000\000\054\000\000\000\084\000\000\000\
\085\000\055\000\086\000\000\000\087\000\088\000\089\000\090\000\
\091\000\000\000\000\000\000\000\000\000\092\000\093\000\094\000\
\095\000\096\000\097\000\084\000\100\000\000\000\000\000\086\000\
\000\000\087\000\088\000\089\000\090\000\091\000\000\000\000\000\
\000\000\000\000\092\000\093\000\094\000\095\000\096\000\097\000\
\084\000\000\000\102\000\000\000\086\000\000\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000\096\000\097\000\084\000\127\000\000\000\
\000\000\086\000\000\000\087\000\088\000\089\000\090\000\091\000\
\000\000\000\000\000\000\000\000\092\000\093\000\094\000\095\000\
\096\000\097\000\084\000\133\000\000\000\000\000\086\000\000\000\
\087\000\088\000\089\000\090\000\091\000\000\000\000\000\000\000\
\000\000\092\000\093\000\094\000\095\000\096\000\097\000\084\000\
\000\000\136\000\000\000\086\000\000\000\087\000\088\000\089\000\
\090\000\091\000\000\000\000\000\000\000\000\000\092\000\093\000\
\094\000\095\000\096\000\097\000\084\000\000\000\000\000\139\000\
\086\000\000\000\087\000\088\000\089\000\090\000\091\000\000\000\
\000\000\000\000\000\000\092\000\093\000\094\000\095\000\096\000\
\097\000\084\000\000\000\000\000\000\000\086\000\000\000\087\000\
\088\000\089\000\090\000\091\000\000\000\000\000\000\000\000\000\
\092\000\093\000\094\000\095\000\096\000\084\000\000\000\000\000\
\000\000\086\000\000\000\087\000\088\000\089\000\090\000\091\000\
\000\000\000\000\000\000\000\000\092\000\093\000\094\000\095\000\
\084\000\000\000\000\000\000\000\086\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\092\000\
\093\000\094\000\095\000"

let yycheck = "\057\000\
\017\000\059\000\008\001\021\001\062\000\028\001\024\000\015\001\
\012\001\032\001\033\001\034\001\030\001\071\000\072\000\031\001\
\074\000\035\000\001\000\035\001\008\001\029\001\040\000\103\000\
\008\001\031\001\084\000\044\000\051\001\087\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
\008\001\121\000\001\001\031\001\003\001\021\001\005\001\105\000\
\007\001\008\001\068\000\001\001\132\000\003\001\030\001\005\001\
\008\001\007\001\008\001\008\001\019\001\033\001\029\001\031\001\
\122\000\021\001\025\001\026\001\027\001\019\001\128\000\032\001\
\010\001\131\000\030\001\025\001\026\001\027\001\016\001\031\001\
\018\001\139\000\032\001\042\001\028\001\023\001\027\001\031\001\
\032\001\033\001\034\001\035\001\042\001\037\001\038\001\039\001\
\040\001\041\001\028\001\034\001\029\001\033\001\046\001\047\001\
\048\001\049\001\050\001\051\001\028\001\033\001\027\001\027\001\
\032\001\033\001\034\001\008\001\010\001\037\001\038\001\039\001\
\040\001\041\001\016\001\035\001\018\001\027\001\046\001\047\001\
\048\001\049\001\050\001\051\001\028\001\031\001\008\001\030\001\
\032\001\033\001\034\001\036\001\008\001\037\001\038\001\039\001\
\040\001\041\001\028\001\027\001\027\001\014\001\046\001\047\001\
\048\001\049\001\050\001\051\001\028\001\028\001\033\001\000\000\
\032\001\033\001\034\001\005\000\028\001\037\001\038\001\039\001\
\040\001\041\001\028\001\028\001\028\001\013\000\046\001\047\001\
\048\001\049\001\050\001\051\001\028\001\055\000\045\000\031\001\
\022\000\143\000\008\001\035\001\010\001\037\001\038\001\039\001\
\040\001\041\001\016\001\255\255\018\001\255\255\046\001\047\001\
\048\001\049\001\050\001\051\001\255\255\028\001\031\001\255\255\
\031\001\255\255\035\001\036\001\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001\050\001\051\001\028\001\255\255\255\255\
\031\001\255\255\255\255\255\255\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001\050\001\051\001\028\001\255\255\255\255\
\031\001\255\255\255\255\255\255\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001\050\001\051\001\028\001\255\255\255\255\
\255\255\032\001\033\001\034\001\255\255\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\031\001\046\001\
\047\001\028\001\035\001\050\001\051\001\032\001\033\001\034\001\
\255\255\255\255\037\001\038\001\039\001\040\001\041\001\048\001\
\049\001\255\255\255\255\046\001\047\001\028\001\255\255\050\001\
\051\001\032\001\033\001\034\001\028\001\255\255\255\255\028\001\
\032\001\033\001\034\001\032\001\033\001\034\001\255\255\255\255\
\255\255\028\001\255\255\050\001\051\001\032\001\033\001\034\001\
\255\255\255\255\050\001\051\001\028\001\050\001\051\001\028\001\
\032\001\033\001\034\001\032\001\033\001\034\001\255\255\050\001\
\051\001\255\255\255\255\008\001\255\255\255\255\011\001\255\255\
\013\001\255\255\050\001\051\001\255\255\050\001\051\001\020\001\
\255\255\022\001\008\001\024\001\255\255\011\001\255\255\013\001\
\029\001\030\001\255\255\255\255\255\255\255\255\020\001\255\255\
\008\001\008\001\024\001\011\001\011\001\013\001\013\001\029\001\
\030\001\255\255\255\255\255\255\020\001\020\001\022\001\022\001\
\024\001\024\001\255\255\255\255\008\001\029\001\029\001\011\001\
\255\255\013\001\255\255\255\255\255\255\255\255\255\255\255\255\
\020\001\255\255\255\255\255\255\024\001\255\255\031\001\255\255\
\033\001\029\001\035\001\255\255\037\001\038\001\039\001\040\001\
\041\001\255\255\255\255\255\255\255\255\046\001\047\001\048\001\
\049\001\050\001\051\001\031\001\032\001\255\255\255\255\035\001\
\255\255\037\001\038\001\039\001\040\001\041\001\255\255\255\255\
\255\255\255\255\046\001\047\001\048\001\049\001\050\001\051\001\
\031\001\255\255\033\001\255\255\035\001\255\255\037\001\038\001\
\039\001\040\001\041\001\255\255\255\255\255\255\255\255\046\001\
\047\001\048\001\049\001\050\001\051\001\031\001\032\001\255\255\
\255\255\035\001\255\255\037\001\038\001\039\001\040\001\041\001\
\255\255\255\255\255\255\255\255\046\001\047\001\048\001\049\001\
\050\001\051\001\031\001\032\001\255\255\255\255\035\001\255\255\
\037\001\038\001\039\001\040\001\041\001\255\255\255\255\255\255\
\255\255\046\001\047\001\048\001\049\001\050\001\051\001\031\001\
\255\255\033\001\255\255\035\001\255\255\037\001\038\001\039\001\
\040\001\041\001\255\255\255\255\255\255\255\255\046\001\047\001\
\048\001\049\001\050\001\051\001\031\001\255\255\255\255\034\001\
\035\001\255\255\037\001\038\001\039\001\040\001\041\001\255\255\
\255\255\255\255\255\255\046\001\047\001\048\001\049\001\050\001\
\051\001\031\001\255\255\255\255\255\255\035\001\255\255\037\001\
\038\001\039\001\040\001\041\001\255\255\255\255\255\255\255\255\
\046\001\047\001\048\001\049\001\050\001\031\001\255\255\255\255\
\255\255\035\001\255\255\037\001\038\001\039\001\040\001\041\001\
\255\255\255\255\255\255\255\255\046\001\047\001\048\001\049\001\
\031\001\255\255\255\255\255\255\035\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\046\001\
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
# 443 "minijavaparse.ml"
               : Minijavaast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'classdecl) in
    Obj.repr(
# 48 "minijavaparse.mly"
                 ( [_1] )
# 450 "minijavaparse.ml"
               : 'classdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'classdecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classdecl) in
    Obj.repr(
# 49 "minijavaparse.mly"
                         ( _1 @ [_2] )
# 458 "minijavaparse.ml"
               : 'classdecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'staticvardecls) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    Obj.repr(
# 52 "minijavaparse.mly"
                                                                   ( Class(_2, "", _4, _5) )
# 467 "minijavaparse.ml"
               : 'classdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'staticvardecls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    Obj.repr(
# 53 "minijavaparse.mly"
                                                                                  ( Class(_2, _4, _6, _7) )
# 477 "minijavaparse.ml"
               : 'classdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "minijavaparse.mly"
                                ( [] )
# 483 "minijavaparse.ml"
               : 'staticvardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'staticvardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'staticvardecl) in
    Obj.repr(
# 57 "minijavaparse.mly"
                                 ( _1 @ [_2] )
# 491 "minijavaparse.ml"
               : 'staticvardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 60 "minijavaparse.mly"
                   ( (NonStatic, _1) )
# 498 "minijavaparse.ml"
               : 'staticvardecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 61 "minijavaparse.mly"
                   ( (Static, _2) )
# 505 "minijavaparse.ml"
               : 'staticvardecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "minijavaparse.mly"
                   ( [] )
# 511 "minijavaparse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 65 "minijavaparse.mly"
                     ( _1 @ [_2] )
# 519 "minijavaparse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 68 "minijavaparse.mly"
                                    ( Var(_1, _2) )
# 527 "minijavaparse.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "minijavaparse.mly"
                    ( [] )
# 533 "minijavaparse.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methoddecl) in
    Obj.repr(
# 72 "minijavaparse.mly"
                           ( _1 @ [_2] )
# 541 "minijavaparse.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 11 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : 'params) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'vardecls) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : 'stmts0) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 75 "minijavaparse.mly"
                                                                                                              ( Method(_2, _3, _5, _8, _9, _11) )
# 553 "minijavaparse.ml"
               : 'methoddecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "minijavaparse.mly"
                      ( [] )
# 559 "minijavaparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 79 "minijavaparse.mly"
                                    ( Var(_1, _2) :: _3 )
# 568 "minijavaparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "minijavaparse.mly"
                        ( [] )
# 574 "minijavaparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 83 "minijavaparse.mly"
                                          ( Var(_2, _3) :: _4 )
# 583 "minijavaparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    Obj.repr(
# 86 "minijavaparse.mly"
                           ( ArrayType _1 )
# 590 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "minijavaparse.mly"
                ( BoolType )
# 596 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "minijavaparse.mly"
              ( FloatType )
# 602 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "minijavaparse.mly"
             ( IntType )
# 608 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "minijavaparse.mly"
                  ( [] )
# 614 "minijavaparse.ml"
               : 'stmts0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts0) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "minijavaparse.mly"
                 ( _1 @ [_2] )
# 622 "minijavaparse.ml"
               : 'stmts0))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts0) in
    Obj.repr(
# 97 "minijavaparse.mly"
                          ( Block _2 )
# 629 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 98 "minijavaparse.mly"
                                        ( Assignment(_1, _3) )
# 637 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "minijavaparse.mly"
                                               ( If(_3, _5, _7) )
# 646 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "minijavaparse.mly"
                                                ( If(_3, _5, Block []) )
# 654 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "minijavaparse.mly"
                                         ( While (_3, _5) )
# 662 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    Obj.repr(
# 103 "minijavaparse.mly"
(
	if _1="System" && _3="out" && _5="println" then Println _7
	else raise Parsing.Parse_error
  	)
# 675 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "minijavaparse.mly"
                     ( Break )
# 681 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "minijavaparse.mly"
                       ( Continue )
# 687 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 109 "minijavaparse.mly"
                                                                ( ArrayAssignment(_1, _3, _6) )
# 696 "minijavaparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 112 "minijavaparse.mly"
                              ( Operation(_1, And, _3) )
# 704 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 113 "minijavaparse.mly"
                             ( Operation(_1, Or, _3) )
# 712 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 114 "minijavaparse.mly"
                             ( Operation(_1, LessThan, _3) )
# 720 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 115 "minijavaparse.mly"
                                ( Operation(_1, GreaterThan, _3) )
# 728 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 116 "minijavaparse.mly"
                                ( Operation(_1, Equal, _3) )
# 736 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 117 "minijavaparse.mly"
                                ( Operation(_1, LessThanEq, _3) )
# 744 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 118 "minijavaparse.mly"
                                ( Operation(_1, GreaterThanEq, _3) )
# 752 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 119 "minijavaparse.mly"
                               ( Operation(_1, Plus, _3) )
# 760 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 120 "minijavaparse.mly"
                                ( Operation(_1, Minus, _3) )
# 768 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 121 "minijavaparse.mly"
                               ( Operation(_1, Multiplication, _3) )
# 776 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 122 "minijavaparse.mly"
                              ( Operation(_1, Division, _3) )
# 784 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 123 "minijavaparse.mly"
                                        ( Subscript(_1, _3) )
# 792 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "minijavaparse.mly"
                              ( if _3="length" then Length _1 else FieldRef(_1, _3) )
# 800 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 125 "minijavaparse.mly"
                                                 ( MethodCall(_1, _3, _5) )
# 809 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 126 "minijavaparse.mly"
                     ( Integer _1 )
# 816 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 127 "minijavaparse.mly"
                   ( Float _1 )
# 823 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "minijavaparse.mly"
                    ( String _1 )
# 830 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "minijavaparse.mly"
                  ( Null )
# 836 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 130 "minijavaparse.mly"
                     ( if _1 then True else False )
# 843 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "minijavaparse.mly"
                 ( Id _1 )
# 850 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "minijavaparse.mly"
           ( This )
# 856 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'exp_type) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 133 "minijavaparse.mly"
                                          ( NewArray(_2, _4) )
# 864 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 134 "minijavaparse.mly"
                                    ( NewId _2 )
# 871 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 135 "minijavaparse.mly"
                       ( Not _2 )
# 878 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 136 "minijavaparse.mly"
                             ( _2 )
# 885 "minijavaparse.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "minijavaparse.mly"
                    ( [] )
# 891 "minijavaparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 140 "minijavaparse.mly"
                         ( _1 :: _2 )
# 899 "minijavaparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "minijavaparse.mly"
                     ( [] )
# 905 "minijavaparse.ml"
               : 'more_args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 144 "minijavaparse.mly"
                               ( _2 :: _3 )
# 913 "minijavaparse.ml"
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
