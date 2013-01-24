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

open Parsing;;
let _ = parse_error;;
# 1 "minijavaparse.mly"

	    open Mp2common
	
# 114 "minijavaparse.ml"
let yytransl_const = [|
    0 (* EOF *);
  265 (* ABSTRACT *);
  266 (* BOOLEAN *);
  267 (* BREAK *);
  268 (* BYTE *);
  269 (* CASE *);
  270 (* CATCH *);
  271 (* CHAR *);
  272 (* CLASS *);
  273 (* CONST *);
  274 (* CONTINUE *);
  275 (* DO *);
  276 (* DOUBLE *);
  277 (* ELSE *);
  278 (* EXTENDS *);
  279 (* FINAL *);
  280 (* FINALLY *);
  281 (* FLOAT *);
  282 (* FOR *);
  283 (* DEFAULT *);
  284 (* IMPLEMENTS *);
  285 (* IMPORT *);
  286 (* INSTANCEOF *);
  287 (* INT *);
  288 (* INTERFACE *);
  289 (* LONG *);
  290 (* NATIVE *);
  291 (* NEW *);
  292 (* GOTO *);
  293 (* IF *);
  294 (* PUBLIC *);
  295 (* SHORT *);
  296 (* SUPER *);
  297 (* SWITCH *);
  298 (* SYNCHRONIZED *);
  299 (* PACKAGE *);
  300 (* PRIVATE *);
  301 (* PROTECTED *);
  302 (* TRANSIENT *);
  303 (* RETURN *);
  304 (* VOID *);
  305 (* STATIC *);
  306 (* WHILE *);
  307 (* THIS *);
  308 (* THROW *);
  309 (* THROWS *);
  310 (* TRY *);
  311 (* VOLATILE *);
  312 (* STRICTFP *);
  313 (* NULL_LITERAL *);
  314 (* LPAREN *);
  315 (* RPAREN *);
  316 (* LBRACE *);
  317 (* RBRACE *);
  318 (* LBRACK *);
  319 (* RBRACK *);
  320 (* SEMICOLON *);
  321 (* COMMA *);
  322 (* DOT *);
  323 (* EQ *);
  324 (* GT *);
  325 (* LT *);
  326 (* NOT *);
  327 (* COMP *);
  328 (* QUESTION *);
  329 (* COLON *);
  330 (* EQEQ *);
  331 (* LTEQ *);
  332 (* GTEQ *);
  333 (* NOTEQ *);
  334 (* ANDAND *);
  335 (* OROR *);
  336 (* PLUSPLUS *);
  337 (* MINUSMINUS *);
  338 (* PLUS *);
  339 (* MINUS *);
  340 (* MULT *);
  341 (* DIV *);
  342 (* AND *);
  343 (* OR *);
  344 (* XOR *);
  345 (* MOD *);
  346 (* LSHIFT *);
  347 (* RSHIFT *);
  348 (* URSHIFT *);
  349 (* PLUSEQ *);
  350 (* MINUSEQ *);
  351 (* MULTEQ *);
  352 (* DIVEQ *);
  353 (* ANDEQ *);
  354 (* OREQ *);
  355 (* XOREQ *);
  356 (* MODEQ *);
  357 (* LSHIFTEQ *);
  358 (* RSHIFTEQ *);
  359 (* URSHIFTEQ *);
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
    0|]

let yylhs = "\255\255\
\001\000\005\000\005\000\002\000\002\000\006\000\006\000\008\000\
\008\000\010\000\010\000\009\000\007\000\007\000\012\000\013\000\
\013\000\015\000\015\000\011\000\011\000\011\000\011\000\014\000\
\014\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\016\000\016\000\017\000\017\000\000\000\
\000\000\000\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\006\000\008\000\000\000\002\000\001\000\
\002\000\000\000\002\000\003\000\000\000\002\000\013\000\000\000\
\003\000\000\000\004\000\003\000\001\000\001\000\001\000\000\000\
\002\000\003\000\004\000\007\000\005\000\005\000\009\000\002\000\
\002\000\007\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\003\000\006\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\005\000\
\004\000\002\000\003\000\000\000\002\000\000\000\003\000\002\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\064\000\002\000\
\000\000\065\000\000\000\000\000\000\000\000\000\000\000\024\000\
\066\000\049\000\050\000\053\000\051\000\054\000\000\000\055\000\
\052\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\
\000\000\032\000\033\000\000\000\000\000\000\000\000\000\021\000\
\022\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\026\000\025\000\000\000\000\000\059\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\000\000\057\000\020\000\000\000\046\000\000\000\006\000\
\000\000\000\000\007\000\008\000\000\000\000\000\000\000\000\000\
\030\000\056\000\000\000\000\000\000\000\009\000\000\000\004\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\061\000\
\048\000\000\000\000\000\012\000\034\000\000\000\028\000\000\000\
\005\000\000\000\000\000\063\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\017\000\010\000\000\000\000\000\000\000\
\011\000\000\000\019\000\000\000\000\000\000\000\015\000"

let yydgoto = "\005\000\
\007\000\008\000\067\000\028\000\009\000\085\000\098\000\099\000\
\100\000\143\000\101\000\113\000\136\000\038\000\140\000\108\000\
\120\000"

let yysindex = "\187\000\
\245\254\245\254\102\255\093\255\000\000\255\254\000\000\000\000\
\245\254\000\000\099\255\217\254\232\254\239\254\250\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\096\255\000\000\
\000\000\093\255\093\255\108\001\242\254\000\000\093\255\039\255\
\093\255\000\000\000\000\093\255\093\255\074\255\253\254\000\000\
\000\000\000\000\018\255\111\255\200\254\093\255\075\255\093\255\
\093\255\093\255\093\255\093\255\093\255\093\255\093\255\093\255\
\093\255\093\255\081\255\000\000\182\000\027\255\208\000\140\255\
\169\255\000\000\000\000\036\255\021\255\000\000\234\000\041\255\
\208\254\208\254\208\254\208\254\208\254\203\254\203\254\200\254\
\200\254\159\001\134\001\037\255\002\255\038\255\095\255\000\000\
\102\255\102\255\000\000\000\000\004\001\000\000\093\255\000\000\
\112\255\237\254\000\000\000\000\251\254\093\255\050\255\088\255\
\000\000\000\000\030\001\064\255\002\255\000\000\112\255\000\000\
\000\000\061\255\069\255\056\001\093\255\102\255\093\255\000\000\
\000\000\015\255\009\255\000\000\000\000\205\255\000\000\030\001\
\000\000\078\255\077\255\000\000\112\255\000\000\012\255\079\255\
\082\255\086\255\112\255\000\000\000\000\013\255\112\255\082\255\
\000\000\108\255\000\000\093\255\082\001\087\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\153\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\154\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\015\000\044\000\073\000\102\000\149\000\117\000\142\000\059\000\
\088\000\156\000\167\000\000\000\016\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\098\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\000\000\000\000\105\255\000\000\016\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\105\255\
\000\000\000\000\000\000\000\000\115\255\000\000\000\000\000\000\
\116\255\000\000\000\000\000\000\000\000\000\000\122\255\116\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\014\000\253\255\012\000\000\000\075\000\067\000\000\000\
\163\255\000\000\235\255\000\000\000\000\035\000\037\000\000\000\
\055\000"

let yytablesize = 756
let yytable = "\017\000\
\047\000\043\000\114\000\110\000\006\000\046\000\029\000\059\000\
\046\000\047\000\029\000\040\000\047\000\046\000\039\000\010\000\
\130\000\047\000\111\000\137\000\144\000\018\000\030\000\019\000\
\034\000\020\000\041\000\021\000\022\000\058\000\055\000\056\000\
\042\000\053\000\054\000\055\000\056\000\044\000\045\000\035\000\
\036\000\112\000\061\000\037\000\063\000\060\000\062\000\064\000\
\065\000\145\000\097\000\037\000\111\000\013\000\068\000\023\000\
\115\000\071\000\044\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\115\000\024\000\
\041\000\115\000\115\000\129\000\013\000\025\000\026\000\069\000\
\093\000\011\000\072\000\092\000\012\000\104\000\105\000\045\000\
\084\000\123\000\027\000\013\000\087\000\018\000\091\000\019\000\
\096\000\020\000\095\000\021\000\022\000\038\000\103\000\039\000\
\102\000\040\000\107\000\117\000\118\000\011\000\014\000\135\000\
\012\000\116\000\127\000\011\000\042\000\142\000\012\000\013\000\
\041\000\040\000\121\000\015\000\124\000\013\000\042\000\023\000\
\126\000\024\000\128\000\092\000\024\000\016\000\066\000\133\000\
\041\000\138\000\014\000\024\000\134\000\043\000\042\000\024\000\
\014\000\141\000\139\000\151\000\040\000\025\000\026\000\015\000\
\001\000\067\000\148\000\035\000\060\000\015\000\024\000\149\000\
\031\000\016\000\027\000\062\000\032\000\033\000\036\000\016\000\
\024\000\070\000\109\000\024\000\046\000\016\000\018\000\122\000\
\047\000\146\000\048\000\049\000\147\000\024\000\132\000\000\000\
\050\000\051\000\052\000\001\000\002\000\003\000\004\000\000\000\
\053\000\054\000\055\000\056\000\057\000\058\000\089\000\000\000\
\000\000\046\000\000\000\000\000\000\000\047\000\000\000\048\000\
\049\000\000\000\000\000\000\000\000\000\050\000\051\000\052\000\
\000\000\000\000\000\000\000\000\000\000\053\000\054\000\055\000\
\056\000\057\000\058\000\090\000\000\000\000\000\046\000\000\000\
\000\000\000\000\047\000\000\000\048\000\049\000\000\000\000\000\
\000\000\000\000\050\000\051\000\052\000\000\000\000\000\000\000\
\000\000\000\000\053\000\054\000\055\000\056\000\057\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\131\000\
\000\000\000\000\046\000\000\000\000\000\000\000\047\000\000\000\
\048\000\049\000\029\000\000\000\000\000\029\000\050\000\051\000\
\052\000\000\000\000\000\000\000\029\000\000\000\053\000\054\000\
\055\000\056\000\057\000\058\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\047\000\029\000\000\000\047\000\047\000\
\047\000\047\000\047\000\000\000\047\000\047\000\029\000\029\000\
\000\000\039\000\047\000\047\000\047\000\039\000\039\000\039\000\
\000\000\000\000\047\000\047\000\047\000\047\000\047\000\047\000\
\058\000\000\000\000\000\000\000\058\000\058\000\058\000\000\000\
\000\000\058\000\058\000\000\000\039\000\039\000\037\000\058\000\
\058\000\058\000\037\000\037\000\037\000\000\000\000\000\058\000\
\058\000\058\000\058\000\058\000\058\000\044\000\000\000\000\000\
\000\000\044\000\044\000\044\000\000\000\000\000\044\000\044\000\
\000\000\037\000\037\000\041\000\044\000\044\000\044\000\041\000\
\041\000\041\000\000\000\000\000\044\000\044\000\044\000\044\000\
\044\000\044\000\045\000\000\000\000\000\000\000\045\000\045\000\
\045\000\000\000\000\000\045\000\045\000\000\000\041\000\041\000\
\038\000\045\000\045\000\045\000\038\000\038\000\038\000\000\000\
\000\000\045\000\045\000\045\000\045\000\045\000\045\000\042\000\
\000\000\000\000\000\000\042\000\042\000\042\000\000\000\000\000\
\042\000\042\000\000\000\038\000\038\000\000\000\042\000\042\000\
\042\000\000\000\000\000\000\000\000\000\000\000\042\000\042\000\
\043\000\000\000\042\000\042\000\043\000\043\000\043\000\040\000\
\000\000\043\000\043\000\040\000\040\000\040\000\035\000\043\000\
\043\000\043\000\035\000\035\000\035\000\000\000\000\000\043\000\
\043\000\036\000\000\000\043\000\043\000\036\000\036\000\036\000\
\000\000\000\000\040\000\040\000\000\000\000\000\000\000\000\000\
\000\000\035\000\035\000\046\000\086\000\000\000\000\000\047\000\
\000\000\048\000\049\000\000\000\000\000\036\000\000\000\050\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\053\000\
\054\000\055\000\056\000\057\000\058\000\046\000\000\000\088\000\
\000\000\047\000\000\000\048\000\049\000\000\000\000\000\000\000\
\000\000\050\000\051\000\052\000\000\000\000\000\000\000\000\000\
\000\000\053\000\054\000\055\000\056\000\057\000\058\000\046\000\
\094\000\000\000\000\000\047\000\000\000\048\000\049\000\000\000\
\000\000\000\000\000\000\050\000\051\000\052\000\000\000\000\000\
\000\000\000\000\000\000\053\000\054\000\055\000\056\000\057\000\
\058\000\046\000\106\000\000\000\000\000\047\000\000\000\048\000\
\049\000\000\000\000\000\000\000\000\000\050\000\051\000\052\000\
\000\000\000\000\000\000\000\000\000\000\053\000\054\000\055\000\
\056\000\057\000\058\000\046\000\000\000\000\000\119\000\047\000\
\000\000\048\000\049\000\000\000\000\000\000\000\000\000\050\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\053\000\
\054\000\055\000\056\000\057\000\058\000\046\000\000\000\125\000\
\000\000\047\000\000\000\048\000\049\000\000\000\000\000\000\000\
\000\000\050\000\051\000\052\000\000\000\000\000\000\000\000\000\
\000\000\053\000\054\000\055\000\056\000\057\000\058\000\046\000\
\000\000\150\000\000\000\047\000\000\000\048\000\049\000\000\000\
\000\000\000\000\000\000\050\000\051\000\052\000\000\000\000\000\
\000\000\000\000\000\000\053\000\054\000\055\000\056\000\057\000\
\058\000\046\000\000\000\000\000\000\000\047\000\000\000\048\000\
\049\000\000\000\000\000\000\000\000\000\050\000\051\000\052\000\
\000\000\000\000\000\000\000\000\000\000\053\000\054\000\055\000\
\056\000\057\000\058\000\046\000\000\000\000\000\000\000\047\000\
\000\000\048\000\049\000\000\000\000\000\000\000\000\000\050\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\053\000\
\054\000\055\000\056\000\057\000\046\000\000\000\000\000\000\000\
\047\000\000\000\048\000\049\000\000\000\000\000\000\000\000\000\
\050\000\051\000\052\000\000\000\000\000\000\000\000\000\000\000\
\053\000\054\000\055\000\056\000"

let yycheck = "\003\000\
\000\000\023\000\008\001\097\000\016\001\062\001\008\001\022\001\
\062\001\066\001\000\000\010\001\066\001\062\001\000\000\002\000\
\008\001\066\001\038\001\008\001\008\001\001\001\009\000\003\001\
\064\001\005\001\025\001\007\001\008\001\000\000\084\001\085\001\
\031\001\082\001\083\001\084\001\085\001\026\000\027\000\064\001\
\058\001\061\001\031\000\000\000\033\000\060\001\008\001\036\000\
\037\000\143\000\049\001\058\001\038\001\038\001\058\001\035\001\
\062\001\046\000\000\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\062\001\051\001\
\000\000\062\001\062\001\061\001\061\001\057\001\058\001\062\001\
\069\000\008\001\008\001\063\001\011\001\089\000\090\000\000\000\
\008\001\111\000\070\001\018\001\066\001\001\001\059\001\003\001\
\060\001\005\001\058\001\007\001\008\001\000\000\008\001\008\001\
\067\001\010\001\095\000\058\001\021\001\008\001\037\001\133\000\
\011\001\102\000\118\000\008\001\000\000\139\000\011\001\018\001\
\025\001\010\001\059\001\050\001\064\001\018\001\031\001\035\001\
\117\000\008\001\119\000\063\001\011\001\060\001\061\001\058\001\
\025\001\059\001\037\001\018\001\064\001\000\000\031\001\051\001\
\037\001\060\001\065\001\061\001\000\000\057\001\058\001\050\001\
\000\000\000\000\047\001\000\000\059\001\050\001\037\001\148\000\
\062\001\060\001\070\001\059\001\066\001\067\001\000\000\060\001\
\047\001\059\001\096\000\050\001\062\001\059\001\059\001\109\000\
\066\001\143\000\068\001\069\001\144\000\060\001\128\000\255\255\
\074\001\075\001\076\001\001\000\002\000\003\000\004\000\255\255\
\082\001\083\001\084\001\085\001\086\001\087\001\059\001\255\255\
\255\255\062\001\255\255\255\255\255\255\066\001\255\255\068\001\
\069\001\255\255\255\255\255\255\255\255\074\001\075\001\076\001\
\255\255\255\255\255\255\255\255\255\255\082\001\083\001\084\001\
\085\001\086\001\087\001\059\001\255\255\255\255\062\001\255\255\
\255\255\255\255\066\001\255\255\068\001\069\001\255\255\255\255\
\255\255\255\255\074\001\075\001\076\001\255\255\255\255\255\255\
\255\255\255\255\082\001\083\001\084\001\085\001\086\001\087\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\059\001\
\255\255\255\255\062\001\255\255\255\255\255\255\066\001\255\255\
\068\001\069\001\008\001\255\255\255\255\011\001\074\001\075\001\
\076\001\255\255\255\255\255\255\018\001\255\255\082\001\083\001\
\084\001\085\001\086\001\087\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\037\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\047\001\255\255\059\001\050\001\255\255\062\001\063\001\
\064\001\065\001\066\001\255\255\068\001\069\001\060\001\061\001\
\255\255\059\001\074\001\075\001\076\001\063\001\064\001\065\001\
\255\255\255\255\082\001\083\001\084\001\085\001\086\001\087\001\
\059\001\255\255\255\255\255\255\063\001\064\001\065\001\255\255\
\255\255\068\001\069\001\255\255\086\001\087\001\059\001\074\001\
\075\001\076\001\063\001\064\001\065\001\255\255\255\255\082\001\
\083\001\084\001\085\001\086\001\087\001\059\001\255\255\255\255\
\255\255\063\001\064\001\065\001\255\255\255\255\068\001\069\001\
\255\255\086\001\087\001\059\001\074\001\075\001\076\001\063\001\
\064\001\065\001\255\255\255\255\082\001\083\001\084\001\085\001\
\086\001\087\001\059\001\255\255\255\255\255\255\063\001\064\001\
\065\001\255\255\255\255\068\001\069\001\255\255\086\001\087\001\
\059\001\074\001\075\001\076\001\063\001\064\001\065\001\255\255\
\255\255\082\001\083\001\084\001\085\001\086\001\087\001\059\001\
\255\255\255\255\255\255\063\001\064\001\065\001\255\255\255\255\
\068\001\069\001\255\255\086\001\087\001\255\255\074\001\075\001\
\076\001\255\255\255\255\255\255\255\255\255\255\082\001\083\001\
\059\001\255\255\086\001\087\001\063\001\064\001\065\001\059\001\
\255\255\068\001\069\001\063\001\064\001\065\001\059\001\074\001\
\075\001\076\001\063\001\064\001\065\001\255\255\255\255\082\001\
\083\001\059\001\255\255\086\001\087\001\063\001\064\001\065\001\
\255\255\255\255\086\001\087\001\255\255\255\255\255\255\255\255\
\255\255\086\001\087\001\062\001\063\001\255\255\255\255\066\001\
\255\255\068\001\069\001\255\255\255\255\087\001\255\255\074\001\
\075\001\076\001\255\255\255\255\255\255\255\255\255\255\082\001\
\083\001\084\001\085\001\086\001\087\001\062\001\255\255\064\001\
\255\255\066\001\255\255\068\001\069\001\255\255\255\255\255\255\
\255\255\074\001\075\001\076\001\255\255\255\255\255\255\255\255\
\255\255\082\001\083\001\084\001\085\001\086\001\087\001\062\001\
\063\001\255\255\255\255\066\001\255\255\068\001\069\001\255\255\
\255\255\255\255\255\255\074\001\075\001\076\001\255\255\255\255\
\255\255\255\255\255\255\082\001\083\001\084\001\085\001\086\001\
\087\001\062\001\063\001\255\255\255\255\066\001\255\255\068\001\
\069\001\255\255\255\255\255\255\255\255\074\001\075\001\076\001\
\255\255\255\255\255\255\255\255\255\255\082\001\083\001\084\001\
\085\001\086\001\087\001\062\001\255\255\255\255\065\001\066\001\
\255\255\068\001\069\001\255\255\255\255\255\255\255\255\074\001\
\075\001\076\001\255\255\255\255\255\255\255\255\255\255\082\001\
\083\001\084\001\085\001\086\001\087\001\062\001\255\255\064\001\
\255\255\066\001\255\255\068\001\069\001\255\255\255\255\255\255\
\255\255\074\001\075\001\076\001\255\255\255\255\255\255\255\255\
\255\255\082\001\083\001\084\001\085\001\086\001\087\001\062\001\
\255\255\064\001\255\255\066\001\255\255\068\001\069\001\255\255\
\255\255\255\255\255\255\074\001\075\001\076\001\255\255\255\255\
\255\255\255\255\255\255\082\001\083\001\084\001\085\001\086\001\
\087\001\062\001\255\255\255\255\255\255\066\001\255\255\068\001\
\069\001\255\255\255\255\255\255\255\255\074\001\075\001\076\001\
\255\255\255\255\255\255\255\255\255\255\082\001\083\001\084\001\
\085\001\086\001\087\001\062\001\255\255\255\255\255\255\066\001\
\255\255\068\001\069\001\255\255\255\255\255\255\255\255\074\001\
\075\001\076\001\255\255\255\255\255\255\255\255\255\255\082\001\
\083\001\084\001\085\001\086\001\062\001\255\255\255\255\255\255\
\066\001\255\255\068\001\069\001\255\255\255\255\255\255\255\255\
\074\001\075\001\076\001\255\255\255\255\255\255\255\255\255\255\
\082\001\083\001\084\001\085\001"

let yynames_const = "\
  EOF\000\
  ABSTRACT\000\
  BOOLEAN\000\
  BREAK\000\
  BYTE\000\
  CASE\000\
  CATCH\000\
  CHAR\000\
  CLASS\000\
  CONST\000\
  CONTINUE\000\
  DO\000\
  DOUBLE\000\
  ELSE\000\
  EXTENDS\000\
  FINAL\000\
  FINALLY\000\
  FLOAT\000\
  FOR\000\
  DEFAULT\000\
  IMPLEMENTS\000\
  IMPORT\000\
  INSTANCEOF\000\
  INT\000\
  INTERFACE\000\
  LONG\000\
  NATIVE\000\
  NEW\000\
  GOTO\000\
  IF\000\
  PUBLIC\000\
  SHORT\000\
  SUPER\000\
  SWITCH\000\
  SYNCHRONIZED\000\
  PACKAGE\000\
  PRIVATE\000\
  PROTECTED\000\
  TRANSIENT\000\
  RETURN\000\
  VOID\000\
  STATIC\000\
  WHILE\000\
  THIS\000\
  THROW\000\
  THROWS\000\
  TRY\000\
  VOLATILE\000\
  STRICTFP\000\
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
  GT\000\
  LT\000\
  NOT\000\
  COMP\000\
  QUESTION\000\
  COLON\000\
  EQEQ\000\
  LTEQ\000\
  GTEQ\000\
  NOTEQ\000\
  ANDAND\000\
  OROR\000\
  PLUSPLUS\000\
  MINUSMINUS\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  AND\000\
  OR\000\
  XOR\000\
  MOD\000\
  LSHIFT\000\
  RSHIFT\000\
  URSHIFT\000\
  PLUSEQ\000\
  MINUSEQ\000\
  MULTEQ\000\
  DIVEQ\000\
  ANDEQ\000\
  OREQ\000\
  XOREQ\000\
  MODEQ\000\
  LSHIFTEQ\000\
  RSHIFTEQ\000\
  URSHIFTEQ\000\
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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'classdecls) in
    Obj.repr(
# 47 "minijavaparse.mly"
                 ( Program _1 )
# 632 "minijavaparse.ml"
               : Mp2common.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.class_decl) in
    Obj.repr(
# 50 "minijavaparse.mly"
                  ( [_1] )
# 639 "minijavaparse.ml"
               : 'classdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'classdecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.class_decl) in
    Obj.repr(
# 51 "minijavaparse.mly"
                          ( _1 @ [_2] )
# 647 "minijavaparse.ml"
               : 'classdecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'staticvardecls) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    Obj.repr(
# 54 "minijavaparse.mly"
                                                                    ( Class(_2, "", _4, _5) )
# 656 "minijavaparse.ml"
               : Mp2common.class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'staticvardecls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    Obj.repr(
# 55 "minijavaparse.mly"
                                                                                   ( Class(_2, _4, _6, _7) )
# 666 "minijavaparse.ml"
               : Mp2common.class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "minijavaparse.mly"
                                 ( [] )
# 672 "minijavaparse.ml"
               : 'staticvardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'staticvardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'staticvardecl) in
    Obj.repr(
# 59 "minijavaparse.mly"
                                  ( _1 @ [_2] )
# 680 "minijavaparse.ml"
               : 'staticvardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 62 "minijavaparse.mly"
                    ( (NonStatic, _1) )
# 687 "minijavaparse.ml"
               : 'staticvardecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 63 "minijavaparse.mly"
                    ( (Static, _2) )
# 694 "minijavaparse.ml"
               : 'staticvardecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "minijavaparse.mly"
                    ( [] )
# 700 "minijavaparse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 67 "minijavaparse.mly"
                      ( _1 @ [_2] )
# 708 "minijavaparse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "minijavaparse.mly"
                                     ( Var(_1, _2) )
# 716 "minijavaparse.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "minijavaparse.mly"
                     ( [] )
# 722 "minijavaparse.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methoddecl) in
    Obj.repr(
# 74 "minijavaparse.mly"
                            ( _1 @ [_2] )
# 730 "minijavaparse.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 11 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 8 : 'params) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'vardecls) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : 'stmts0) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    Obj.repr(
# 77 "minijavaparse.mly"
                                                                                                               ( Method(_2, _3, _5, _8, _9, _11) )
# 742 "minijavaparse.ml"
               : 'methoddecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "minijavaparse.mly"
                       ( [] )
# 748 "minijavaparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 81 "minijavaparse.mly"
                                     ( Var(_1, _2) :: _3 )
# 757 "minijavaparse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "minijavaparse.mly"
                         ( [] )
# 763 "minijavaparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'more_params) in
    Obj.repr(
# 85 "minijavaparse.mly"
                                           ( Var(_2, _3) :: _4 )
# 772 "minijavaparse.ml"
               : 'more_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_type) in
    Obj.repr(
# 88 "minijavaparse.mly"
                           ( ArrayType _1 )
# 779 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "minijavaparse.mly"
                ( BoolType )
# 785 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "minijavaparse.mly"
              ( FloatType )
# 791 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "minijavaparse.mly"
             ( IntType )
# 797 "minijavaparse.ml"
               : 'exp_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "minijavaparse.mly"
                  ( [] )
# 803 "minijavaparse.ml"
               : 'stmts0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts0) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.statement) in
    Obj.repr(
# 96 "minijavaparse.mly"
                 ( _1 @ [_2] )
# 811 "minijavaparse.ml"
               : 'stmts0))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts0) in
    Obj.repr(
# 99 "minijavaparse.mly"
                          ( Block _2 )
# 818 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    Obj.repr(
# 100 "minijavaparse.mly"
                                        ( Assignment(_1, _3) )
# 826 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Mp2common.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.statement) in
    Obj.repr(
# 101 "minijavaparse.mly"
                                               ( If(_3, _5, _7) )
# 835 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.statement) in
    Obj.repr(
# 102 "minijavaparse.mly"
                                                ( If(_3, _5, Block []) )
# 843 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.statement) in
    Obj.repr(
# 103 "minijavaparse.mly"
                                         ( While (_3, _5) )
# 851 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    Obj.repr(
# 105 "minijavaparse.mly"
 (
		if _1="System" && _3="out" && _5="println" then Println _7
		else raise Parsing.Parse_error
  	)
# 864 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "minijavaparse.mly"
                     ( Break )
# 870 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "minijavaparse.mly"
                       ( Continue )
# 876 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Mp2common.exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    Obj.repr(
# 111 "minijavaparse.mly"
                                                                ( ArrayAssignment(_1, _3, _6) )
# 885 "minijavaparse.ml"
               : Mp2common.statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 114 "minijavaparse.mly"
                              ( Operation(_1, And, _3) )
# 893 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 115 "minijavaparse.mly"
                             ( Operation(_1, Or, _3) )
# 901 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 116 "minijavaparse.mly"
                             ( Operation(_1, LessThan, _3) )
# 909 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 117 "minijavaparse.mly"
                               ( Operation(_1, LessThanEq, _3) )
# 917 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 118 "minijavaparse.mly"
                             ( Operation(_1, GreaterThan, _3) )
# 925 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 119 "minijavaparse.mly"
                               ( Operation(_1, GreaterThanEq, _3) )
# 933 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 120 "minijavaparse.mly"
                               ( Operation(_1, Equal, _3) )
# 941 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 121 "minijavaparse.mly"
                               ( Operation(_1, Plus, _3) )
# 949 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 122 "minijavaparse.mly"
                                ( Operation(_1, Minus, _3) )
# 957 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 123 "minijavaparse.mly"
                               ( Operation(_1, Multiplication, _3) )
# 965 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 124 "minijavaparse.mly"
                              ( Operation(_1, Division, _3) )
# 973 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    Obj.repr(
# 125 "minijavaparse.mly"
                                        ( Subscript(_1, _3) )
# 981 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "minijavaparse.mly"
                              ( if _3="length" then Length _1 else FieldRef(_1, _3) )
# 989 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 128 "minijavaparse.mly"
                                                 ( MethodCall(_1, _3, _5) )
# 998 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "minijavaparse.mly"
                     ( Integer _1 )
# 1005 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 130 "minijavaparse.mly"
                   ( Float _1 )
# 1012 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "minijavaparse.mly"
                    ( String _1 )
# 1019 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "minijavaparse.mly"
                  ( Null )
# 1025 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 133 "minijavaparse.mly"
                     ( if _1 then True else False )
# 1032 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "minijavaparse.mly"
                 ( Id _1 )
# 1039 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "minijavaparse.mly"
           ( This )
# 1045 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'exp_type) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    Obj.repr(
# 136 "minijavaparse.mly"
                                          ( NewArray(_2, _4) )
# 1053 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 137 "minijavaparse.mly"
                                    ( NewId _2 )
# 1060 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Mp2common.exp) in
    Obj.repr(
# 138 "minijavaparse.mly"
                       ( Not _2 )
# 1067 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    Obj.repr(
# 139 "minijavaparse.mly"
                             ( _2 )
# 1074 "minijavaparse.ml"
               : Mp2common.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "minijavaparse.mly"
                    ( [] )
# 1080 "minijavaparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 143 "minijavaparse.mly"
                         ( _1 :: _2 )
# 1088 "minijavaparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "minijavaparse.mly"
                     ( [] )
# 1094 "minijavaparse.ml"
               : 'more_args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mp2common.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'more_args) in
    Obj.repr(
# 147 "minijavaparse.mly"
                               ( _2 :: _3 )
# 1102 "minijavaparse.ml"
               : 'more_args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry classdecl *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry stmt *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry expression *)
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Mp2common.program)
let classdecl (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Mp2common.class_decl)
let stmt (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Mp2common.statement)
let expression (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Mp2common.exp)
