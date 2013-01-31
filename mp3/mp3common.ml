
type token = Id of string | Intlit of string | Floatlit of string
           | Lbrack | Rbrack | Divide | Errtok | EOF

type state = Start
           | BadChar | Whitespace | Lbracket | Rbracket
           | Ident
           | Int | IntDot | Float
           | Slash | CPPcomment | CPPcommentEnd
                   | Ccomment | CcommentStar | CcommentEnd
           | NoMove

type actions = Discard | Error | Token

