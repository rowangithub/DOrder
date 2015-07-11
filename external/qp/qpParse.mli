type token =
  | Id of (string)
  | Num of (int)
  | LPAREN
  | RPAREN
  | LB
  | RB
  | EQ
  | LEQ
  | AND
  | OR
  | NOT
  | IMPL
  | IFF
  | SEMI
  | TRUE
  | FALSE
  | EOF
  | PLUS
  | MINUS
  | MUL
  | DEF
  | REF
  | ITE

val pred2 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (QpDag.predicate * QpDag.predicate)
val pred2list :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (QpDag.predicate * QpDag.predicate) list
val predlist :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> QpDag.predicate list
