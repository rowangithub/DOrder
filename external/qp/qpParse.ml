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

open Parsing;;
# 3 "qpParse.mly"
open QpDag

(* module IntHashStruct = struct
  type t = int
  let equal x y = (x = y)
  let hash = Hashtbl.hash
end

module IntHash = Hashtbl.Make(IntHashStruct)
*)

module IntHash = QpMisc.IntHash

let pred_defs = IntHash.create 251

let exp_defs = IntHash.create 251


let  parse_error msg =
  QpErrormsg.error (symbol_start ()) msg

# 50 "qpParse.ml"
let yytransl_const = [|
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* LB *);
  262 (* RB *);
  263 (* EQ *);
  264 (* LEQ *);
  265 (* AND *);
  266 (* OR *);
  267 (* NOT *);
  268 (* IMPL *);
  269 (* IFF *);
  270 (* SEMI *);
  271 (* TRUE *);
  272 (* FALSE *);
    0 (* EOF *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* MUL *);
  276 (* DEF *);
  277 (* REF *);
  278 (* ITE *);
    0|]

let yytransl_block = [|
  257 (* Id *);
  258 (* Num *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\001\000\003\000\003\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\007\000\
\007\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\001\000\000\000\002\000\001\000\
\001\000\004\000\004\000\002\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\002\000\001\000\001\000\002\000\003\000\
\004\000\004\000\004\000\004\000\003\000\002\000\004\000\000\000\
\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\009\000\000\000\
\000\000\034\000\000\000\000\000\035\000\036\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\020\000\000\000\001\000\000\000\015\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\030\000\000\000\016\000\017\000\
\000\000\000\000\000\000\013\000\014\000\019\000\003\000\004\000\
\000\000\000\000\000\000\000\000\024\000\000\000\029\000\000\000\
\010\000\007\000\011\000\033\000\026\000\027\000\028\000\025\000\
\031\000"

let yydgoto = "\004\000\
\020\000\021\000\022\000\057\000\019\000\065\000\066\000"

let yysindex = "\008\000\
\061\255\061\255\061\255\000\000\000\000\061\255\034\255\034\255\
\007\255\012\255\061\255\061\255\061\255\000\000\000\000\021\255\
\023\255\000\000\016\255\061\255\000\000\000\000\017\255\022\255\
\013\255\000\000\027\255\025\255\002\255\031\255\037\255\061\255\
\034\255\034\255\061\255\061\255\000\000\061\255\061\255\061\255\
\000\000\061\255\000\000\061\255\000\000\034\255\034\255\034\255\
\000\000\034\255\041\255\034\255\000\000\034\255\000\000\000\000\
\042\255\061\255\044\255\000\000\000\000\000\000\000\000\000\000\
\034\255\053\255\052\255\055\255\000\000\034\255\000\000\034\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\059\000\000\000\000\000\063\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\255\060\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\071\255\072\255\072\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\060\255\000\000\000\000\000\000\000\000\000\000\000\000\
\015\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\078\000\060\000\039\000\220\255\002\000\251\255\215\255"

let yytablesize = 279
let yytable = "\059\000\
\021\000\033\000\034\000\050\000\023\000\067\000\068\000\024\000\
\001\000\002\000\003\000\035\000\037\000\038\000\039\000\046\000\
\036\000\047\000\032\000\051\000\032\000\074\000\040\000\076\000\
\041\000\045\000\049\000\055\000\056\000\042\000\044\000\048\000\
\052\000\054\000\025\000\026\000\058\000\058\000\053\000\060\000\
\061\000\062\000\070\000\063\000\069\000\023\000\071\000\073\000\
\072\000\075\000\027\000\028\000\029\000\030\000\031\000\032\000\
\077\000\078\000\002\000\058\000\079\000\005\000\005\000\006\000\
\080\000\006\000\081\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\032\000\014\000\015\000\032\000\018\000\043\000\
\016\000\017\000\064\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\021\000\000\000\021\000\000\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000"

let yycheck = "\036\000\
\000\000\007\000\008\000\002\001\003\000\047\000\048\000\006\000\
\001\000\002\000\003\000\005\001\011\000\012\000\013\000\003\001\
\005\001\005\001\004\001\018\001\006\001\058\000\002\001\065\000\
\002\001\004\001\002\001\033\000\034\000\014\001\014\001\005\001\
\002\001\032\000\001\001\002\001\035\000\036\000\002\001\038\000\
\039\000\040\000\002\001\042\000\050\000\044\000\052\000\006\001\
\054\000\006\001\017\001\018\001\019\001\020\001\021\001\022\001\
\004\001\006\001\000\000\058\000\006\001\001\001\000\000\003\001\
\070\000\006\001\072\000\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\004\001\015\001\016\001\006\001\001\000\020\000\
\020\001\021\001\044\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LB\000\
  RB\000\
  EQ\000\
  LEQ\000\
  AND\000\
  OR\000\
  NOT\000\
  IMPL\000\
  IFF\000\
  SEMI\000\
  TRUE\000\
  FALSE\000\
  EOF\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DEF\000\
  REF\000\
  ITE\000\
  "

let yynames_block = "\
  Id\000\
  Num\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : (QpDag.predicate * QpDag.predicate)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (QpDag.predicate * QpDag.predicate) list) in
    Obj.repr(
# 54 "qpParse.mly"
                   ( _1 :: _2 )
# 251 "qpParse.ml"
               : (QpDag.predicate * QpDag.predicate) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (QpDag.predicate * QpDag.predicate)) in
    Obj.repr(
# 55 "qpParse.mly"
                                       ( [ _1 ] )
# 258 "qpParse.ml"
               : (QpDag.predicate * QpDag.predicate) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : predicate) in
    Obj.repr(
# 59 "qpParse.mly"
                   ( (_1,_3) )
# 266 "qpParse.ml"
               : (QpDag.predicate * QpDag.predicate)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : QpDag.predicate list) in
    Obj.repr(
# 63 "qpParse.mly"
                     ( _1 :: _3 )
# 274 "qpParse.ml"
               : QpDag.predicate list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : predicate) in
    Obj.repr(
# 64 "qpParse.mly"
                                      ( [ _1 ] )
# 281 "qpParse.ml"
               : QpDag.predicate list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "qpParse.mly"
          ( [] )
# 287 "qpParse.ml"
               : ((predicate list))))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : predicate) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ((predicate list))) in
    Obj.repr(
# 69 "qpParse.mly"
                  ( _1 :: _2 )
# 295 "qpParse.ml"
               : ((predicate list))))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "qpParse.mly"
            ( pwr True  )
# 301 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "qpParse.mly"
             ( pwr False )
# 307 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ((predicate list))) in
    Obj.repr(
# 75 "qpParse.mly"
                       ( pwr (And (_3)) )
# 314 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ((predicate list))) in
    Obj.repr(
# 76 "qpParse.mly"
                     ( pwr (Or (_3)) )
# 321 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : predicate) in
    Obj.repr(
# 77 "qpParse.mly"
                ( pwr (Not (_2)) )
# 328 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : predicate) in
    Obj.repr(
# 78 "qpParse.mly"
                     ( pwr (Implies  (_2, _3)) )
# 336 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : predicate) in
    Obj.repr(
# 79 "qpParse.mly"
                                        ( pAnd[pOr[pNot _2; _3];pOr[_2; pNot _3]] )
# 344 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : predicate) in
    Obj.repr(
# 80 "qpParse.mly"
                         ( _2 )
# 351 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expression) in
    Obj.repr(
# 81 "qpParse.mly"
                   ( pwr (Equality(_2, _3)) )
# 359 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expression) in
    Obj.repr(
# 82 "qpParse.mly"
                   ( pwr (Leq(_2, _3)) )
# 367 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "qpParse.mly"
                                        ( pwr (Atom(_1)) )
# 374 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : predicate) in
    Obj.repr(
# 84 "qpParse.mly"
                                        ( IntHash.replace pred_defs _2 _3; _3 )
# 382 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 85 "qpParse.mly"
                                        ( try IntHash.find pred_defs _2 with Not_found -> raise Parsing.Parse_error )
# 389 "qpParse.ml"
               : predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "qpParse.mly"
                  ( ewr (Variable _1) )
# 396 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "qpParse.mly"
            ( ewr (Constant(Constant.Int _1)) )
# 403 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "qpParse.mly"
                                        ( ewr (Constant(Constant.Int (-_2))) )
# 410 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expression) in
    Obj.repr(
# 92 "qpParse.mly"
                                        ( ewr (Coeff (Constant.Int (_2)  , _3)) )
# 418 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : expression) in
    Obj.repr(
# 93 "qpParse.mly"
                                        ( ewr (Coeff (Constant.Int (-_3) , _4)) )
# 426 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exps) in
    Obj.repr(
# 94 "qpParse.mly"
                                  ( ewr (Application (_1,_3)) )
# 434 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exps) in
    Obj.repr(
# 95 "qpParse.mly"
                           ( ewr (Application (_1,_3)) )
# 442 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exps) in
    Obj.repr(
# 96 "qpParse.mly"
                                        ( ewr (Sum (_3)) )
# 449 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : expression) in
    Obj.repr(
# 97 "qpParse.mly"
                                        ( IntHash.replace exp_defs _2 _3; _3 )
# 457 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 98 "qpParse.mly"
                                        ( try IntHash.find exp_defs _2 with Not_found -> raise Parsing.Parse_error )
# 464 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : expression) in
    Obj.repr(
# 99 "qpParse.mly"
                                        ( ewr (Ite (_2,_3,_4)) )
# 473 "qpParse.ml"
               : expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "qpParse.mly"
         ( [] )
# 479 "qpParse.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 104 "qpParse.mly"
                ( _1 :: _2 )
# 487 "qpParse.ml"
               : 'exps))
(* Entry pred2 *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry pred2list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry predlist *)
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
let pred2 (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (QpDag.predicate * QpDag.predicate))
let pred2list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (QpDag.predicate * QpDag.predicate) list)
let predlist (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : QpDag.predicate list)
