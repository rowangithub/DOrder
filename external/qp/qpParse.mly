
%{
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

%}

%token <string> Id
%token <int> Num
%token LPAREN  RPAREN LB RB
%token EQ LEQ
%token AND OR NOT IMPL IFF SEMI
%token TRUE FALSE
%token EOF
%token PLUS
%token MINUS
%token MUL
%token DEF
%token REF
%token ITE

%start pred2
%start pred2list
%start predlist

%type <(QpDag.predicate * QpDag.predicate) list> pred2list
%type <QpDag.predicate list> predlist
%type <(QpDag.predicate * QpDag.predicate)> pred2
%type <((predicate list))> preds
%type <predicate> pred
%type <expression> exp


%%
pred2list:
	pred2 pred2list			{ $1 :: $2 }
    |   pred2                         	{ [ $1 ] }
;

pred2:
    pred SEMI pred { ($1,$3) }
;

predlist:
	pred SEMI predlist		{ $1 :: $3 }
    |   pred                         	{ [ $1 ] }
;

preds:
    	 				{ [] }
  | pred preds				{ $1 :: $2 }
;

pred:
    TRUE				{ pwr True  }
  | FALSE				{ pwr False }
  | AND LB preds RB 			{ pwr (And ($3)) }
  | OR LB preds RB			{ pwr (Or ($3)) }
  | NOT pred				{ pwr (Not ($2)) }
  | IMPL pred pred			{ pwr (Implies  ($2, $3)) }
  | IFF pred pred                       { pAnd[pOr[pNot $2; $3];pOr[$2; pNot $3]] }
  | LPAREN pred RPAREN			{ $2 }
  | EQ  exp exp				{ pwr (Equality($2, $3)) }
  | LEQ  exp exp			{ pwr (Leq($2, $3)) }
  | Id                                  { pwr (Atom($1)) }
  | DEF Num pred                        { IntHash.replace pred_defs $2 $3; $3 }  
  | REF Num                             { try IntHash.find pred_defs $2 with Not_found -> raise Parsing.Parse_error }
;

exp:
    Id				        { ewr (Variable $1) }
  | Num 				{ ewr (Constant(Constant.Int $1)) }
  | MINUS Num                           { ewr (Constant(Constant.Int (-$2))) }
  | MUL Num exp                         { ewr (Coeff (Constant.Int ($2)  , $3)) }
  | MUL MINUS Num exp                   { ewr (Coeff (Constant.Int (-$3) , $4)) }
  | Id LPAREN exps RPAREN	        { ewr (Application ($1,$3)) }
  | Id LB exps RB		        { ewr (Application ($1,$3)) }
  | PLUS LB exps RB                     { ewr (Sum ($3)) }
  | DEF Num exp                         { IntHash.replace exp_defs $2 $3; $3 }  
  | REF Num                             { try IntHash.find exp_defs $2 with Not_found -> raise Parsing.Parse_error }
  | ITE pred exp exp                    { ewr (Ite ($2,$3,$4)) }
;

exps:
    					{ [] }
  | exp exps				{ $1 :: $2 }

