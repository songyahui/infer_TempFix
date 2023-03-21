%{ open Ast_utility %}
%{ open List %}

(*%token <string> EVENT*)
%token <string> VAR
%token <int> INTE
%token EMPTY LPAR RPAR CONCAT  POWER  DISJ   
%token COLON  REQUIRE ENSURE FUTURESpec LSPEC RSPEC NULL
%token UNDERLINE KLEENE EOF BOTTOM NOTSINGLE RETURN
%token GT LT EQ GTEQ LTEQ CONJ COMMA MINUS 
%token PLUS TRUE FALSE 
%token FUTURE GLOBAL IMPLY LTLNOT NEXT UNTIL LILAND LILOR
%left DISJ 
%left CONCAT



%start specification
%type <(Ast_utility.specification)> specification
%type <(Ast_utility.pure)> pure
%type <(Ast_utility.effect option * Ast_utility.effect option * Ast_utility.effect option)> optionalPrecondition
%type <(Ast_utility.effect option * Ast_utility.effect option)> optionalPostcondition
%type <(Ast_utility.effect option)> optionalFuturecondition
%type <(Ast_utility.basic_type)> basic_type
%type <(Ast_utility.basic_type list)> parm
%type <(Ast_utility.es)> es
%type <(string list)> formalparm
%type <(Ast_utility.effect)> effect
%type <(Ast_utility.es)> es_or_ltl
%type <(Ast_utility.ltl)> ltl
%type <(Ast_utility.terms)> term
%%

basic_type : 
| i = INTE{      BINT ( i)
    }
| v = VAR {BVAR v} 
| NULL {BNULL}
| RETURN {BRET}

parm:
| {[]}
| LPAR argument= basic_type RPAR {[argument]}


es:
| BOTTOM {Bot}
| EMPTY {Emp}
| NOTSINGLE str = VAR p=parm { NotSingleton ((str, p)) }
| str = VAR p=parm { Singleton ((str, p), None) }
| LPAR r = es RPAR { r }
| a = es DISJ b = es { Disj(a, b) }
| UNDERLINE {Any}
| a = es CONCAT b = es { Concatenate (a, b) } 
| LPAR a = es  RPAR POWER KLEENE {Kleene a}

term:
| b = basic_type {Basic b}
| LPAR r = term RPAR { r }
| a = term b = INTE {Minus (a, Basic( BINT (0 -  b)))}
| LPAR a = term MINUS b = term RPAR {Minus (a, b )}
| LPAR a = term PLUS b = term RPAR {Plus (a, b)}


(*

pure_helper:
| GT b = term {(">", b)}
| LT b = term {("<", b)}
| GTEQ b = term {(">=", b)}
| LTEQ b = term {("<=", b)}
| EQ b = term {("=", b)}

pure_aux:
| CONJ b = pure {("conj", b)}
| DISJ b = pure {("disj", b)}

*)

pure:
| TRUE {TRUE}
| FALSE {FALSE}
| NOTSINGLE LPAR a = pure RPAR {Neg a}
| LPAR r = pure RPAR { r }
| a = term GT b = term {Gt (a, b)}
| a = term LT b = term {Lt (a, b)}
| a = term GTEQ b = term {GtEq (a, b)}
| a = term LTEQ b = term {LtEq (a, b)}
| a = term EQ b = term {Eq (a, b)}
| a = pure CONJ b = pure {PureAnd (a, b)}
| a = pure DISJ b = pure {PureOr (a, b)}

ltl : 
| str = VAR p=parm {Lable (str, p)} 
| LPAR r = ltl RPAR { r }
| NEXT p = ltl  {Next p}
| LPAR p1= ltl UNTIL p2= ltl RPAR {Until (p1, p2)}
| GLOBAL p = ltl {Global p}
| FUTURE p = ltl {Future p}
| LTLNOT p = ltl {NotLTL p}
| LPAR p1= ltl IMPLY p2= ltl RPAR {Imply (p1, p2)}
| LPAR p1= ltl LILAND p2= ltl RPAR {AndLTL (p1, p2)}  
| LPAR p1= ltl LILOR p2= ltl RPAR {OrLTL (p1, p2)}

es_or_ltl:
| COMMA  b= es  {b}
| COLON b = ltl {
  let rec ltlToEs l = 
    match l with 
    | Lable str ->  Singleton (str, None)
    | Next ltl -> Concatenate (Any, ltlToEs ltl)
    | Global ltl -> Kleene (ltlToEs ltl)
    | Future ltl -> Concatenate (Kleene Any, ltlToEs ltl)
    | OrLTL (ltl1, ltl2) -> Disj (ltlToEs ltl1, ltlToEs ltl2)
    | NotLTL (Lable str) ->  NotSingleton str 
    | Imply (Lable str, ltl2) -> Disj (NotSingleton str ,  ltlToEs ltl2)
    | Until (ltl1, ltl2) -> Concatenate(Kleene (ltlToEs ltl1), ltlToEs ltl2)
    (*
    | NotLTL of ltl 
    | Imply of ltl * ltl
    | AndLTL of ltl * ltl
     *)
    | _ ->  Singleton (("ltlToEs not yet", []), None)
  in ltlToEs b 
}

effect:
| LPAR r = effect RPAR { r }
| a = pure  b = es_or_ltl {[(a, b)]}
| a = effect  DISJ  b=effect  {List.append a b}



optionalFuturecondition:
| {None}
| FUTURESpec e3 = effect {Some e3}

optionalPostcondition:
| ENSURE e2 = effect a = optionalFuturecondition {
  (Some e2, a)
}
| a = optionalFuturecondition {
  (None, a)
}

optionalPrecondition:
| REQUIRE e1 = effect a = optionalPostcondition 
{let (e2, e3) = a in 
  (Some e1, e2, e3)}
| a = optionalPostcondition 
{let (e2, e3) = a in 
  (None, e2, e3)}


formalparm:
| {[]}
| str = VAR {[str]}
| str = VAR COMMA rest=formalparm {str:: rest}


specification: 
| EOF {(("", []), None, None, None)}
| LSPEC str = VAR LPAR argument=formalparm RPAR COLON 
a = optionalPrecondition 
RSPEC {
  let (e1, e2, e3) = a in 
  ((str, argument), e1, e2, e3)}


(*
specification: 
| EOF {("", None, None, None)}
| LSPEC str = VAR COLON 
REQUIRE e1=effect ENSURE e2=effect FUTURESpec e3=effect
RSPEC {  (str, Some e1, Some e2, Some e3)}
*)
