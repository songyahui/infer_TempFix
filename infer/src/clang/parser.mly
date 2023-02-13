%{ open Ast_utility %}
%{ open List %}

(*%token <string> EVENT*)
%token <string> VAR
%token <int> INTE
%token EMPTY LPAR RPAR CONCAT  POWER  DISJ   
%token   COLON  REQUIRE ENSURE LSPEC RSPEC
%token UNDERLINE KLEENE EOF BOTTOM NOTSINGLE
%token GT LT EQ GTEQ LTEQ CONJ COMMA MINUS 
%token PLUS TRUE FALSE 
%left DISJ 
%left CONCAT



%start specification
%type <(Ast_utility.specification)> specification

%%


(*parm:
| {None}
| LPAR i=INTE RPAR {Some i}
*)

es:
| BOTTOM {Bot}
| EMPTY { Emp }
| NOTSINGLE str = VAR (*p=parm*) { NotSingleton ( str) }
| str = VAR (*p=parm*) { Singleton (str, None) }
| LPAR r = es RPAR { r }
| a = es DISJ b = es { Disj(a, b) }
| UNDERLINE {Any}
| a = es CONCAT b = es { Concatenate (a, b) } 
| LPAR a = es  RPAR POWER KLEENE {Kleene a}

term:
| str = VAR { Var str }
| n = INTE {Number n}
| LPAR r = term RPAR { r }
| a = term b = INTE {Minus (a, Number (0 -  b))}
| LPAR a = term MINUS b = term RPAR {Minus (a, b )}
| LPAR a = term PLUS b = term RPAR {Plus (a, b)}


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

effect:
| LPAR r = effect RPAR { r }
| a = pure  COMMA  b= es  {[(a, b)]}
| a = effect  DISJ  b=effect  {List.append a b}


specification: 
| EOF {("", [(TRUE, Emp)], [(TRUE, Emp)])}
(*
| EOF {("", Emp, Emp)}
*)
| LSPEC str = VAR COLON REQUIRE e1 = effect  ENSURE e2 = effect RSPEC {(str, e1, e2)}


