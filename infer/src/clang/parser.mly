%{ open Ast_utility %}
%{ open List %}

(*%token <string> EVENT*)
%token <string> VAR
(*%token <int> INTE*)
%token EMPTY LPAR RPAR CONCAT  POWER  DISJ   
%token   COLON  REQUIRE ENSURE LSPEC RSPEC
%token UNDERLINE KLEENE EOF

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
| EMPTY { Emp }
| str = VAR (*p=parm*) { Singleton ( str) }
| LPAR r = es RPAR { r }
| a = es DISJ b = es { Disj(a, b) }
| UNDERLINE {Any}
| a = es CONCAT b = es { Concatenate (a, b) } 
| LPAR a = es POWER KLEENE RPAR{Kleene a}


specification: 
| EOF {("", Emp, Emp)}
| LSPEC str = VAR COLON REQUIRE e1 = es  ENSURE e2 = es RSPEC {(str, e1, e2)}


