open Z3

type basic_type = BINT of int | BVAR of string | BNULL | BRET


type event = string * (basic_type list)


type ltl = Lable of event 
        | Next of ltl
        | Until of ltl * ltl
        | Global of ltl
        | Future of ltl
        | NotLTL of ltl
        | Imply of ltl * ltl
        | AndLTL of ltl * ltl
        | OrLTL of ltl * ltl

type line_number = int option

type bindings = (string * basic_type) list


type terms = Basic of basic_type 
           | Plus of terms * terms
           | Minus of terms * terms 
       
(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure


type es = Bot | Emp | Any 
              | Singleton of (event * line_number) 
              | NotArguments of (basic_type list) 
              | NotSingleton of event 
              | Disj of es * es 
              | Concatenate of es * es 
              | Kleene of es 


type effect = (pure * es) list 

(* pure /\ es, the first int is the exit code, and the "int list" is the footprint *)
type programState = (pure * es  * int * int list)

type programStates = (programState list)

type mnsigniture = (string *  (string list))

type specification = (mnsigniture * effect option * effect option * effect option)

type fstElem = Wildcard | Event of (event * line_number)  | NotEvent of event

type effectwithfootprint = (pure * es * int list)

(* Global States *)
let (dynamicSpec: (specification list) ref) = ref [] 
let (propogatedSpecs: (specification list) ref) = ref [] 
let (currentModule: string ref) = ref ""
let (currentModuleBody: (Clang_ast_t.stmt) option  ref) = ref None
let (currentLable: (string list)  ref) = ref []

let (variablesInScope: (string list) ref) = ref [] 
let (parametersInScope: (string list) ref) = ref [] 


let (varSet: (string list) ref) = ref [] 
let (handlerVar: string option ref) = ref None 

(* net value of request of proving, (total number * failed number)*)
let (proofObligations: int ref) = ref 0 
let (failedProofObligations: int ref) = ref 0 

(* number of the assertions are failed 
assertions are differnet from proof obligations 
because proof obligations can be triggerred while searching *)
let (failedAssertions: int ref) =  ref 0 
let (repairRecord:( (int * int) list) ref) = ref []  

(* number of the assertions are failed, which are fixed *)
let (reapiredFailedAssertions: int ref) =  ref 0 
let repairTime = ref 0.0
(* Experimental Summary *)
let currentFunctionLineNumber = ref (0, 0) 


let finalReport = (ref "")

let verifier_counter: int ref = ref 0;;

let end_of_var = Str.regexp "_?[0-9]+$"
let verifier_getAfreeVar from :string  =
  (* this prefix shows provenance, but that turned out to be useless *)
  (* let prefix = from |> Option.map (fun v -> v ^ "_") |> Option.value ~default:"_f" in *)
  let prefix =
    (* match from with *)
    (* | None -> "_f" *)
    (* | Some f -> *)
      Str.global_replace end_of_var "" from
  in
  let x = prefix ^ string_of_int (!verifier_counter) in 
  incr verifier_counter;
  x 


let programStates2effectwithfootprintlist eff = 
  List.map ~f:(fun (p, es, _, ft)-> (p, es, ft)) eff

let effects2programStates eff = 
  List.map eff ~f:(fun (p, es)-> (p, es, 0, []))

let programStates2effects eff = 
  List.map eff ~f:(fun (p, es, _ , _)-> (p, es))

let rec flattenList lili = 
  match lili with 
  | [] -> []
  | x :: xs -> List.append x (flattenList xs) 

  
let rec iter f = function
  | [] -> ()
  | [x] ->
      f true x
  | x :: tl ->
      f false x;
      iter f tl

let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
  let rec print_root indent x =
    bprintf buf "%s\n" (get_name x);
    let children = get_children x in
    iter (print_child indent) children
  and print_child indent is_last x =
    let line =
      if is_last then
        "└── "
      else
        "├── "
    in
    bprintf buf "%s%s" indent line;
    let extra_indent =
      if is_last then
        "    "
      else
        "│   "
    in
    print_root (indent ^ extra_indent) x
  in
  Buffer.add_string buf line_prefix;
  print_root line_prefix x

let printTree ?line_prefix ~get_name ~get_children x =
  let buf = Buffer.create 1000 in
  to_buffer ?line_prefix ~get_name ~get_children buf x;
  Buffer.contents buf

type binary_tree =
  | Node of string * (binary_tree  list )
  | Leaf

let get_name = function
    | Leaf -> "."
    | Node (name, li) -> name;;

let get_children = function
    | Leaf -> []
    | Node (_, li) -> List.filter ~f:(fun a ->
      match a with 
      | Leaf -> false 
      | _ -> true ) li;;


let string_of_basic_t v = 
  match v with 
  | BVAR name -> name
  | BINT n -> string_of_int n
  | BNULL -> "NULL"
  | BRET -> ".ret"



  

let basic_type2_string v = 
  match v with 
  | BVAR name -> [name]
  | BINT _ 
  | BNULL 
  | BRET -> []



let argumentsTerms2basic_types (t: (terms option) list): (basic_type list) = 
  List.fold_left t ~init:[] ~f:(fun acc a ->
    match a with 
    | Some (Basic (BVAR str)) -> List.append acc [(BVAR str)]
    | _ -> acc 
  )


let rec string_of_terms (t:terms):string = 
  match t with
  | Basic v -> string_of_basic_t v 
  | Plus (t1, t2) -> (string_of_terms t1) ^ ("_plus_") ^ (string_of_terms t2)
  | Minus (t1, t2) -> (string_of_terms t1) ^ ("_minus_") ^ (string_of_terms t2)


let string_of_termOption t : string option  = 
  match t with 
  | None -> None 
  | Some t -> Some (string_of_terms t)

let rec string_of_pure (p:pure):string =   
  match p with
    TRUE -> "⊤"
  | FALSE -> "⊥"
  | Gt (t1, t2) -> (string_of_terms t1) ^ ">" ^ (string_of_terms t2)
  | Lt (t1, t2) -> (string_of_terms t1) ^ "<" ^ (string_of_terms t2)
  | GtEq (t1, t2) -> (string_of_terms t1) ^ "≥" ^ (string_of_terms t2)
  | LtEq (t1, t2) -> (string_of_terms t1) ^ "≤" ^ (string_of_terms t2)
  | Eq (t1, t2) -> (string_of_terms t1) ^ "=" ^ (string_of_terms t2)
  | PureOr (p1, p2) -> "("^string_of_pure p1 ^ "∨" ^ string_of_pure p2^")"
  | PureAnd (p1, p2) -> string_of_pure p1 ^ "∧" ^ string_of_pure p2
  | Neg (Eq (t1, t2)) -> "("^(string_of_terms t1) ^ "!=" ^ (string_of_terms t2)^")"
  | Neg p -> "!(" ^ string_of_pure p^")"

let rec string_of_pure_output (p:pure):string =   
  match p with
    TRUE -> "⊤"
  | FALSE -> "⊥"
  | Gt (t1, t2) -> (string_of_terms t1) ^ ">" ^ (string_of_terms t2)
  | Lt (t1, t2) -> (string_of_terms t1) ^ "<" ^ (string_of_terms t2)
  | GtEq (t1, t2) -> (string_of_terms t1) ^ ">=" ^ (string_of_terms t2)
  | LtEq (t1, t2) -> (string_of_terms t1) ^ "<=" ^ (string_of_terms t2)
  | Eq (t1, t2) -> (string_of_terms t1) ^ "==" ^ (string_of_terms t2)
  | PureOr (p1, p2) -> "("^string_of_pure_output p1 ^ "∨" ^ string_of_pure_output p2^")"
  | PureAnd (p1, p2) -> string_of_pure_output p1 ^ "∧" ^ string_of_pure_output p2
  | Neg (Eq (t1, t2)) -> "("^(string_of_terms t1) ^ "!=" ^ (string_of_terms t2)^")"
  | Neg p -> "!(" ^ string_of_pure_output p^")"

let rec varFromTerm (t:terms): string list =   
  match t with
  | Basic (BVAR v) -> [v]
  | Plus (t1, t2) 
  | Minus (t1, t2) ->  List.append (varFromTerm t1) (varFromTerm t2)
  | _ -> []

let string_of_varSet (li: string list) : string = 
  (List.fold_left li ~init:"" ~f:(fun acc a -> acc ^ "," ^ a)) ^ "\n"


let rec getRoot str = 
  let strLi = String.split_on_chars  str ['.'] in 
  match strLi with
  | [] -> str 
  | x :: _ -> x
;;

let rec getMostRoot str = 
  let strLi = String.split_on_chars  str ['.'] in 
  let rec helper acc li =
    match li with
    | []
    | [_]  -> acc  
    | x :: rest -> helper (acc@[x]) rest
  in 
  let dereferencelist = helper [] strLi in 
  let rec aux li =
    match li with 
    | [] -> ""
    | [x] ->  x 
    | x :: xs -> x ^ "." ^ aux xs
  in  aux dereferencelist
;;

let rec string_with_seperator f li sep = 
  match li with 
  | [] -> ""
  | [x] -> f x 
  | x :: xs  -> f x ^ sep ^ string_with_seperator f xs sep



let twoStringSetOverlap (sli1) (sli2) = 
  let rec helper str li = 
    match li with 
    | [] -> false 
    | x :: xs -> if String.compare (getRoot x) str == 0 ||  String.compare (x) str == 0 then true else helper str xs 
  in 
  let rec aux li = 
    match li with 
    | [] -> false 
    | y :: ys -> if helper y sli2 then true else aux ys
  in aux sli1
 
let rec varFromPure (p:pure): string list =   
    match p with
    TRUE -> []
  | FALSE -> []
  | Gt (t1, t2) 
  | Lt (t1, t2) 
  | GtEq (t1, t2) 
  | LtEq (t1, t2) 
  | Eq (t1, t2) -> List.append (varFromTerm t1) (varFromTerm t2)
  | PureOr (p1, p2) 
  | PureAnd (p1, p2) -> List.append (varFromPure p1) (varFromPure p2)
  | Neg p -> varFromPure p 


let rec varFromEffects (eff:effect option) :  string list =  
  match eff with 
  | None -> []
  | Some eff -> 
  flattenList (List.map eff ~f:(fun (a, b) -> varFromPure a))
  
(**********************************************)
exception FooAskz3 of string

let rec convertTerm (t:terms):string = 
  match t with
  | (Basic (BVAR name)) -> " " ^ name ^ " "
  | (Basic (BINT n)) -> " " ^ string_of_int n ^ " "
  | (Basic (BNULL)) -> " " ^ "nil" ^ " "
  | Plus (t1, t2) -> ("(+") ^ (convertTerm t1) ^  (convertTerm t2) ^ ")"
  | Minus (t1, t2) -> ("(-") ^ (convertTerm t1) ^  (convertTerm t2) ^ ")"
  | _ -> "convertTerm error"
  ;;

let rec convertPure (pi:pure) (acc:string):string = 
  match pi with
    TRUE -> "(< 0 1)"
  | FALSE -> "(> 0 1)"
  | Gt (t1, t2) -> 
      let temp1 = convertTerm t1 in
      let temp2 = convertTerm t2 in
      acc ^ "(>" ^ temp1 ^ temp2 ^")"
  | Lt (t1, t2) -> 
      let temp1 = convertTerm t1 in
      let temp2 = convertTerm t2 in
      acc ^ "(<" ^ temp1 ^ temp2 ^")"
  | GtEq (t1, t2) -> 
      let temp1 = convertTerm t1 in
      let temp2 = convertTerm t2 in
      acc ^ "(>=" ^ temp1 ^ temp2 ^")"
  | LtEq (t1, t2) -> 
      let temp1 = convertTerm t1 in
      let temp2 = convertTerm t2 in
      acc ^ "(<=" ^ temp1 ^ temp2 ^")"
  | Eq (t1, t2) -> 
      let temp1 = convertTerm t1 in
      let temp2 = convertTerm t2 in
      acc ^ "(=" ^ temp1 ^ temp2 ^")"
  | PureAnd (pi1,pi2) -> 
      let temp1 = convertPure pi1 "" in
      let temp2 = convertPure pi2 "" in
      acc ^ "(and" ^temp1 ^ temp2 ^ ")"
  | Neg piN -> 
      let temp1 = convertPure piN "" in
      acc ^ "(not" ^temp1 ^ ")"
  | PureOr (pi1,pi2) -> 
      let temp1 = convertPure pi1 "" in
      let temp2 = convertPure pi2 "" in
      acc ^ "(or" ^temp1 ^ temp2 ^ ")"
      ;;




let rec getAllVarFromTerm (t:terms) (acc:string list):string list = 
  match t with
| Basic (BVAR name) -> List.append acc [name]
| Plus (t1, t2) -> 
    let cur = getAllVarFromTerm t1 acc in 
    getAllVarFromTerm t2 cur
| Minus (t1, t2) -> 
    let cur = getAllVarFromTerm t1 acc in 
    getAllVarFromTerm t2 cur
| _ -> acc
;;



let rec getAllVarFromES (es:es): string list = 
  match es with   
  | Bot | Emp | Any -> []
  | NotSingleton (str, btList) 
  | Singleton ((str, btList), _) ->  
    List.fold_left btList ~init:[] ~f:(fun acc a -> List.append acc (basic_type2_string a))
    
  | Disj(es1, es2) -> List.append (getAllVarFromES es1) (getAllVarFromES es2)
  | Concatenate (es1, es2) -> List.append (getAllVarFromES es1) (getAllVarFromES es2)
  | Kleene es1 -> (getAllVarFromES es1)
  | _ -> ["getAllVarFromES error"]



let rec getAllVarFromPure (pi:pure) (acc:string list):string list = 
  match pi with
    TRUE -> acc
  | FALSE -> acc
  | Gt (term1, term2) -> 
      let allVarFromTerm1 = getAllVarFromTerm term1 [] in
      let allVarFromTerm2 = getAllVarFromTerm term2 [] in
      List.append acc (List.append allVarFromTerm1 allVarFromTerm2)
  | Lt (term1, term2) -> 
      let allVarFromTerm1 = getAllVarFromTerm term1 [] in
      let allVarFromTerm2 = getAllVarFromTerm term2 [] in
      List.append acc (List.append allVarFromTerm1 allVarFromTerm2)
  | GtEq (term1, term2) -> 
      let allVarFromTerm1 = getAllVarFromTerm term1 [] in
      let allVarFromTerm2 = getAllVarFromTerm term2 [] in
      List.append acc (List.append allVarFromTerm1 allVarFromTerm2)
  | LtEq (term1, term2) -> 
      let allVarFromTerm1 = getAllVarFromTerm term1 [] in
      let allVarFromTerm2 = getAllVarFromTerm term2 [] in
      List.append acc (List.append allVarFromTerm1 allVarFromTerm2)
  | Eq (term1, term2) -> 
      let allVarFromTerm1 = getAllVarFromTerm term1 [] in
      let allVarFromTerm2 = getAllVarFromTerm term2 [] in
      List.append acc (List.append allVarFromTerm1 allVarFromTerm2)
  | PureAnd (pi1,pi2) -> 
      let temp1 = getAllVarFromPure pi1 [] in
      let temp2 = getAllVarFromPure pi2 [] in
      List.append acc (List.append temp1 temp2) 
  | Neg piN -> 
      List.append acc (getAllVarFromPure piN [])
  | PureOr (pi1,pi2) -> 
      let temp1 = getAllVarFromPure pi1 [] in
      let temp2 = getAllVarFromPure pi2 [] in
      List.append acc (List.append temp1 temp2) 
  ;;


let addAssert (str:string) :string =
  "(assert " ^ str ^ " ) \n (check-sat) \n"
  ;;

let counter : int ref = ref 0 ;;


let (historyTable: ((string * bool)list)ref) = ref [] ;;

let rec existInhistoryTable pi table= 
  match table with 
  | [] -> None
  | (x, b)::xs -> 
    if String.compare x (string_of_pure pi) == 0 then Some b 
    else existInhistoryTable pi  xs




let rec term_to_expr ctx : terms -> Z3.Expr.expr = function
  | (Basic(BINT n))        -> Z3.Arithmetic.Real.mk_numeral_i ctx n
  | (Basic(BVAR v))           -> Z3.Arithmetic.Real.mk_const_s ctx v
  | (Basic(BNULL))           -> Z3.Arithmetic.Real.mk_const_s ctx "nil"
  | (Basic(BRET))           -> Z3.Arithmetic.Real.mk_const_s ctx "ret"

  (*
  | Gen i          -> Z3.Arithmetic.Real.mk_const_s ctx ("t" ^ string_of_int i ^ "'")
  *)
  | Plus (t1, t2)  -> Z3.Arithmetic.mk_add ctx [ term_to_expr ctx t1; term_to_expr ctx t2 ]
  | Minus (t1, t2) -> Z3.Arithmetic.mk_sub ctx [ term_to_expr ctx t1; term_to_expr ctx t2 ]


let rec pi_to_expr ctx : pure -> Expr.expr = function
  | TRUE                -> Z3.Boolean.mk_true ctx
  | FALSE               -> Z3.Boolean.mk_false ctx
  | Gt (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_gt ctx t1 t2
  | GtEq (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_ge ctx t1 t2
  | Lt (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_lt ctx t1 t2
  | LtEq (t1, t2) -> 
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      Z3.Arithmetic.mk_le ctx t1 t2
  | Eq (t1, t2) -> 
      let newP = PureAnd (GtEq(t1, t2), LtEq(t1, t2)) in 
      pi_to_expr ctx newP
(*
  | Atomic (op, t1, t2) -> (
      let t1 = term_to_expr ctx t1 in
      let t2 = term_to_expr ctx t2 in
      match op with
      | Eq -> Z3.Boolean.mk_eq ctx t1 t2
      | Lt -> Z3.Arithmetic.mk_lt ctx t1 t2
      | Le -> Z3.Arithmetic.mk_le ctx t1 t2
      | Gt -> Z3.Arithmetic.mk_gt ctx t1 t2
      | Ge -> Z3.Arithmetic.mk_ge ctx t1 t2)
      *)
  | PureAnd (pi1, pi2)      -> Z3.Boolean.mk_and ctx [ pi_to_expr ctx pi1; pi_to_expr ctx pi2 ]
  | PureOr (pi1, pi2)       -> Z3.Boolean.mk_or ctx [ pi_to_expr ctx pi1; pi_to_expr ctx pi2 ]
  (*| Imply (pi1, pi2)    -> Z3.Boolean.mk_implies ctx (pi_to_expr ctx pi1) (pi_to_expr ctx pi2)
  *)
  | Neg pi              -> Z3.Boolean.mk_not ctx (pi_to_expr ctx pi)


let check pi =
  let cfg = [ ("model", "false"); ("proof", "false") ] in
  let ctx = mk_context cfg in
  let expr = pi_to_expr ctx pi in
  (* print_endline (Expr.to_string expr); *)
  let goal = Goal.mk_goal ctx true true false in
  (* print_endline (Goal.to_string goal); *)
  Goal.add goal [ expr ];
  let solver = Solver.mk_simple_solver ctx in
  List.iter ~f:(fun a -> Solver.add solver [ a ]) (Goal.get_formulas goal);
  let sat = Solver.check solver [] == Solver.SATISFIABLE in
  (* print_endline (Solver.to_string solver); *)
  sat

let askZ3 pi = 
  match existInhistoryTable pi !historyTable with 
  | Some b  -> b
  | None ->
  
  let _ = counter := !counter + 1 in 
  let re = check pi in 
  let ()= historyTable := (string_of_pure pi, re)::!historyTable in 
  
  re;;


let entailConstrains pi1 pi2 = 

  let sat = not (askZ3 (Neg (PureOr (Neg pi1, pi2)))) in
  (*
  print_string (string_of_pure pi1 ^" -> " ^ string_of_pure pi2 ^" == ");
  print_string (string_of_bool (sat) ^ "\n");
  *)
  sat;;
  
(* 
let askZ3 pi = 
  let _ = counter := !counter + 1 in 
  (*
  let startTimeStamp = Sys.time() in
  *)
  
  let inFile = Sys.getcwd () ^ "/askZ3.txt" in
  let outFile = Sys.getcwd () ^ "/answerZ3.txt" in 
  let declear = List.fold_right (fun v acc ->acc ^ ("(declare-const " ^ v ^ " Int)\n") ) (checkRedundant (getAllVarFromPure pi [])) "" in
  let assertions = addAssert (convertPure (pi) "") in
  let oc = open_out inFile in    (* 新建或修改文件,返回通道 *)
      (*print_string (declear^assertions^"\n************\n");   (* 写一些东西 *)
      *)
      fprintf oc "%s\n" (declear^assertions);   (* 写一些东西 *)
      close_out oc;                (* 写入并关闭通道 *)
      ignore (Sys.command ("z3 -smt2 "^ inFile ^" > " ^ outFile));
      let ic = open_in outFile in
      try 
        let line = input_line ic in  (* 从输入通道读入一行并丢弃'\n'字符 *)
        close_in ic ;                 (* 关闭输入通道 *) 
        (*
        let verification_time = "[askZ3 Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ " s]\n" in

        print_string (verification_time); 
        *)
        match line with 
        "sat" -> true
        | "unsat" -> false 
        | _ -> false 
      with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
     
*)

(***********************************************)

let string_of_binary_tree tree = printTree ~line_prefix:"* " ~get_name ~get_children tree;; 

let string_of_event (str, li) = 
  let temp = 
    match li with 
    | [] -> ""
    | [x] ->  string_of_basic_t x 
    | x::xs->
      List.fold_left xs 
      ~init:(string_of_basic_t x) 
      ~f:(fun acc a -> acc ^ "," ^ string_of_basic_t a )
  in 
  str ^ "("^temp^")"

let rec string_of_es (eff:es) : string = 
  match eff with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐"
  | Any -> "_" 
  | Singleton (str, l)  -> 
    string_of_event str ^ (match l with | None -> "" | Some i -> "@"^ string_of_int i)
  (* | NotArguments (x::_) -> -> "!_" ^ string_of_basic_t x  
  currently NotArguments is represented using NotSingleton _
  *)
  | NotSingleton str          -> "!" ^ string_of_event str 
  | Concatenate (eff1, eff2) ->
      string_of_es eff1 ^ " · " ^ string_of_es eff2 
  | Disj (eff1, eff2) ->
      "(" ^ string_of_es eff1 ^ " \\/ " ^ string_of_es eff2 ^ ")"
  | Kleene effIn          ->
      "(" ^ string_of_es effIn ^ ")^*"
      

  | _ -> "string_of_es error"

let string_of_exists exs = 
  if List.length exs == 0 then ""
  else 
    let rec aux li  =
      match li with
      | [x] -> x 
      | x :: xs -> x ^ "," ^ aux xs 
      | [] -> ""
    in aux exs ^ ". "

      
let rec string_of_effect (eff:effect) : string = 
  match eff with 
  | [] -> ""
  | [(pi, es)] ->  "(" ^ string_of_pure pi ^ " /\\ " ^ string_of_es es ^ ")"
  | (pi, es) :: xs ->  "(" ^ string_of_pure pi ^ " /\\ " ^ string_of_es es ^ ") \\/ " ^ string_of_effect xs
  
let rec string_of_programStates (eff:programStates) : string = 
  match eff with 
  | [] -> ""
  | [(pi, es, code, _)] ->  "(" ^ string_of_pure pi ^ " /\\ " ^ string_of_es es ^"," ^ string_of_int code ^")"
  | (pi, es, code, _) :: xs ->  "(" ^ string_of_pure pi ^ " /\\ " ^ string_of_es es ^"," ^ string_of_int code ^ ") \\/ " ^ string_of_programStates xs

let compareBasic_type (bt1:basic_type) (bt2:basic_type) : bool = 
  match (bt1, bt2) with 
  | ((BVAR s1), (BVAR s2)) -> String.compare s1 s2 == 0
  | (BINT n1, BINT n2) -> n1 == n2 
  | (BNULL, BNULL)
  | (BRET, BRET) -> true 
  | _ -> false 

let rec stricTcompareTerm (term1:terms) (term2:terms) : bool = 
  match (term1, term2) with 
  | (Basic t1, Basic t2) -> compareBasic_type t1 t2
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | _ -> false 

let rec comparePure (pi1:pure) (pi2:pure):bool = 
  match (pi1 , pi2) with 
    (TRUE, TRUE) -> true
  | (FALSE, FALSE) -> true 
  | (Gt (t1, t11), Gt (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Lt (t1, t11), Lt (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (GtEq (t1, t11), GtEq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (LtEq (t1, t11), LtEq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Eq (t1, t11), Eq (t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (PureOr (p1, p2), PureOr (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (PureAnd (p1, p2), PureAnd (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (Neg p1, Neg p2) -> comparePure p1 p2
  | _ -> false



let rec getAllPi piIn acc= 
    (match piIn with 
      PureAnd (pi1, pi2) -> List.append (getAllPi pi1 acc ) (getAllPi pi2 acc )
    | _ -> List.append acc [piIn]
    )
let rec existPi pi li = 
    (match li with 
      [] -> false 
    | x :: xs -> if comparePure pi x then true else existPi pi xs 
    )

let rec normalPure (pi : pure) : pure =
  let rec helper pi =
    match pi with
    | PureAnd (p1, p2) ->
      let p1 = normalPure p1 in
      let p2 = normalPure p2 in
      (match (p1, p2) with
      | TRUE, _ -> p2
      | _, TRUE -> p1
      | FALSE, _ -> FALSE
      | _, FALSE -> FALSE
      | _, _ -> PureAnd (p1, p2))
    | PureOr (p1, p2) ->
      let p1 = normalPure p1 in
      let p2 = normalPure p2 in
      (match (p1, p2) with
      | TRUE, _ -> TRUE
      | _, TRUE -> TRUE
      | FALSE, _ -> p2
      | _, FALSE -> p1
      | _, _ -> PureOr (p1, p2))
    | Neg p1 -> Neg (normalPure p1)
    | Eq (t1, t2) -> 
      if String.compare (string_of_terms t1) (string_of_terms t2) == 0 
      then TRUE else pi
    | _ -> pi
  in 
  if entailConstrains pi FALSE then FALSE 
  else
    helper pi
    
(*
let rec normalPure (pi:pure):pure = 
  let allPi = getAllPi pi [] in
  let rec clear_Pi pi li = 
    (match li with 
      [] -> [pi]
    | x :: xs -> if existPi pi li then clear_Pi x xs else List.append [pi] (clear_Pi x xs)
    )in 
  let finalPi = clear_Pi TRUE allPi in
  let rec connectPi li acc = 
    (match li with 
      [] -> acc 
    | x :: xs -> if entailConstrains TRUE x then (connectPi xs acc) else PureAnd (x, (connectPi xs acc)) 
    ) in 
  let filte_true = List.filter finalPi ~f:(fun ele-> not (comparePure ele TRUE)  ) in 
  if List.length filte_true == 0 then  TRUE
  else 
    (match filte_true with 
    | [] -> TRUE
    | x :: xs ->connectPi xs x
    )
*)

let rec nullable (eff:es) : bool = 
  match eff with 
  | Bot              -> false 
  | Emp              -> true 
  | Any              -> false 
  | Singleton ((str, _), _) -> 
    if String.compare str "RET" == 0 then true else false 
  | NotSingleton str          -> false
  | Concatenate (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Disj (eff1, eff2) -> nullable eff1 || nullable eff2  
  | Kleene effIn      -> true
  | _ -> false 



let rec normalise_es (eff:es) : es = 
  match eff with 
  | Disj(es1, es2) -> 
    let es1 = normalise_es es1 in 
    let es2 = normalise_es es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Emp, _) -> if nullable es2 then es2 else (Disj (es1, es2))
    | (Bot, es) -> normalise_es es 
    | (es, Bot) -> normalise_es es 
    | (Disj (es11, es12), es3) -> Disj (es11, Disj (es12, es3))
    | _ -> 
      (Disj (es1, es2))
    )
  | Concatenate (es1, es2) -> 
    let es1 = normalise_es es1 in 
    let es2 = normalise_es es2 in 
    (match (es1, es2) with 
    | (Emp, _) -> normalise_es es2
    | (_, Emp) -> normalise_es es1
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Disj (es11, es12), es3) -> Disj(Concatenate (es11,es3),  Concatenate (es12, es3))
    | (Concatenate (es11, es12), es3) -> (Concatenate (es11, Concatenate (es12, es3)))
    | _ -> (Concatenate (es1, es2))
    )
  | Kleene effIn -> 
    let effIn' = normalise_es effIn in 
    (match effIn' with 
    | Emp -> Emp 
    | _ ->  
    Kleene (effIn'))
  | _ -> eff 


let comapreEvents (str1, li1) (str2, li2) = 
  let rec aux l1 l2  =
    match (l1, l2) with 
    | ([], []) -> true 
    | (x::xs, y:: ys) -> compareBasic_type x y &&  aux xs ys
    | (_, _) -> false 
  in 
  String.compare str1 str2 == 0 && aux li1 li2

let compareLineNumOption (l1:int option) (l2:int option) : bool = 
  match (l1, l2) with 
  | (_, None) -> true 
  | (Some i1, Some i2) -> i1 == i2
  | (_, _) -> false 


let rec comparees (eff1:es) (eff2:es): bool =
  match (eff1, eff2) with 
  | (Bot, Bot) 
  | (Any, Any) 
  | (Emp, Emp) -> true 
  | (Singleton (s1, line1), Singleton (s2, line2)) -> 
    if comapreEvents s1 s2 == true && compareLineNumOption line1 line2 
    then true else false 
  | (NotSingleton s1, NotSingleton s2) -> 
    if comapreEvents s1 s2 == true  then true else false 
  | (Concatenate (a1, a2), Concatenate(a3, a4)) 
  | (Disj (a1, a2), Disj(a3, a4)) -> 
    comparees a1 a3 && comparees a2 a4
  | (Kleene e1, Kleene e2) -> comparees e1 e2
  | _ -> false 




let rec isBot (eff:es) : bool =
  match eff with 
  | Bot -> true 
  | _ -> false 

let rec existEff acc ( pi, es) : bool = 
  match acc with 
  | [] -> false 
  | (pi1, es1) :: xs -> if comparePure pi1 pi && comparees es1 es then true 
  else existEff xs ( pi, es) 

let normalise_effect (eff:effect) : effect = 
  let temp = List.map eff ~f:(fun (pi, es) -> ( normalPure pi, normalise_es es)) in 
  let noBoteff = List.filter temp ~f:(fun ( _, es) -> not (isBot es)) in 
  let rec helper effList = 
    match effList with 
    | [] -> []
    | x :: xs  -> if existEff xs x then helper xs else x :: helper xs
    
  in helper noBoteff




let rec fst (eff:es) : (fstElem list) = 
  match eff with 
  | Bot                
  | Emp             -> []
  | Any             -> [ Wildcard ]  
  | Singleton ((str, args), ft) -> if String.compare str "RET" == 0 then [] else [(Event ((str, args), ft))]  
  | NotSingleton str          -> [(NotEvent str)] 
  | Concatenate (eff1, eff2) -> 
    if nullable eff1 then List.append (fst eff1) (fst eff2)
    else (fst eff1)
  | Disj (eff1, eff2) -> List.append (fst eff1) (fst eff2)
  | Kleene effIn      -> (fst effIn) 
  | _ -> []

let rec exists_basic_type (t:basic_type) (li:basic_type list) : bool = 
    match li with 
    | [] -> false 
    | x :: xs -> if compareBasic_type t x then true else exists_basic_type t xs 



let rec basic_type_common (bt1:basic_type list) (bt2:basic_type list) : bool = 
  match bt1 with 
  | [] -> false  
  | y::ys -> if exists_basic_type y bt2 == true then true else basic_type_common ys bt2
 

let rec basic_type_subset (bt1:basic_type list) (bt2:basic_type list) : bool = 
  match bt1 with 
  | [] -> true 
  | y::ys -> if exists_basic_type y bt2 == false then false else basic_type_subset ys bt2

  (* arg1 is object.field, and arg2 is object *)
let rec comapreEventArgs arg1 arg2 = 
  let compareBasic_type_custimise (bt1:basic_type) (bt2:basic_type) : bool = 
    match (bt1, bt2) with 
    | ((BVAR s1), (BVAR s2)) -> 
      (*print_endline ("s1 =" ^ s1 ^ ", s2 =" ^ s2 ^"."); 
      print_endline ("getRoot s1 =" ^ (getRoot s1) ^ ", s2 =" ^ s2^"."); 
      print_endline (string_of_int (String.compare (getRoot s1) s2));
*)
      if (String.compare (getRoot s1) s2 == 0 || String.compare s1 s2 == 0 ) then true else false  
    | (BINT n1, BINT n2) -> n1 == n2 
    | (BNULL, BNULL)
    | (BRET, BRET) -> true 
    | _ -> false 
  in 
  match (arg1, arg2) with 
  | ([x], y::ys) -> if compareBasic_type_custimise x y then true else comapreEventArgs arg1 ys 
  | _ -> false 

let rec derivitives (f:fstElem) (eff:es) : es = 
  match eff with 
  | Bot        
  | Emp   -> Bot                
  | Any   -> Emp
  | Singleton (event, _) -> 
    let (str, args) = event in 
    (match f with 
    | Wildcard _ -> Bot 
    | Event (event1, _) -> 
      let (str1, args1) = event1 in 
      (*print_endline (string_of_event event1 ^ " |- " ^ string_of_event event); *)
      if (String.compare str1 "CONSUME" == 0 || String.compare str1 str ==0)  && comapreEventArgs args args1 
      then (Emp)  else Bot
      (*if String.compare str1 "CONSUME" == 0 && comapreEventArgs args args1 then (Emp) 
      else 
        if comapreEvents event event1 == true then Emp else Bot 
        *)
    | NotEvent event  ->  Bot
    )
    (*grub_gpt_read(dev.disk) event |- !_(dev)   ep  *)
  | NotSingleton str -> 
    (*print_endline ("current event = " ^ string_of_event str);*)
    let (ename, ep) = str in 
    if String.compare ename "_" == 0 then 
      (match f with 
      | Wildcard _ -> Bot 
      | Event ((str1, event), _) -> 
        (*print_endline ("fst event = " ^ string_of_event (str1, event));*)
        if String.compare str1 "CONSUME" == 0 then 
          Emp
        else 
          if (basic_type_common ep event == true) || (comapreEventArgs event ep) then Bot else Emp
      | NotEvent (_, event)  ->  if basic_type_subset ep event   == true then Emp else Bot
      )

    else 
      (match f with 
      | Wildcard _ -> Bot 
      | Event (event, _) -> if comapreEvents str event == true then Bot else Emp
      | NotEvent event  ->  if comapreEvents str event == true then Emp else Bot
      )
  | Concatenate (eff1, eff2) -> 
    if nullable eff1 then 
      Disj (
        Concatenate (derivitives f eff1, eff2), 
                     derivitives f eff2)
    else Concatenate (derivitives f eff1, eff2)
  | Disj (eff1, eff2) -> Disj (derivitives f eff1, derivitives f eff2)
  | Kleene effIn      -> Concatenate (derivitives f effIn, eff)
  | _ -> eff

let showEntailemnt (lhs:es) (rhs:es) : string =
  string_of_es lhs  ^" |- "^ string_of_es rhs ;;


let rec deleteFromEnv (env:(specification list)) (fname:string): ((specification list)) = 
  match env with
  | [] -> []
  | x :: xs -> 
    let ((fName, li), _,_, _) =  x in 
    if String.compare fName fname == 0 then xs else x :: (deleteFromEnv xs fname)

let compareEntailents (e1, e2) (e3, e4) : bool =
  comparees e1 e3 && comparees e2 e4

let rec reoccur (lhs:es) (rhs:es) (ctx: (es*es)list): bool = 
  match ctx with 
  | [] -> false 
  | (a, b):: xs -> 
    if compareEntailents (a, b) (lhs, rhs) 
    then true 
    else reoccur lhs rhs xs
  ;;



(*specLHS ｜- specRHS；
-
-
A.b.c |- A.M.C
b.c ｜- M.C

A . C.  B |- B .C. A \/ F.G.H

find the most similar trace to repair

A =B
B =A
*)

let rec inclusion (lhs:es) (rhs:es) (ctx: (es*es) list): (bool* binary_tree) =
  let lhs = normalise_es lhs in 
  let rhs = normalise_es rhs in  
  let entailent = showEntailemnt lhs rhs in 
  (*print_string (entailent ^ "\n");*)
  if isBot lhs then (true, Node (entailent ^ "  [False LHS]", []) )
  else if nullable lhs && (not (nullable rhs)) then 
    (false, Node (entailent ^ "  [Disprove]", []) )
  else if reoccur lhs rhs ctx then 
    (true, Node (entailent ^ "  [Reoccur]", []) )
  else 
    let fstSet = fst lhs in 
    if List.length fstSet == 0 then 
      (true, Node (entailent ^ "  [Prove]", []) )
    else 
      match (lhs, rhs) with 
      | (Disj (lhs1, lhs2), _) -> 
        let (result1, tree1) = inclusion lhs1 rhs ((lhs, rhs):: ctx) in 
        if not result1 then (result1, Node(entailent, [tree1])) 
        else 
          let (result2, tree2) = inclusion lhs2 rhs ((lhs1,rhs)::(lhs, rhs):: ctx) in 
          (result2, Node (entailent ^ "  [DisjL]",[tree1; tree2]))

      | (_, Disj (rhs1, rhs2)) -> 
        let (result1, tree1) = inclusion lhs rhs1 ((lhs, rhs):: ctx) in 
        if result1 then (result1, Node (entailent, [tree1])) 
        else 
          let (result2, tree2) = inclusion lhs rhs2 ((lhs, rhs):: ctx) in 
          (result2, Node (entailent ^ "  [DisjR]",[tree1; tree2]))
      | _ -> 
      let rec ietrater fList : (bool* binary_tree) = 
        match fList with 
        | [] -> assert false 
        | [f] -> 
          let derL = derivitives f lhs in 
          let derR = derivitives f rhs in 
          let (result, tree) = inclusion derL derR ((lhs, rhs):: ctx) in 
          (result, Node(entailent ^ "  [Unfold]" , [tree])) 
        | f :: restF -> 
          let derL = derivitives f lhs in 
          let derR = derivitives f rhs in 
          let (result, tree) = inclusion derL derR ((lhs, rhs):: ctx) in 
          (match result with 
          | true -> 
            let (resultRest, treeRest)  = ietrater restF in 
            (resultRest, Node (entailent ^ "  [DisjL]",[tree; treeRest]))
          | false -> (result, Node (entailent,[tree]))) 
      in ietrater fstSet 
    

(* the int is the currentposition, meaning from where the inclusion started to be wrong*)
type error_info = (pure * es * int * es)     
    
let getLineNumFromfstElem (f:fstElem) lowerbound = 
  match f with 
  | Event(_, Some i) -> i 
  | Event (_, None ) 
  | Wildcard 
  | NotEvent _ -> lowerbound


let modifiyTheProofOblgationCounters re = 
  let () = proofObligations := !proofObligations + 1 in 
  if List.length re > 0 then 
    failedProofObligations := !failedProofObligations +1 
  else ()

let modifiyTheassertionCounters () = 
  (*if List.length re > 0 then *)
  failedAssertions := !failedAssertions +1 
  (*else ()*)

let rec getFirstPostion es currentposition = 
  match es with 
  | Singleton (ins, Some fp) ->  fp
  | Concatenate (es1, _) -> getFirstPostion es1 currentposition
  | _ -> currentposition


  
let rec inclusion' 
  (pathcondition:pure)
  (currentposition:int)
  (lhs:es) 
  (rhs:es) 
  (ctx: (es*es) list) : ((error_info list) * binary_tree ) =

(*
print_endline (string_of_pure pathcondition);
*)
if entailConstrains pathcondition FALSE 
then ([], Node (string_of_pure pathcondition ^ "  [Prove]", []) )
else 


  let lhs = normalise_es lhs in 
  let rhs = normalise_es rhs in  
  let entailent = showEntailemnt lhs rhs in 
  (*print_endline (entailent); *)

  if isBot lhs then ([], Node (entailent ^ "  [False LHS]", []) )
  else if nullable lhs && (not (nullable rhs)) then 
    let currentposition = getFirstPostion lhs currentposition in 
    ([(pathcondition, lhs, currentposition ,rhs)], Node (entailent ^ "  [Nullable Disprove]", []) )

  else if reoccur lhs rhs ctx then 
    ([], Node (entailent ^ "  [Reoccur]", []) )
  else 
    let (fstSet: fstElem list) = fst lhs in 
    if List.length fstSet == 0 then 
      ([], Node (entailent ^ "  [Prove]", []) )
    else 
      match (lhs, rhs) with 
      | (Disj (lhs1, lhs2), _) -> 
        let (result1, tree1) = inclusion' pathcondition currentposition lhs1 rhs ((lhs, rhs):: ctx) in 
        if List.length result1 > 0 then (result1, Node(entailent, [tree1])) 
        else 
          let (result2, tree2) = inclusion' pathcondition currentposition lhs2 rhs ((lhs1,rhs)::(lhs, rhs):: ctx) in 
          (result2, Node (entailent ^ "  [DisjL]",[tree1; tree2]))

      | (_, Disj (rhs1, rhs2)) -> 
        let (result1, tree1) = inclusion' pathcondition currentposition lhs rhs1 ((lhs, rhs):: ctx) in 
        if List.length result1 == 0 then (result1, Node (entailent, [tree1])) 
        else 
          let (result2, tree2) = inclusion' pathcondition currentposition lhs rhs2 ((lhs, rhs):: ctx) in 
          if List.length result2 == 0 then (result2, Node (entailent, [tree2])) 
          else 
          (List.append result1 result2, Node (entailent ^ "  [DisjR]",[tree1; tree2]))
      | _ -> 
      let rec ietrater fList : ((error_info list)* binary_tree) = 
        match fList with 
        | [] -> assert false 
        | [f] -> 
          let derL = (derivitives f lhs) in 
          let derR = normalise_es (derivitives f rhs) in 
          if (isBot derR) then 
            let currentposition = if currentposition == (1000) then 
            ( (*print_string ("lalallalallal"^ string_of_int (getLineNumFromfstElem f)  ^ "\n"); *)
            (getLineNumFromfstElem f currentposition)) else currentposition in 
            ([(pathcondition, lhs, currentposition, rhs)], Node (entailent ^ "  [Disprove]", []) )
          else 
            let (result, tree) = inclusion' pathcondition (getLineNumFromfstElem f currentposition) derL derR ((lhs, rhs):: ctx) in 
            (result, Node(entailent ^ "  [Unfold]" , [tree])) 
        | f :: restF -> 
          let derL = (derivitives f lhs) in 
          let derR = normalise_es (derivitives f rhs) in 
          if (isBot derR) then 
            let currentposition = if currentposition == (1000) then 
            ((*print_string ("lalallalallal"^ string_of_int (getLineNumFromfstElem f)  ^ "\n");*)
            (getLineNumFromfstElem f currentposition)) else currentposition in 
            ([(pathcondition, lhs, currentposition, rhs)], Node (entailent ^ "  [Disprove]", []) )
          else 
          let (result, tree) = inclusion' pathcondition (getLineNumFromfstElem f currentposition) derL derR ((lhs, rhs):: ctx) in 
          (match result with 
          | [] -> 
            let (resultRest, treeRest)  = ietrater restF in 
            (resultRest, Node (entailent ^ "  [DisjL]",[tree; treeRest]))
          | _ -> (result, Node (entailent,[tree]))) 
      in ietrater fstSet 

let cartesian_product li1 li2 = 
    flattenList (List.map li1 ~f:(fun l1 -> 
      List.map li2 ~f:(fun l2 -> (l1, l2))))



let effect_inclusion (lhs:effect) (rhs:effect) : ((error_info list) * binary_tree) = 

  
  (*
  let listOflistofPairs = List.filter rhs 
    ~f:(fun (piR, _) -> 
        let pairs' = List.map lhs ~f:(fun (piL, esL)-> (piL, piR)) in 
        let pairs = List.filter pairs' ~f:(fun (piL, piR)->  not (entailConstrains piL piR)) in 
        if List.length pairs == 0 then true 
        else false  
       )
  in 
  print_string ("not matched specs:\n" ^ List.fold_left listOflistofPairs ~init:"" 
  ~f:(fun acc a -> acc ^ "\n " ^ string_of_effect [a]) );
  print_string ("\n------------\n");
  *)

  let mixLi = cartesian_product lhs rhs in 
  (* SYH: here is very important, as this is the under approximation of the specifiction *)
  let validPairs = List.map mixLi 
    ~f:(fun ((p1, a), (p2, c)) -> 
      let pathcondition = 
        if entailConstrains p1 p2 then TRUE else PureAnd(p1, p2) in 
    ((pathcondition (**), a), (p2, c))) in 

  let (functionStart, _) = !currentFunctionLineNumber in 

  
  let (f_re, f_tree) = (List.fold_left validPairs ~init:([], []) ~f:(
    fun (accre, acctree) ((p1, es1), (p2, es2)) ->
    let (re, tree) = inclusion' p2 functionStart es1 es2 [] in 
    modifiyTheProofOblgationCounters re; 

    (List.append accre re, List.append acctree [(Node ((string_of_pure p1 ^ "|-" ^ string_of_pure p2) , [tree]))])
    )) in 
    (f_re, Node ("TRS:", f_tree))

type pathList = (int list) list

(* returns error_information, and correctTraces and errorTraces *)
let effectwithfootprintInclusion (lhs: effectwithfootprint list) (rhs:effect) : 
((error_info list) * binary_tree * pathList * pathList) = 
  
  (*print_endline (string_of_effect rhs);

  print_endline (string_of_int (List.length lhs) ^ " " ^ string_of_int (List.length rhs));
*)
  let mixLi = cartesian_product lhs rhs in 

  (*print_endline ("mixLi length:" ^ string_of_int (List.length mixLi));
*)
  (* SYH: here is very important, as this is the under approximation of the specifiction *)
  let validPairs = List.map mixLi 
    ~f:(fun ((p1, a, b), (p2, c)) -> 
      let pathcondition = 
        if entailConstrains p1 p2 then p2 else 
        PureAnd(p1, p2) in 
    ((pathcondition, a, b), (p2, c))) in 
    
    (*List.filter mixLi ~f:(fun ((p1, _, _), (p2, _)) -> entailConstrains p1 p2) in *)


(*
  print_endline ("validPairs length:" ^ string_of_int (List.length validPairs));
*)
  let (functionStart, _) = !currentFunctionLineNumber in 

  let (f_re, f_tree, correctT, errorT) = 
  (List.fold_left validPairs ~init:([], [], [], []) ~f:(
    fun (accre, acctree, correctTrace, errorTrace) ((p1, es1, li), (p2, es2)) ->

    let (re, tree) = inclusion' p1 functionStart es1 es2 [] in   
    modifiyTheProofOblgationCounters re; 
   (* modifiyTheassertionCounters re; *)
    let (correctTrace', errorTrace') = 
      if List.length re == 0 
      then (List.append correctTrace [li], errorTrace)
      else (correctTrace, List.append errorTrace [li])
    in 
    (List.append accre re, List.append acctree [(Node ((string_of_pure p1 ^ "|-" ^ string_of_pure p2) , [tree]))], 
    correctTrace', errorTrace'
    )
    )) in 
    (f_re, Node ("TRS:", f_tree), correctT, errorT)


let rec reversees (eff:es) : es = 
  match eff with 
  | Bot             
  | Emp           
  | Any 
  | Singleton _   
  | NotSingleton _        -> eff 
  | Concatenate (eff1, eff2) ->
    Concatenate (reversees eff2, reversees eff1)
  | Disj (eff1, eff2) ->
    Disj (reversees eff1, reversees eff2)
  | Kleene effIn          ->Kleene (reversees effIn)
  | _ -> eff


let bugLocalisation (paths: error_info list): (pure * es * (int * int) * es) list = 
  let rec helper li =
    match li with 
    | [] -> []
    | (pathcondition, lhs, start, rhs):: rest -> 
      let revlhs = reversees lhs in 
      let revrhs = reversees rhs in 
      let (_, functionEnd) = !currentFunctionLineNumber in 

      let (result, tree) = inclusion' pathcondition functionEnd revlhs revrhs [] in 
      modifiyTheProofOblgationCounters result; 

      (*
      print_string (showEntailemnt revlhs revrhs ^ " " ^ string_of_int (List.length result)^"\n ------- \n");
*)
      let temp = List.map result ~f:(fun (pathcondition, a, n, b)-> 
(*      print_string (showEntailemnt (reversees a) (reversees b) ^ "\n ------- \n");
*)
        (*print_endline ("startpoint:" ^ (string_of_int start) ^ " endpoint:" ^ (string_of_int n)); *)
        (pathcondition, reversees a, (start, n), reversees b)) in 

      
      List.append temp (helper rest)
  in helper paths



let getNumberFromfstElem (f:fstElem): int option = 
  match f with 
  | Event (_, n) -> n  
  | Wildcard 
  | NotEvent _ ->  None 


(*let retriveLines (eff:es) : (int*int) = 
  let fstSetOrigin = fst eff in 
  let startNum = List.fold_left ~init:0 ~f:(fun acc a -> 
    match getNumberFromfstElem a with 
    | None -> acc 
    | Some a ->  if acc == 0 then a else if a < acc then a else acc) fstSetOrigin in 
  let fstSetReversed = fst (reversees eff) in 
  let endNum = List.fold_left ~init:0 ~f:(fun acc a ->
    match getNumberFromfstElem a with 
    | None -> acc 
    | Some a ->  if a > acc then a else acc) fstSetReversed in 
  (startNum, endNum)
  *)

let rec existEff_withfootprint acc (pi, es, ft, li) : bool = 
  match acc with 
  | [] -> false 
  | (pi1, es1, ft1, _) :: xs -> 
  if comparePure pi1 pi && comparees es1 es && ft == ft1
  then true 
  else existEff_withfootprint xs (pi, es, ft, li) 

let normaliseProgramStates (li:programStates) : programStates =
  let temp = List.fold_left li ~init:[] ~f:(fun acc (p, a, ft, li) -> 
    let p = normalPure p in 
    if entailConstrains p FALSE then acc
    else List.append acc [(p, normalise_es a, ft, li)]) in 

  let rec helper effList = 
    match effList with 
    | [] -> []
    | x :: xs  -> 
      if existEff_withfootprint xs x then helper xs else x :: helper xs
    
  in helper temp

let rec findRetFromBindings (bt:bindings) (str: string) : basic_type option =
  match bt with
  | [] -> None 
  | (formal, artual) :: xs -> 
    if String.compare formal str == 0 then Some artual else findRetFromBindings xs str
  


let rec findRetFromBindingsRet (bt:bindings) : string option =
  match bt with
  | [] -> None 
  | (formal, BRET) :: xs -> Some formal 
  | x :: xs -> findRetFromBindingsRet xs 



let instantiateAugument_Basic_type (bt:basic_type) (bds:bindings):  basic_type = 
  match bt with 
  | BVAR str -> 
    (match findRetFromBindings bds (getRoot str) with
    | None -> bt
    | Some term ->  term 
    )
  | _ -> bt 

let instantiateRet_Basic_type (bt:basic_type) (str:string):  basic_type = 
  match bt with 
  | BRET -> BVAR str
  | _ -> bt 
  
let rec instantiateAugument_Term (t:terms) (bds:bindings): terms = 
  match t with
  | Basic (bt ) -> Basic (instantiateAugument_Basic_type bt bds)
  | Plus (t1, t2) -> Plus (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
  | Minus (t1, t2) -> Minus (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
let rec instantiateRet_Term (t:terms) (bds:string): terms = 
  match t with
  | Basic (bt ) -> Basic (instantiateRet_Basic_type bt bds)
  | Plus (t1, t2) -> Plus (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)
  | Minus (t1, t2) -> Minus (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)


let rec instantiateAugumentPure (p:pure) (bds:bindings): pure = 
  match p with
    TRUE 
  | FALSE -> p
  | Gt (t1, t2) -> Gt (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
  | Lt (t1, t2) -> Lt (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
  | GtEq (t1, t2) -> GtEq (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
  | LtEq (t1, t2) -> LtEq (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
  | Eq (t1, t2) -> Eq (instantiateAugument_Term t1 bds, instantiateAugument_Term t2 bds)
  | PureOr (p1, p2) -> PureOr (instantiateAugumentPure p1 bds, instantiateAugumentPure p2 bds)
  | PureAnd (p1, p2) -> PureAnd (instantiateAugumentPure p1 bds, instantiateAugumentPure p2 bds)
  | Neg p1 -> Neg (instantiateAugumentPure p1 bds)


let rec instantiateRetPure (p:pure) (bds:string): pure = 
  match p with
    TRUE 
  | FALSE -> p
  | Gt (t1, t2) -> Gt (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)
  | Lt (t1, t2) -> Lt (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)
  | GtEq (t1, t2) -> GtEq (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)
  | LtEq (t1, t2) -> LtEq (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)
  | Eq (t1, t2) -> Eq (instantiateRet_Term t1 bds, instantiateRet_Term t2 bds)
  | PureOr (p1, p2) -> PureOr (instantiateRetPure p1 bds, instantiateRetPure p2 bds)
  | PureAnd (p1, p2) -> PureAnd (instantiateRetPure p1 bds, instantiateRetPure p2 bds)
  | Neg p1 -> Neg (instantiateRetPure p1 bds)


  


let rec instantiateAugumentEs (es:es) (bds:bindings): es =
  match es with   
  | Bot | Emp | Any -> es
  | NotSingleton (str, btList) -> 
    (*let () = print_string ("instantiateAugumentEs: " ^ string_of_es es^ "\n") in *)
    let newbyList = List.map btList ~f:(fun bt -> instantiateAugument_Basic_type bt bds) in 
    let newes = NotSingleton (str, newbyList) in 
    (*let () = print_string ("instantiateAugumentEs after : " ^ string_of_es newes^ "\n") in *)
    newes
  | Singleton ((str, btList), l) ->  
    (*let () = print_string ("instantiateAugumentEs: " ^ string_of_es es^ "\n") in *)
    let newbyList = List.map btList ~f:(fun bt -> instantiateAugument_Basic_type bt bds) in 
    let newes = Singleton ((str, newbyList), l) in 
    (*let () = print_string ("instantiateAugumentEs after : " ^ string_of_es newes^ "\n") in *)
    newes
    
  | Disj(es1, es2) -> Disj(instantiateAugumentEs es1 bds, instantiateAugumentEs es2 bds)
  | Concatenate (es1, es2) -> Concatenate(instantiateAugumentEs es1 bds, instantiateAugumentEs es2 bds)
  | Kleene es1 -> Kleene (instantiateAugumentEs es1 bds)
  | _ -> es 

let rec instantiateRetEs (es:es) (bds:string): es =
  match es with   
  | Bot | Emp | Any -> es
  | NotSingleton (str, btList) -> 
    let newbyList = List.map btList ~f:(fun bt -> instantiateRet_Basic_type bt bds) in 
    let newes = NotSingleton (str, newbyList) in 
    newes
  | Singleton ((str, btList), l) ->  
    let newbyList = List.map btList ~f:(fun bt -> instantiateRet_Basic_type bt bds) in 
    let newes = Singleton ((str, newbyList), l) in 
    newes
    
  | Disj(es1, es2) -> Disj(instantiateRetEs es1 bds, instantiateRetEs es2 bds)
  | Concatenate (es1, es2) -> Concatenate(instantiateRetEs es1 bds, instantiateRetEs es2 bds)
  | Kleene es1 -> Kleene (instantiateRetEs es1 bds)
  | _ -> es 


let instantiateAugument (eff:effect option) (bds:bindings) : effect option = 
  match eff with 
  | None -> None 
  | Some eff -> 
    (*let () = print_string ("instantiateRet: " ^ string_of_effect eff^ "\n") in *)
    let temp = List.map eff ~f:(fun (pi, es) -> (instantiateAugumentPure pi bds, instantiateAugumentEs es bds)) in 
    (*let () = print_string ("instantiateRet after : " ^ string_of_effect temp^ "\n") in *)
     Some (temp)

let instantiateReturn (eff:effect option) (str:string) : effect option = 
  match eff with 
  | None -> None 
  | Some eff -> 
    (*let () = print_string ("instantiateRet: " ^ string_of_effect eff^ "\n") in *)
    let temp = List.map eff ~f:(fun (pi, es) -> (instantiateRetPure pi str, instantiateRetEs es str)) in 
    (*let () = print_string ("instantiateRet after : " ^ string_of_effect temp^ "\n") in *)
     Some (temp)

let instantiateAugumentSome (eff:effect) (bds:bindings) : effect  = 
  List.map eff ~f:(fun (pi, es) -> (instantiateAugumentPure pi bds, instantiateAugumentEs es bds)) 


let instantiateRetSome (eff:effect) (bds:string) : effect  = 
  List.map eff ~f:(fun (pi, es) -> (instantiateRetPure pi bds, instantiateRetEs es bds)) 

let instantiateProgramStates (eff:programStates) (bds:bindings) : programStates  = 
  List.map eff ~f:(fun (pi, es, a, b) -> (instantiateAugumentPure pi bds, instantiateAugumentEs es bds, a, b)) 

let rec findReturnValueES es : string option =
    match es with 
    | Singleton ((str, [((BVAR arg))]), _) -> 
      if String.compare str "RET" == 0 then Some arg else None 
    | Singleton _ -> None 
    | Concatenate (a, b) -> findReturnValueES b 
    | Bot | Emp | Any | NotArguments _
    | NotSingleton _ -> None 
    | Disj (a, b) -> 
      (match findReturnValueES a with
      |None -> findReturnValueES b
      |Some str -> Some str)
    | Kleene (a) -> findReturnValueES a

let rec findReturnValueProgramStates (eff:programStates) : string option =
 
  match eff with
  | [] -> None 
  | (_, es, _, _):: xs -> 
    (match findReturnValueES es with 
    | None -> findReturnValueProgramStates xs 
    | Some str -> Some str )


let enforeceLineNum (fp:int list) (eff:effect option) : effect option = 
  match eff with 
  | None -> None 
  | Some eff -> 
  match fp with 
  | [] -> 
    (*print_string (" enforeceLineNum NOne \n");*)
    Some eff 
  | x::_ -> 
        (*print_string (" enforeceLineNum " ^ string_of_int x ^" \n");
*)
    let rec helper es = 
      match es with 
      | Bot | Emp | Any 
      | NotSingleton _
      -> es 
      | Singleton (ins, _) ->  Singleton (ins, Some x)
      | Disj(es1, es2) -> Disj(helper es1, helper es2)
      | Concatenate (es1, es2) -> Concatenate(helper es1, helper es2)
      | Kleene es1 -> Kleene (helper es1)
      | _ -> es
    in 
    Some (List.map eff ~f:(fun (p, es) -> (p, helper es)))



let effectwithfootprint2Effect eff = 
  List.map eff ~f:(fun (a, b, _) -> (a, b)) 

let string_of_inclusion_results (extra_info: string) (info:((error_info list) * binary_tree * pathList * pathList)) : string = 
  let (error_paths, tree, correctTraces, errorTraces) = info in 
  if List.length error_paths == 0 then "" (*^
    "Inclusion Succeed!\n" ^  string_of_binary_tree tree  *)
  else 
    extra_info^ 
    "Failed!\n" ^  string_of_binary_tree tree   

let string_of_function_sepc (pre, post, future) : string = 
  let pre = match pre with 
    | None -> "No Pre"
    | Some eff -> string_of_effect eff 
  in 
  let post = match post with 
    | None -> "No Post"
    | Some eff -> string_of_effect eff 
  in 
  let future = match future with 
    | None -> "No Future"
    | Some eff -> string_of_effect eff 
  in pre ^ "\n" ^ post ^ "\n" ^ future ^ "\n"


let string_of_foot_print (fp:int list ) : string = 
  match fp with 
  | [] -> ""
  | x :: _-> " @" ^ string_of_int x



let existRetTerm_basic_t v =
  match v with
  | BRET -> true 
  | _ -> false 
let rec existRetTerm t = 
  match t with
  | Basic v -> existRetTerm_basic_t v 
  | Plus (t1, t2) 
  | Minus (t1, t2) -> existRetTerm t1 ||  existRetTerm t2


let rec existRetPure pi = 
  match pi with
    TRUE -> false 
  | FALSE -> false
  | Gt (t1, t2) 
  | Lt (t1, t2) 
  | GtEq (t1, t2) 
  | LtEq (t1, t2) 
  | Eq (t1, t2) -> existRetTerm t1 ||  existRetTerm t2
  | PureOr (p1, p2) 
  | PureAnd (p1, p2) -> existRetPure p1 ||  existRetPure p2
  | Neg p -> existRetPure p



let existRetEff (eff: effect option) : bool = 
  let aux (pi, es) = if existRetPure pi then true else false (*existRetEs es *)
  in 
  let rec helper li  = 
    match li with 
    | [] -> false 
    | (pi, es) :: xs  -> if aux (pi, es) then true else helper xs
  in 
  match eff with 
  | None -> false 
  | Some effIn -> helper effIn

let string_of_basic_t_prime v = 
  match v with 
  | BVAR name -> name
  | BINT n -> string_of_int n
  | BNULL -> "NULL"
  | BRET -> "ret"

let rec string_of_terms_prime (t:terms):string = 
  match t with
  | Basic v -> string_of_basic_t_prime v 
  | Plus (t1, t2) -> (string_of_terms_prime t1) ^ ("_plus_") ^ (string_of_terms_prime t2)
  | Minus (t1, t2) -> (string_of_terms_prime t1) ^ ("_minus_") ^ (string_of_terms_prime t2)

let rec string_of_pure_prime (p:pure):string =   
  match p with
    TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | Gt (t1, t2) -> "("^(string_of_terms_prime t1) ^ ">" ^ (string_of_terms_prime t2)^")"
  | Lt (t1, t2) -> "("^(string_of_terms_prime t1) ^ "<" ^ (string_of_terms_prime t2)^")"
  | GtEq (t1, t2) -> "("^(string_of_terms_prime t1) ^ ">=" ^ (string_of_terms_prime t2)^")"
  | LtEq (t1, t2) -> "("^(string_of_terms_prime t1) ^ "<=" ^ (string_of_terms_prime t2)^")"
  | Eq (t1, t2) -> "("^(string_of_terms_prime t1) ^ "=" ^ (string_of_terms_prime t2)^")"
  | PureOr (p1, p2) -> "("^string_of_pure_prime p1 ^ "\\/" ^ string_of_pure_prime p2^")"
  | PureAnd (p1, p2) -> "("^string_of_pure_prime p1 ^ "/\\" ^ string_of_pure_prime p2^")"
  | Neg (Eq (t1, t2)) -> "!("^(string_of_terms_prime t1) ^ "=" ^ (string_of_terms_prime t2)^")"
  | Neg p -> "!(" ^ string_of_pure_prime p^")"

let string_of_event_prime (str, li) = 
  let temp = 
    match li with 
    | [] -> ""
    | [x] ->  string_of_basic_t_prime x 
    | x::xs->
      List.fold_left xs 
      ~init:(string_of_basic_t_prime x) 
      ~f:(fun acc a -> acc ^ "," ^ string_of_basic_t_prime a )
  in 
  str ^ "("^temp^")"
let rec string_of_es_prime (eff:es) : string = 
  match eff with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐"
  | Any -> "_" 
  | Singleton (str, l)  -> 
    string_of_event_prime str (*^ (match l with | None -> "" | Some i -> "@"^ string_of_int i)*)
  (* | NotArguments (x::_) -> -> "!_" ^ string_of_basic_t x  
  currently NotArguments is represented using NotSingleton _
  *)
  | NotSingleton str          -> "!" ^ string_of_event_prime str 
  | Concatenate (eff1, eff2) ->
      string_of_es_prime eff1 ^ " · " ^ string_of_es_prime eff2 
  | Disj (eff1, eff2) ->
      "(" ^ string_of_es_prime eff1 ^ " \\/ " ^ string_of_es_prime eff2 ^ ")"
  | Kleene effIn          ->
      "(" ^ string_of_es_prime effIn ^ ")^*"

  | _ -> "string_of_es error"

let rec string_of_effect_prime (eff:effect) : string = 
  match eff with 
  | [] -> ""
  | [(pi, es)] ->  "(" ^ string_of_pure_prime pi ^ ", " ^ string_of_es_prime es ^ ")"
  | (pi, es) :: xs ->  "(" ^ string_of_pure_prime pi ^ ", " ^ string_of_es_prime es ^ ") \\/ " ^ string_of_effect_prime xs

  
let string_of_specification (((name, args),  pre, post, future):specification)  : string 
  = 
    let helper number eff = 
      match eff with
      | None -> ""
      | Some eff -> 
        "    "^ (if number == 0 then "Pre "
        else if number == 1 then "Post "
        else "Future ")
        ^ string_of_effect_prime eff
    in 
    let rec print_arg li  = 
      match li with 
      | [] -> ""
      | [x] -> x
      | x :: xs  -> x ^ ", " ^ print_arg xs 
    in 
    let pre_str = (helper 0 pre)  in 
    let post_str = helper 1 post  in 
    let future_str = helper 2 future  in 
    "/*@ "^  name ^ "("^ print_arg args ^"):\n"  
    ^ pre_str
    ^ (if String.compare future_str "" == 0 
      then post_str else (if String.compare post_str "" == 0 then "" else post_str ^ "\n"))
    ^ future_str
    ^  "@*/\n"

let rec seperateDisjunctivesES (es:es) : es list =
  match es with 
  | Disj(es1, es2) -> List.append (seperateDisjunctivesES es1) (seperateDisjunctivesES es2)
  | _ -> [es]


let rec seperateDisjunctives (info:((error_info list) * binary_tree * pathList * pathList)): ((error_info list) * binary_tree * pathList * pathList) list = 
  match  info with 
  | (error_paths, a, b, c) -> 
    match error_paths with 
    | [] -> [] 
    | (pi, es1, d, es2) :: xs -> 
      let es1_s = seperateDisjunctivesES es1 in 
      List.map es1_s ~f:(fun es -> ([(pi, es, d, es2)], a, b, c)) 
      @ seperateDisjunctives (xs, a, b, c)


let rec eliminateAllTheRetturnPure pi =
  match pi with 
  | Gt (Basic(BRET), _) 
  | Lt (Basic(BRET), _) 
  | GtEq (Basic(BRET), _) 
  | LtEq (Basic(BRET), _) 
  | Eq (Basic(BRET), _) 
  | Gt (_,Basic(BRET)) 
  | Lt (_,Basic(BRET)) 
  | GtEq (_,Basic(BRET)) 
  | LtEq (_,Basic(BRET)) 
  | Eq (_, Basic(BRET)) -> TRUE
    
  | PureAnd (pi1,pi2) -> PureAnd (eliminateAllTheRetturnPure pi1, eliminateAllTheRetturnPure pi2)
  | Neg piN -> Neg (eliminateAllTheRetturnPure piN)
  | PureOr (pi1,pi2) -> PureOr (eliminateAllTheRetturnPure pi1, eliminateAllTheRetturnPure pi2)
  | _ -> pi

let rec eliminateAllTheRetturnES eff : es  = 
  match eff with
  | Disj(es1, es2) -> 
    let es1 = eliminateAllTheRetturnES es1 in 
    let es2 = eliminateAllTheRetturnES es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Bot, es) -> eliminateAllTheRetturnES es 
    | (es, Bot) -> eliminateAllTheRetturnES es 
    | (Disj (es11, es12), es3) -> Disj (es11, Disj (es12, es3))
    | _ -> (Disj (es1, es2))
    )
  | Singleton ((str, argList), _) -> 
    if String.compare str "RET" == 0 then Emp 
    else if twoStringSetOverlap (List.map argList ~f:(fun a ->getRoot ( string_of_basic_t a))) (!parametersInScope) then eff
    else Emp
    
  | Concatenate (es1, es2) -> 
    let es1 = eliminateAllTheRetturnES es1 in 
    let es2 = eliminateAllTheRetturnES es2 in 
    (match (es1, es2) with 
    | (Emp, _) -> eliminateAllTheRetturnES es2
    | (_, Emp) -> eliminateAllTheRetturnES es1
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Disj (es11, es12), es3) -> Disj(Concatenate (es11,es3),  Concatenate (es12, es3))
    | (Concatenate (es11, es12), es3) -> (Concatenate (es11, Concatenate (es12, es3)))
    | _ -> (Concatenate (es1, es2))
    )
  | Kleene effIn -> 
    let effIn' = eliminateAllTheRetturnES effIn in 
    (match effIn' with 
    | Emp -> Emp 
    | _ ->  
    Kleene (effIn'))
  | _ -> eff 


let rec eliminateAllTheRetturn eff : effect = 
  match eff with 
  | [] ->  []
  | (pi, es):: xs -> 
    let (pi', es') = (eliminateAllTheRetturnPure pi, eliminateAllTheRetturnES es) in 
    match (pi', es') with 
    | (TRUE, Emp) -> eliminateAllTheRetturn xs
    | _ -> (pi', es') :: eliminateAllTheRetturn xs

let rec returningNULLES es : bool = 
  match es with 
  | Singleton ((_, [((BNULL))]), _) -> true 
  | Singleton _ -> false 
  | Concatenate (a, b) -> returningNULLES b 
  | Bot | Emp | Any | NotArguments _
  | NotSingleton _ -> false 
  | Disj (a, b) -> 
    (match returningNULLES a with
    |false -> returningNULLES b
    |true -> true)
  | Kleene (a) -> returningNULLES a


let rec returningNULL eff : bool = 
  match eff with 
  | [] -> false  
  | (_, es) :: xs -> if returningNULLES es then true else returningNULL xs 


let isNotProjectFile source_Address source_Addressnow  = 
  String.compare source_Address source_Addressnow != 0
  (*let source_Address = String.sub source_Address 0 10 in 
  let source_Addressnow = String.sub source_Addressnow 0 10 in 
  if String.compare source_Address source_Addressnow == 0 then false else true*)


let mergeSpec eff1 eff2 = 
  match (eff1, eff2) with 
  | (None, None) -> None 
  | (Some a, None) 
  | (None, Some a) -> 
    (match a with 
    | [] -> Some []
    | e1::_-> Some [e1]
    )
    
  | (Some a, Some b) -> 
    (match a with 
    | [] -> Some []
    | e1::_-> Some [e1]
    )
    (*if String.compare (string_of_effect_prime a) (string_of_effect_prime b) == 0 then Some a 
    else if List.length a >= 5 then Some a 
    else Some (a@b) *)


let deepSimplifyEffect ((pi, es1):(pure * es)): (pure * es) = 
(*print_endline ("deepSimplifyEffect");
print_endline (string_of_es es1);*)
  let (vacabulary: (string list) ref) = ref [] in 
  let rec aux es: es = 
    match es with 
    | Bot | Emp | Any -> es 
    | NotArguments _
    | NotSingleton _
    | Singleton _ -> 
      let sign = string_of_es_prime es in 
      if twoStringSetOverlap [sign] !vacabulary then Emp 
      else (let () = vacabulary := !vacabulary @[sign] in es)
    | Disj (esIn1, esIn2) -> Disj (aux esIn1, aux esIn2)
    | Concatenate  (esIn1, esIn2) -> Concatenate (aux esIn1, aux esIn2)
    | Kleene  (esIn1) -> Kleene (aux esIn1)
  in 
  let es' = normalise_es (aux es1) in 
  (*print_endline ("after deepSimplifyEffect");
  print_endline (string_of_es es');*)
  (normalPure pi, es')

let isNotFalse pi = 
  match pi with 
  | FALSE -> true 
  | _ ->  false 


let compactDisjunctions (state:programStates): programStates = 
  let rec mergeIntoAcc (pi, es, a, b) (acc:programStates) :programStates = 
    match acc with 
    | [] -> [(pi, es, a, b)]
    | (piacc, esacc, aacc, bacc)::rest -> 
      if comparePure pi piacc && aacc == a then (piacc, normalise_es(Disj(esacc, es)), aacc, bacc)::rest
      else (piacc, esacc, aacc, bacc)::(mergeIntoAcc (pi, es, a, b) rest)

  in 
  let rec helper acc li  = 
    match li with 
    | [] -> acc 
    | eff::rest -> 
      let acc' = mergeIntoAcc eff acc in 
      helper acc' rest 
  in  helper [] state
      




let creatingDisjunctiveProgramStates (state1:programStates) (state2:programStates) :programStates = 
  print_endline ("----------\nCreatingDisjunctiveProgramStates");
  print_endline (string_of_programStates state1);
  print_endline (string_of_programStates state2);
  (*
  match (state1, state2) with 
  | ([(pi1, es1, a, b)], [(pi2, es2, _, _)]) -> 
    if comparePure pi1 pi2 
      then if  comparees es1 es2 then state1
      else [(pi1, Disj(es1, es2), a, b)]
    else 
      List.append state1 state2

  | _ -> 
  *)
  let temp =   compactDisjunctions (List.append state1 state2) in 
  print_endline ("result: " ^ string_of_programStates temp);
  temp

