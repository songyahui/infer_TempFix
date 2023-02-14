open Z3

type ltl = Lable of string 
        | Next of ltl
        | Until of ltl * ltl
        | Global of ltl
        | Future of ltl
        | NotLTL of ltl
        | Imply of ltl * ltl
        | AndLTL of ltl * ltl
        | OrLTL of ltl * ltl

type line_number = int option

type terms = Var of string
           | Number of int
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
              | Singleton of (string * line_number) 
              | NotSingleton of string 
              | Disj of es * es 
              | Concatenate of es * es 
              | Kleene of es 


type effect = (pure * es) list 


type specification = (string * effect * effect)

type fstElem = Wildcard | Event of (string * line_number)  | NotEvent of string


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

let rec showTerms (t:terms):string = 
  match t with
    Var name -> name
  | Number n -> string_of_int n
  | Plus (t1, t2) -> (showTerms t1) ^ ("+") ^ (showTerms t2)
  | Minus (t1, t2) -> (showTerms t1) ^ ("-") ^ (showTerms t2)

let rec showPure (p:pure):string =   
  match p with
    TRUE -> "⊤"
  | FALSE -> "⊥"
  | Gt (t1, t2) -> (showTerms t1) ^ ">" ^ (showTerms t2)
  | Lt (t1, t2) -> (showTerms t1) ^ "<" ^ (showTerms t2)
  | GtEq (t1, t2) -> (showTerms t1) ^ "≥" ^ (showTerms t2)
  | LtEq (t1, t2) -> (showTerms t1) ^ "≤" ^ (showTerms t2)
  | Eq (t1, t2) -> (showTerms t1) ^ "=" ^ (showTerms t2)
  | PureOr (p1, p2) -> "("^showPure p1 ^ "∨" ^ showPure p2^")"
  | PureAnd (p1, p2) -> showPure p1 ^ "∧" ^ showPure p2
  | Neg (Eq (t1, t2)) -> "(~("^(showTerms t1) ^ "=" ^ (showTerms t2)^"))"
  | Neg p -> "(~" ^ showPure p^")"
  
(**********************************************)
exception FooAskz3 of string

let rec convertTerm (t:terms):string = 
  match t with
    Var name -> " " ^ name ^ " "
  | Number n -> " " ^ string_of_int n ^ " "
  | Plus (t1, t2) -> ("(+") ^ (convertTerm t1) ^  (convertTerm t2) ^ ")"
  | Minus (t1, t2) -> ("(-") ^ (convertTerm t1) ^  (convertTerm t2) ^ ")"
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

let rec exist li ele = 
  match li with 
    [] -> false 
  | x :: xs -> if (String.compare x ele) == 0 then true else exist xs ele
  ;;

let rec checkRedundant (li:string list) : string list = 
  match li with
    [] -> []
  | x ::xs -> if (exist xs x) == true then checkRedundant xs else List.append [x] (checkRedundant xs)

;;


let rec getAllVarFromTerm (t:terms) (acc:string list):string list = 
  match t with
  Var name -> List.append acc [name]
| Plus (t1, t2) -> 
    let cur = getAllVarFromTerm t1 acc in 
    getAllVarFromTerm t2 cur
| Minus (t1, t2) -> 
    let cur = getAllVarFromTerm t1 acc in 
    getAllVarFromTerm t2 cur
| _ -> acc
;;

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
    if String.compare x (showPure pi) == 0 then Some b 
    else existInhistoryTable pi  xs




let rec term_to_expr ctx : terms -> Z3.Expr.expr = function
  | Number n        -> Z3.Arithmetic.Real.mk_numeral_i ctx n
  | Var v          -> Z3.Arithmetic.Real.mk_const_s ctx v
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
  let ()= historyTable := (showPure pi, re)::!historyTable in 
  
  re;;


let entailConstrains pi1 pi2 = 

  let sat = not (askZ3 (Neg (PureOr (Neg pi1, pi2)))) in
  (*
  print_string (showPure pi1 ^" -> " ^ showPure pi2 ^" == ");
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

let rec string_of_es (eff:es) : string = 
  match eff with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐"
  | Any -> "_" 
  | Singleton (str, l)  -> 
    str ^ (match l with | None -> "" | Some i -> "@"^ string_of_int i)
  | NotSingleton str          -> "!" ^ str 
  | Concatenate (eff1, eff2) ->
      string_of_es eff1 ^ " · " ^ string_of_es eff2 
  | Disj (eff1, eff2) ->
      "(" ^ string_of_es eff1 ^ " \\/ " ^ string_of_es eff2 ^ ")"
  | Kleene effIn          ->
      "(" ^ string_of_es effIn ^ ")^*"
      
let string_of_effect (eff:effect) : string = 
  List.fold_left eff ~init:"" ~f:(fun acc (pi, es) -> 
    acc ^ ", " ^ showPure pi ^ " /\\ " ^ string_of_es es
  )

let rec normalise_es (eff:es) : es = 
  match eff with 
  | Disj(es1, es2) -> 
    let es1 = normalise_es es1 in 
    let es2 = normalise_es es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Bot, es) -> normalise_es es 
    | (es, Bot) -> normalise_es es 
    | (Disj (es11, es12), es3) -> Disj (es11, Disj (es12, es3))
    | _ -> (Disj (es1, es2))
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



let rec isBot (eff:es) : bool =
  match eff with 
  | Bot -> true 
  | _ -> false 

let normalise_effect (eff:effect) : effect = 
  let temp = List.map eff ~f:(fun (pi, es) -> (pi, normalise_es es)) in 
  List.filter temp ~f:(fun (pi, es) -> not (isBot es)) 



let rec nullable (eff:es) : bool = 
  match eff with 
  | Bot              -> false 
  | Emp              -> true 
  | Any 
  | Singleton _          -> false 
  | NotSingleton str          -> false
  | Concatenate (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Disj (eff1, eff2) -> nullable eff1 || nullable eff2  
  | Kleene effIn      -> true

let rec fst (eff:es) : (fstElem list) = 
  match eff with 
  | Bot                
  | Emp             -> []
  | Any             -> [ Wildcard ]  
  | Singleton s   -> [(Event s)] 
  | NotSingleton str          -> [(NotEvent str)] 
  | Concatenate (eff1, eff2) -> 
    if nullable eff1 then List.append (fst eff1) (fst eff2)
    else (fst eff1)
  | Disj (eff1, eff2) -> List.append (fst eff1) (fst eff2)
  | Kleene effIn      -> (fst effIn) 

let rec derivitives (f:fstElem) (eff:es) : es = 
  match eff with 
  | Bot        
  | Emp   -> Bot                
  | Any   -> Emp
  | Singleton (str, _) -> 
    (match f with 
    | Wildcard _ -> Bot 
    | Event (event, _) -> if String.compare str event == 0 then Emp else Bot 
    | NotEvent event  ->  Bot
    )
  | NotSingleton str -> 
    (match f with 
    | Wildcard _ -> Bot 
    | Event (event, _) -> if String.compare str event == 0 then Bot else Emp
    | NotEvent event  ->  if String.compare str event == 0 then Emp else Bot
    )
  | Concatenate (eff1, eff2) -> 
    if nullable eff1 then 
      Disj (
        Concatenate (derivitives f eff1, eff2), 
                     derivitives f eff2)
    else Concatenate (derivitives f eff1, eff2)
  | Disj (eff1, eff2) -> Disj (derivitives f eff1, derivitives f eff2)
  | Kleene effIn      -> Concatenate (derivitives f effIn, eff)


let showEntailemnt (lhs:es) (rhs:es) : string =
  string_of_es lhs  ^" |- "^ string_of_es rhs ;;

let rec comparees (eff1:es) (eff2:es): bool =
  match (eff1, eff2) with 
  | (Bot, Bot) 
  | (Any, Any) 
  | (Emp, Emp) -> true 
  | (Singleton (s1, _), Singleton (s2, _)) -> 
    if String.compare s1 s2 == 0  then true else false 
  | (NotSingleton s1, NotSingleton s2) -> 
    if String.compare s1 s2 == 0  then true else false 
  | (Concatenate (a1, a2), Concatenate(a3, a4)) 
  | (Disj (a1, a2), Disj(a3, a4)) -> 
    comparees a1 a3 && comparees a2 a4
  | (Kleene e1, Kleene e2) -> comparees e1 e2
  | _ -> false 
  ;;
  

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
    

type error_info = (es * int * es)     
    
let getLineNumFromfstElem (f:fstElem) = 
  match f with 
  | Event(_, Some i) -> i 
  | Event (_, None ) 
  | Wildcard 
  | NotEvent _ -> -1

  
let rec inclusion' 
  (currentposition:int)
  (lhs:es) 
  (rhs:es) 
  (ctx: (es*es) list) : ((error_info list) * binary_tree ) =

  let lhs = normalise_es lhs in 
  let rhs = normalise_es rhs in  
  let entailent = showEntailemnt lhs rhs in 
  (*print_string (entailent ^ "\n");*)
  if isBot lhs then ([], Node (entailent ^ "  [False LHS]", []) )
  else if nullable lhs && (not (nullable rhs)) then 
  ([(lhs, currentposition ,rhs)], Node (entailent ^ "  [Disprove]", []) )

  else if reoccur lhs rhs ctx then 
    ([], Node (entailent ^ "  [Reoccur]", []) )
  else 
    let (fstSet: fstElem list) = fst lhs in 
    if List.length fstSet == 0 then 
      ([], Node (entailent ^ "  [Prove]", []) )
    else 
      match (lhs, rhs) with 
      | (Disj (lhs1, lhs2), _) -> 
        let (result1, tree1) = inclusion' currentposition lhs1 rhs ((lhs, rhs):: ctx) in 
        if List.length result1 > 0 then (result1, Node(entailent, [tree1])) 
        else 
          let (result2, tree2) = inclusion' currentposition lhs2 rhs ((lhs1,rhs)::(lhs, rhs):: ctx) in 
          (result2, Node (entailent ^ "  [DisjL]",[tree1; tree2]))

      | (_, Disj (rhs1, rhs2)) -> 
        let (result1, tree1) = inclusion' currentposition lhs rhs1 ((lhs, rhs):: ctx) in 
        if List.length result1 == 0 then (result1, Node (entailent, [tree1])) 
        else 
          let (result2, tree2) = inclusion' currentposition lhs rhs2 ((lhs, rhs):: ctx) in 
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
            let currentposition = if currentposition == (-1000) then 
            (print_string ("lalallalallal"^ string_of_int (getLineNumFromfstElem f)  ^ "\n");
            (getLineNumFromfstElem f)) else currentposition in 
            ([(lhs, currentposition, rhs)], Node (entailent ^ "  [Disprove]", []) )
          else 
            let (result, tree) = inclusion' (getLineNumFromfstElem f) derL derR ((lhs, rhs):: ctx) in 
            (result, Node(entailent ^ "  [Unfold]" , [tree])) 
        | f :: restF -> 
          let derL = (derivitives f lhs) in 
          let derR = normalise_es (derivitives f rhs) in 
          if (isBot derR) then 
            let currentposition = if currentposition == (-1000) then 
            (print_string ("lalallalallal"^ string_of_int (getLineNumFromfstElem f)  ^ "\n");
            (getLineNumFromfstElem f)) else currentposition in 
            ([(lhs, currentposition, rhs)], Node (entailent ^ "  [Disprove]", []) )
          else 
          let (result, tree) = inclusion' (getLineNumFromfstElem f) derL derR ((lhs, rhs):: ctx) in 
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
  let mixLi = cartesian_product lhs rhs in 
  let validPairs = List.filter mixLi ~f:(fun ((p1, es1), (p2, es2)) -> entailConstrains p1 p2 )
  in 
  let (f_re, f_tree) = (List.fold_left validPairs ~init:([], []) ~f:(
    fun (accre, acctree) ((p1, es1), (p2, es2)) ->
    let (re, tree) = 
    inclusion' 0 es1 es2 []
    in (List.append accre re, List.append acctree [tree])
    )) in 
    (f_re, Node ("root", f_tree))


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


let bugLocalisation (paths: error_info list): (es * (int * int) * es) list = 
  let rec helper li =
    match li with 
    | [] -> []
    | (lhs, start, rhs):: rest -> 
      let revlhs = reversees lhs in 
      let revrhs = reversees rhs in 
      let (result, tree) = inclusion' (-1000) revlhs revrhs [] in 
      print_string (showEntailemnt revlhs revrhs ^ " " ^ string_of_int (List.length result)^"\n ------- \n");

      let temp = List.map result ~f:(fun (a, n, b)-> 
(*      print_string (showEntailemnt (reversees a) (reversees b) ^ "\n ------- \n");
*)
        (reversees a, (start, n), reversees b)) in 

      
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

  let normaliseProgramStates (li:(pure * es *int) list) : effect =
    let temp = List.map li ~f:(fun (p, a, _) -> (p, normalise_es a)) in 
    temp
