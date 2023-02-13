type line_number = int option


           
type es = Bot | Emp | Any 
              | Singleton of (string * line_number) 
              | NotSingleton of string 
              | Disj of es * es 
              | Concatenate of es * es 
              | Kleene of es 

type specification = (string * es * es)

type fstElem = Wildcard | Event of (string * line_number)  | NotEvent of string

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

let string_of_binary_tree tree = printTree ~line_prefix:"* " ~get_name ~get_children tree;; 

let rec string_of_es (eff:es) : string = 
  match eff with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐"
  | Any -> "_" 
  | Singleton (str, l)          -> str ^ (match l with | None -> "" | Some i -> "@"^ string_of_int i)
  | NotSingleton str          -> "!" ^ str 
  | Concatenate (eff1, eff2) ->
      string_of_es eff1 ^ " · " ^ string_of_es eff2 
  | Disj (eff1, eff2) ->
      "(" ^ string_of_es eff1 ^ " \\/ " ^ string_of_es eff2 ^ ")"
  | Kleene effIn          ->
      "(" ^ string_of_es effIn ^ ")^*" 

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
    )
  | NotSingleton str -> 
    (match f with 
    | Wildcard _ -> Bot 
    | Event (event, _) -> if String.compare str event == 0 then Bot  else Emp
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


let rec isBot (eff:es) : bool =
  match eff with 
  | Bot -> true 
  | _ -> false 

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

  let normaliseProgramStates (li:(es*int) list) : es =
    let temp = List.map li ~f:(fun (a, _) -> normalise_es a) in 
    let rec ifstmtDisj (li: es list) = 
      match li with 
      | [] -> Bot 
      | x :: xs -> Disj (x, ifstmtDisj xs) 
    in ifstmtDisj temp