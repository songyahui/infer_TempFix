type effects = Bot | Emp | Any | Singleton of string 
              | Disj of effects * effects 
              | Concatenate of effects * effects 
              | Kleene of effects 

type specification = (string * effects * effects)

type fstElem = Wildcard | Event of string 

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

let rec string_of_effects (eff:effects) : string = 
  match eff with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐"
  | Any -> "_"
  | Singleton str          -> str 
  | Concatenate (eff1, eff2) ->
      string_of_effects eff1 ^ " · " ^ string_of_effects eff2 
  | Disj (eff1, eff2) ->
      "(" ^ string_of_effects eff1 ^ " \\/ " ^ string_of_effects eff2 ^ ")"
  | Kleene effIn          ->
      "(" ^ string_of_effects effIn ^ ")^*" 

let rec normalise_effects (eff:effects) : effects = 
  match eff with 
  | Disj(es1, es2) -> 
    let es1 = normalise_effects es1 in 
    let es2 = normalise_effects es2 in 
    (match (es1, es2) with 
    | (Emp, Emp) -> Emp
    | (Bot, es) -> normalise_effects es 
    | (es, Bot) -> normalise_effects es 
    | (Disj (es11, es12), es3) -> Disj (es11, Disj (es12, es3))
    | _ -> (Disj (es1, es2))
    )
  | Concatenate (es1, es2) -> 
    let es1 = normalise_effects es1 in 
    let es2 = normalise_effects es2 in 
    (match (es1, es2) with 
    | (Emp, _) -> normalise_effects es2
    | (_, Emp) -> normalise_effects es1
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Concatenate (es11, es12), es3) -> (Concatenate (es11, Concatenate (es12, es3)))
    | _ -> (Concatenate (es1, es2))
    )
  | Kleene effIn -> 
    let effIn' = normalise_effects effIn in 
    (match effIn' with 
    | Emp -> Emp 
    | _ ->  
    Kleene (effIn'))
  | _ -> eff 



let rec nullable (eff:effects) : bool = 
  match eff with 
  | Bot              -> false 
  | Emp              -> true 
  | Any 
  | Singleton _          -> false 
  | Concatenate (eff1, eff2) -> nullable eff1 && nullable eff2  
  | Disj (eff1, eff2) -> nullable eff1 || nullable eff2  
  | Kleene effIn      -> true

let rec fst (eff:effects) : (fstElem list) = 
  match eff with 
  | Bot              
  | Emp              
  | Any             -> [ Wildcard ]  
  | Singleton str   -> [(Event str)] 
  | Concatenate (eff1, eff2) -> 
    if nullable eff1 then List.append (fst eff1) (fst eff2)
    else (fst eff1)
  | Disj (eff1, eff2) -> List.append (fst eff1) (fst eff2)
  | Kleene effIn      -> (fst effIn) 

let rec derivitives (f:fstElem) (eff:effects) : effects = 
  match eff with 
  | Bot        
  | Emp   -> Bot                
  | Any   -> Emp
  | Singleton str -> 
    (match f with 
    | Wildcard _ -> Bot 
    | Event event -> if String.compare str event == 0 then Emp else Bot 
    )
  | Concatenate (eff1, eff2) -> 
    if nullable eff1 then 
      Disj (
        Concatenate (derivitives f eff1, eff2), 
                     derivitives f eff2)
    else Concatenate (derivitives f eff1, eff2)
  | Disj (eff1, eff2) -> Disj (derivitives f eff1, derivitives f eff2)
  | Kleene effIn      -> Concatenate (derivitives f effIn, eff)


let showEntailemnt (lhs:effects) (rhs:effects) : string =
  string_of_effects lhs  ^" |- "^ string_of_effects rhs ;;

let rec compareEffects (eff1:effects) (eff2:effects): bool =
  match (eff1, eff2) with 
  | (Bot, Bot) 
  | (Any, Any) 
  | (Emp, Emp) -> true 
  | (Singleton s1, Singleton s2) -> 
    if String.compare s1 s2 == 0  then true else false 
  | (Concatenate (a1, a2), Concatenate(a3, a4)) 
  | (Disj (a1, a2), Disj(a3, a4)) -> 
    compareEffects a1 a3 && compareEffects a2 a4
  | (Kleene e1, Kleene e2) -> compareEffects e1 e2
  | _ -> false 
  ;;
  

let compareEntailents (e1, e2) (e3, e4) : bool =
  compareEffects e1 e3 && compareEffects e2 e4

let rec reoccur (lhs:effects) (rhs:effects) (ctx: (effects*effects)list): bool = 
  match ctx with 
  | [] -> false 
  | (a, b):: xs -> 
    if compareEntailents (a, b) (lhs, rhs) 
    then true 
    else reoccur lhs rhs xs
  ;;




let rec inclusion (lhs:effects) (rhs:effects) (ctx: (effects*effects) list): (bool* binary_tree) =
  let lhs = normalise_effects lhs in 
  let rhs = normalise_effects rhs in  
  let entailent = showEntailemnt lhs rhs in 
  if nullable lhs && (not (nullable rhs)) then 
    (false, Node (entailent ^ "   [Disprove]", []) )
  else if reoccur lhs rhs ctx then 
    (true, Node (entailent ^ "   [Reoccur]", []) )
  else 
    let fstSet = fst lhs in 
    if List.length fstSet == 0 then 
      (true, Node (entailent ^ "   [Prove]", []) )
    else 
      let rec ietrater fList : (bool* binary_tree) = 
        match fList with 
        | [] -> assert false 
        | [f] -> 
          let derL = derivitives f lhs in 
          let derR = derivitives f rhs in 
          let (result, tree) = inclusion derL derR ((lhs, rhs):: ctx) in 
          (result, Node(entailent ^ "   [Unfold]" , [tree])) 
        | f :: restF -> 
          let derL = derivitives f lhs in 
          let derR = derivitives f rhs in 
          let (result, tree) = inclusion derL derR ((lhs, rhs):: ctx) in 
          (match result with 
          | true -> 
            let (resultRest, treeRest)  = ietrater restF in 
            (resultRest, Node (entailent ^ "   [Disj]",[tree; treeRest]))
          | false -> (result, tree)) 
      in ietrater fstSet 
    

        
    
  