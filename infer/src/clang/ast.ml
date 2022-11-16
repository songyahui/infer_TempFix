type effects = Bot | Emp | Any | Singleton of string 
              | Disj of effects * effects 
              | Concatenate of effects * effects 
              | Kleene of effects 

type specification = (string * effects * effects)

let rec string_of_effects (eff:effects) : string = 
  match eff with 
  | Bot              -> "⏊"
  | Emp              -> "𝝐"
  | Any -> "_"
  | Singleton str          -> str 
  | Concatenate (eff1, eff2) ->
      string_of_effects eff1 ^ " · " ^ string_of_effects eff2 
  | Disj (eff1, eff2) ->
      "(" ^ string_of_effects eff1 ^ " + " ^ string_of_effects eff2 ^ ")"
  | Kleene effIn          ->
      "(" ^ string_of_effects effIn ^ ")﹡" 

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
    (*| (Concatenate (es11, es12), es3) -> (Concatenate (es11, Concatenate (es12, es3)))*)
    | _ -> (Concatenate (es1, es2))
    )
  | Kleene effIn -> 
    let effIn' = normalise_effects effIn in 
    (match effIn' with 
    | Emp -> Emp 
    | _ ->  
    Kleene (effIn'))
  | _ -> eff 