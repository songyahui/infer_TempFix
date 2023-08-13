(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Unix
open Ast_utility
open Parser
open Lexer
module L = Logging


(* ocamlc gets confused by [module rec]: https://caml.inria.fr/mantis/view.php?id=6714 *)
(* it also ignores the warning suppression at toplevel, hence the [include struct ... end] trick *)
include struct
  [@@@warning "-60"]

  module rec CTransImpl : CModule_type.CTranslation = CTrans.CTrans_funct (CFrontend_declImpl)
  and CFrontend_declImpl : CModule_type.CFrontend = CFrontend_decl.CFrontend_decl_funct (CTransImpl)
end

(* Translates a file by translating the ast into a cfg. *)
let compute_icfg trans_unit_ctx tenv ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl (_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list ;
      L.(debug Capture Verbose) "@\n Start creating icfg@\n" ;
      let cfg = Cfg.create () in
      List.iter
        ~f:(CFrontend_declImpl.translate_one_declaration trans_unit_ctx tenv cfg `DeclTraversal)
        decl_list ;
      L.(debug Capture Verbose) "@\n Finished creating icfg@\n" ;
      cfg
  | _ ->
      assert false


(* NOTE: Assumes that an AST always starts with a TranslationUnitDecl *)

let init_global_state_capture () =
  Ident.NameGenerator.reset () ;
  CFrontend_config.global_translation_unit_decls := [] ;
  CFrontend_config.reset_block_counter ()



let rec getRoot str = 
  let strLi = String.split_on_chars  str ['.'] in 
  match strLi with
  | [] -> str 
  | x :: _ -> x
;;

let string_of_source_range ((s1, s2):Clang_ast_t.source_range) :string = 
  match (s1.sl_file, s2.sl_file) with 
  | (Some name, _) 
  | (_, Some name) -> name
  | (_, _) -> "none"



let stmt2Term_helper (op: string) (t1: terms option) (t2: terms option) : terms option = 
  match (t1, t2) with 
  | (None, _) 
  | (_, None ) -> None 
  | (Some t1, Some t2) -> 
    let p = 
      if String.compare op "+" == 0 then Plus (t1, t2)
    else Minus (t1, t2)
    in Some p 

let rec stmt2Term (instr: Clang_ast_t.stmt) : terms option = 
  (*print_endline ("term kind:" ^ Clang_ast_proj.get_stmt_kind_string instr);*)

  match instr with 
  | ImplicitCastExpr (_, x::_, _, _, _) 
    -> 
    stmt2Term x
  | CStyleCastExpr (_, x::rest, _, _, _) 
  | ParenExpr (_, x::rest, _) -> 
  (*print_string ("ParenExpr/CStyleCastExpr: " ^ (
    List.fold_left (x::rest) ~init:"" ~f:(fun acc a -> acc ^ "," ^ Clang_ast_proj.get_stmt_kind_string a)
  )^ "\n");
  *)

    stmt2Term x
  
  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
    (match binop_info.boi_kind with
    | `Add -> stmt2Term_helper "+" (stmt2Term x) (stmt2Term y) 
    | `Sub -> stmt2Term_helper "" (stmt2Term x) (stmt2Term y) 
  | _ -> None 
  )
  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    let int_str = integer_literal_info.ili_value in 

    if String.length int_str > 18 then Some (Basic(BVAR "SYH_BIGINT"))
    else Some (Basic(BINT (int_of_string(int_str))))

  | DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) -> 
    let (sl1, sl2) = stmt_info.si_source_range in 

    (match decl_ref_expr_info.drti_decl_ref with 
    | None -> None 
    | Some decl_ref ->
      (
      match decl_ref.dr_name with 
      | None -> None
      | Some named_decl_info -> Some (Basic(BVAR (named_decl_info.ni_name)))
      
      )
    )
  | NullStmt _ -> Some (Basic(BVAR ("NULL")))
  | MemberExpr (_, arlist, _, member_expr_info)  -> 
    let memArg = member_expr_info.mei_name.ni_name in 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    let name  = List.fold_left temp ~init:"" ~f:(fun acc a -> 
    acc ^ (
      match a with
      | None -> "_"
      | Some t -> string_of_terms t ^ "."
    )) in 
    Some (Basic(BVAR(name^memArg)))

  | ArraySubscriptExpr (_, arlist, _)  -> 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    (*print_endline (string_of_int (List.length temp)); *)
    let name  = List.fold_left temp ~init:"" ~f:(fun acc a -> 
    acc ^ (
      match a with
      | None -> "_"
      | Some t -> string_of_terms t ^ "_"
    )) in 
    Some (Basic(BVAR(name)))

  | UnaryOperator (stmt_info, x::_, expr_info, op_info) ->
    (match op_info.uoi_kind with
    | `Minus -> 
      (match stmt2Term x with 
      | Some (Basic (BINT t)) -> Some (Basic(BINT (0-t)))
      | _ -> 
        (*print_endline ("`stmt2Term UnaryOperator2 "); *)
        stmt2Term x

      )
      
    | _ -> 
      (*print_endline ("`stmt2Term UnaryOperator"); *)
      stmt2Term x
    )
   

  | RecoveryExpr (_, [], _) -> Some (Basic(BINT(0))) 
  | StringLiteral (_, _, _, str_list) -> 
    
    let str = 
      let rec straux li = 
      match li with 
      | [] -> ""
      | x :: xs  -> x  ^ " " ^ straux xs 
      in  straux str_list
    in 
    Some (Basic(BVAR("\"" ^ str ^ "\""))) 

  | ConditionalOperator (_, x::y::_, _) -> stmt2Term y 

  | _ -> Some (Basic(BVAR(Clang_ast_proj.get_stmt_kind_string instr))) 


let rec string_of_decl (dec :Clang_ast_t.decl) : string = 
  match dec with 
  | VarDecl (_, ndi, qt, vdi) -> 
    ndi.ni_name ^ "::" ^ Clang_ast_extend.type_ptr_to_string qt.qt_type_ptr
    ^" "^ (match vdi.vdi_init_expr with 
    | None -> ""
    | Some stmt -> string_of_stmt stmt)

    (* clang_prt_raw 1305- int, 901 - char *)
  | _ ->  Clang_ast_proj.get_decl_kind_string dec

and string_of_stmt_list (li: Clang_ast_t.stmt list) sep : string = 
    match li with 
  | [] -> ""
  | [x] -> string_of_stmt x 
  | x::xs -> string_of_stmt x ^ sep ^ string_of_stmt_list xs sep

and string_of_stmt (instr: Clang_ast_t.stmt) : string = 
  let rec helper_decl li sep = 
    match li with 
  | [] -> ""
  | [x] -> string_of_decl  x 
  | x::xs -> string_of_decl  x ^ sep ^ helper_decl xs sep
  in 
(*
  let rec helper li sep = 
    match li with 
  | [] -> ""
  | [x] -> string_of_stmt x 
  | x::xs -> string_of_stmt x ^ sep ^ helper xs sep
  in 
*)
  match instr with 
  | ReturnStmt (stmt_info, stmt_list) ->
    "ReturnStmt " ^ string_of_stmt_list stmt_list " " 

  | MemberExpr (_, arlist, _, member_expr_info)  -> 
    let memArg = member_expr_info.mei_name.ni_name in 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    let name  = List.fold_left temp ~init:"" ~f:(fun acc a -> 
    acc ^ (
      match a with
      | None -> "_"
      | Some t -> string_of_terms t ^ "."
    )) in name^memArg

  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    (*"IntegerLiteral " ^*) integer_literal_info.ili_value

  | StringLiteral (_, stmt_list, expr_info, str_list) -> 
    let rec straux li = 
      match li with 
      | [] -> ""
      | x :: xs  -> x  ^ " " ^ straux xs 
    in (* "StringLiteral " ^ string_of_int (List.length stmt_list)  ^ ": " ^ *) straux str_list


  | UnaryOperator (stmt_info, stmt_list, expr_info, unary_operator_info) ->
    "UnaryOperator " ^ string_of_stmt_list stmt_list " " ^ ""
  
  | ImplicitCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
    (*"ImplicitCastExpr " ^*) string_of_stmt_list stmt_list " " 
  | DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) ->
    (*"DeclRefExpr "^*)
    (match decl_ref_expr_info.drti_decl_ref with 
    | None -> "none"
    | Some decl_ref ->
      (
        match decl_ref.dr_name with 
        | None -> "none1"
        | Some named_decl_info -> named_decl_info.ni_name
      )
    )

  | ParenExpr (stmt_info (*{Clang_ast_t.si_source_range} *), stmt_list, _) ->

    "ParenExpr " (*^ string_of_source_range  stmt_info.si_source_range*)
    ^ string_of_stmt_list stmt_list " " 

    
  | CStyleCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
    string_of_stmt_list stmt_list " " ^ ""


  | IfStmt (stmt_info, stmt_list, if_stmt_info) ->

  "IfStmt " ^ string_of_stmt_list stmt_list "," ^ ""
 
  | CompoundStmt (_, stmt_list) -> string_of_stmt_list stmt_list "; " 

  | BinaryOperator (stmt_info, stmt_list, expr_info, binop_info) -> 
   "BinaryOperator " ^ string_of_stmt_list stmt_list (" "^ Clang_ast_proj.string_of_binop_kind binop_info.boi_kind ^" ")  ^""

  | DeclStmt (stmt_info, stmt_list, decl_list) -> 
  "DeclStmt " (*  ^ string_of_stmt_list stmt_list " " ^ "\n"^
    "/\\ "^ string_of_int stmt_info.si_pointer^ " " *)  ^ helper_decl decl_list " " ^ "" 
  
  | CallExpr (stmt_info, stmt_list, ei) -> 
      (match stmt_list with
      | [] -> assert false 
      | x :: rest -> 
    "CallExpr " ^ string_of_stmt x ^" (" ^  string_of_stmt_list rest ", " ^ ") "
)

  | ForStmt (stmt_info, [init; decl_stmt; condition; increment; body]) ->
    "ForStmt " ^  string_of_stmt_list ([body]) " " 

  | WhileStmt (stmt_info, [condition; body]) ->
    "WhileStmt " ^  string_of_stmt_list ([body]) " " 
  | WhileStmt (stmt_info, [decl_stmt; condition; body]) ->
    "WhileStmt " ^  string_of_stmt_list ([body]) " " 

  | RecoveryExpr (stmt_info, x::_, _) -> "RecoveryExpr " ^ string_of_stmt x
  | RecoveryExpr (stmt_info, [], _) -> "RecoveryExpr []" 

  | BreakStmt _ -> "BreakStmt"

  | ArraySubscriptExpr (_, arlist, _)  -> 
    let temp = List.map arlist ~f:(fun a -> stmt2Term a) in 
    (*print_endline (string_of_int (List.length temp)); *)
    let name  = List.fold_left temp ~init:"" ~f:(fun acc a -> 
    acc ^ (
      match a with
      | None -> "_"
      | Some t -> string_of_terms t ^ "_"
    )) in 
    ("ArraySubscriptExpr " ^ name)


  | _ -> "string_of_stmt not yet " ^ Clang_ast_proj.get_stmt_kind_string instr;;






let rec extractEventFromFUnctionCall (x:Clang_ast_t.stmt) (rest:Clang_ast_t.stmt list) : event option = 
(match x with
| DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) -> 
  let (sl1, sl2) = stmt_info.si_source_range in 
  let (lineLoc:int option) = sl1.sl_line in 

  (match decl_ref_expr_info.drti_decl_ref with 
  | None -> None  
  | Some decl_ref ->
    (
    match decl_ref.dr_name with 
    | None -> None 
    | Some named_decl_info -> 
      Some (named_decl_info.ni_name, argumentsTerms2basic_types((
        List.map rest ~f:(fun r -> stmt2Term r))))
    )
  )

| ImplicitCastExpr (_, stmt_list, _, _, _) ->
  (match stmt_list with 
  | [] -> None 
  | y :: restY -> extractEventFromFUnctionCall y rest)
| _ -> None 
)

let getFirst (a, _) = a

let conjunctPure (pi1:pure) (pi2:pure): pure = 
  (*if entailConstrains pi1 pi2 then pi1 
        else if entailConstrains pi2 pi1 then pi2
        else*)  PureAnd (pi1, pi2)


let concatenateTwoEffect (eff1:effect) (eff2:effect) : effect = 
  let (mixLi:(((pure * es) * (pure * es)) list)) = cartesian_product eff1 eff2 in 
  List.map mixLi ~f:(fun ((pi1, es1), (pi2, es2)) -> 
    let (trace:es)  = Concatenate(es1, es2) in 
    match (pi1, pi2) with
    | (TRUE, p2) -> (p2, trace)
    | (p1, TRUE) -> (p1, trace)
    | (_, _) -> 
      let newPure = conjunctPure pi1 pi2 in 
      (newPure, trace)
  )

let rec findReturnValue (pi:pure) : terms option = 
  match pi with
  | Eq (Basic (BRET), t2) 
  | Eq (t2, Basic (BRET)) -> Some t2 
  | TRUE 
  | FALSE 
  | Gt _ 
  | Lt _ 
  | GtEq _ 
  | LtEq _ 
  | Neg _ 
  | Eq _ -> None 
  | PureAnd (pi1, pi2) 
  | PureOr (pi1,pi2) -> 
      (match findReturnValue pi1 with 
      | None -> findReturnValue pi2 
      | Some t -> Some t)
  
(* be carefule with this function, which is specifically used when checking future condition. *)
let concatenateTwoEffectswithoutFlag (effectLi4X: programStates) (effectRest: programStates): programStates = 
  let mixLi = cartesian_product effectLi4X effectRest in 
  
  let temp = List.map mixLi ~f:(
    fun ((pi1, eff_x, t_x, fp1),  (pi2, eff_y, t_y, fp2)) -> 
      match findReturnValue pi1 with 
      | Some (Basic (BVAR (retTerm))) -> 
        (conjunctPure pi1 pi2, Concatenate (eff_x, instantiateRetEs eff_y [(retTerm, BRET)]  ),  t_y, List.append fp1 fp2)
      | _ -> (conjunctPure pi1 pi2, Concatenate (eff_x, eff_y),  t_y, List.append fp1 fp2)

  ) in 
  normaliseProgramStates temp
  


let concatenateTwoEffectswithFlag (effectLi4X: programStates) (effectRest: programStates): programStates = 
  let mixLi = cartesian_product effectLi4X effectRest in 
  (*
  print_endline ("============");
  print_string (string_of_int (List.length effectLi4X) ^ 
  " x " ^  string_of_int (List.length effectRest)^ "\n");

  print_string (string_of_int (List.length mixLi) ^ "\n");
  print_endline ("============");

  print_endline ("concatenateTwoEffectswithFlag 0");
*)
  let temp = List.map mixLi ~f:(
    fun ((pi1, eff_x, t_x, fp1),  (pi2, eff_y, t_y, fp2)) -> 
      if t_x > 0 then (pi1, eff_x, t_x, fp1)
      else
      (conjunctPure pi1 pi2, Concatenate (eff_x, eff_y),  t_y, List.append fp1 fp2)
  ) in 

  let ret = normaliseProgramStates temp in 

  ret
  



  
  
let enforePure (p:pure) (eff:programStates) : programStates = 
  List.map eff ~f:(fun (p1, es, f, fp) ->(conjunctPure p p1, es, f, fp)) 


let stmt2Pure_helper (op: string) (t1: terms option) (t2: terms option) : pure option = 
  match (t1, t2) with 
  | (None, _) 
  | (_, None ) -> None 
  | (Some t1, Some t2) -> 
    let p = 
      if String.compare op "<" == 0 then Lt (t1, t2)
    else if String.compare op ">" == 0 then Gt (t1, t2)
    else if String.compare op ">=" == 0 then GtEq (t1, t2)
    else if String.compare op "<=" == 0 then LtEq (t1, t2)
    else if String.compare op "!=" == 0 then Neg (Eq (t1, t2))

    else Eq (t1, t2)
    in Some p 


let rec stmt2Pure (instr: Clang_ast_t.stmt) : pure option = 

  match instr with 
  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
    (match binop_info.boi_kind with
    | `LT -> stmt2Pure_helper "<" (stmt2Term x) (stmt2Term y) 
    | `GT -> stmt2Pure_helper ">" (stmt2Term x) (stmt2Term y) 
    | `GE -> stmt2Pure_helper ">=" (stmt2Term x) (stmt2Term y) 
    | `LE -> stmt2Pure_helper "<=" (stmt2Term x) (stmt2Term y) 
    | `EQ -> stmt2Pure_helper "=" (stmt2Term x) (stmt2Term y) 
    | `NE -> stmt2Pure_helper "!=" (stmt2Term x) (stmt2Term y) 
    | `Or | `LOr | `Xor-> 
      (match ((stmt2Pure x ), (stmt2Pure y )) with 
      | Some p1, Some p2 -> Some (Ast_utility.PureOr (p1, p2))
      | Some p1, None -> Some (p1)
      | None, Some p1 -> Some (p1)
      | None, None -> None 
      )
    | _ -> None 
    )

  | ImplicitCastExpr (_, x::_, _, _, _) -> stmt2Pure x
  | UnaryOperator (stmt_info, x::_, expr_info, op_info)->
    (match op_info.uoi_kind with
    | `Not -> 
      (match stmt2Pure x with 
      | None -> None 
      | Some p -> Some (Neg p))
    | `LNot -> 
      (match stmt2Term x with 
      | None -> None 
      | Some t -> Some (Eq(t, Basic(BINT 0)))
      )
      
    | _ -> 
      (*print_endline ("`LNot DeclRefExpr none4"); *)
      None
    )
  | ParenExpr (_, x::rest, _) -> stmt2Pure x

  | DeclRefExpr _ -> 
    (match stmt2Term instr with 
    | Some t -> Some (Neg(Eq(t, Basic(BINT 0))))
    | _ -> Some (Gt ((Basic( BVAR (Clang_ast_proj.get_stmt_kind_string instr))), Basic( BVAR ("null"))))
    )
  
  | _ -> Some (Gt ((Basic( BVAR (Clang_ast_proj.get_stmt_kind_string instr))), Basic( BVAR ("null"))))


  
let prefixLoction (li: int list) (state:programStates) : programStates= 
  List.map state ~f:(fun (a, b, c, d) -> (a, b, c, List.append li d))


let postfixLoction (li: int list) (state:programStates) : programStates= 
  List.map state ~f:(fun (a, b, c, d) -> (a, b, c, List.append d li))


let getStmtlocation (instr: Clang_ast_t.stmt) : (int option * int option) =
  match instr with 
  | CompoundStmt (stmt_info, _) 
  | DeclStmt (stmt_info, _, _) 
  | ReturnStmt (stmt_info, _) 
  | UnaryOperator (stmt_info, _, _, _) 
  | ImplicitCastExpr (stmt_info, _, _, _, _) 
  | BinaryOperator (stmt_info, _, _, _)
  | CompoundAssignOperator (stmt_info, _, _, _, _)
  | CallExpr (stmt_info, _, _)  
  | ParenExpr (stmt_info (*{Clang_ast_t.si_source_range} *), _, _)
  | ArraySubscriptExpr (stmt_info, _, _) 
  | UnaryExprOrTypeTraitExpr (stmt_info, _, _, _)
  | IfStmt (stmt_info, _, _) 
  | CXXConstructExpr (stmt_info, _, _, _)
  | ExprWithCleanups (stmt_info, _, _, _)
  | CXXDeleteExpr (stmt_info, _, _, _)
  | ForStmt (stmt_info, _)
  | MemberExpr (stmt_info, _ , _, _) 
  | BreakStmt (stmt_info, _ )
  | DefaultStmt (stmt_info, _) 
  | CaseStmt (stmt_info, _) 
  | SwitchStmt (stmt_info, _, _)
  | DeclRefExpr (stmt_info, _, _, _)
  | NullStmt (stmt_info, _)
  | CXXOperatorCallExpr (stmt_info, _, _)
  | CStyleCastExpr (stmt_info, _, _, _, _)  ->
    let (sl1, sl2) = stmt_info.si_source_range in 
    (sl1.sl_line , sl2.sl_line)
  | _ -> (None, None) 

let maybeIntToListInt ((s1, s2):(int option * int option )) : (int list * int list)  = 
  let aux l = match l with | None -> [] | Some l -> [l] 
  in (aux s1, aux s2)


let stmt_intfor2FootPrint (stmt_info:Clang_ast_t.stmt_info): (int list * int list) = 
  let ((sl1, sl2)) = stmt_info.si_source_range in 
    (* let (lineLoc:int option) = sl1.sl_line in *)
  maybeIntToListInt (sl1.sl_line, sl2.sl_line) 


(*
env - records all the specifications 
current - \Phi_{pre} prestates 
future - F garenteed to be happening 
varSet - key varaibles to capture 
instr - current expression 
output - postcondition (the extension derived from instr)
---------------------------------------
F ｜- {current} instr {postconsition }
*)


let rec findSpecFrom (specs:specification list) (fName: string): specification option = 
  match specs with 
  | [] -> None
  | ((str, li), a, b, c):: rest -> if String.compare str fName == 0 then Some ((str, li), a, b, c) else 
  findSpecFrom rest fName
  ;;


let string_of_decl (decl:Clang_ast_t.decl) : string = 
  match decl with
  | Clang_ast_t.VarDecl (_, a , _, _) -> 
  (*Clang_ast_proj.get_decl_kind_string*) a.ni_name 
  | ParmVarDecl (_, ndi, _, _) -> ndi.ni_name

  | _ -> Clang_ast_proj.get_decl_kind_string decl


let rec var_binding (formal:string list) (actual: basic_type list) : bindings = 
  match (formal, actual) with 
  | (x::xs, v::ys) -> (x, v) :: (var_binding xs ys)
  | _ -> []
  ;;

let specialCases es = 
  match es with 
  | Kleene (NotSingleton (str, _)) -> 
    if String.compare str "_" == 0  then true else false 
  | _ ->  false 
  ;;

let rec synthsisFromSpec (effect:(pure * es)) (env:(specification list)) : string option =  

  (*
     let () = finalReport := !finalReport ^ (**"synthsisFromSpec " ^*) (string_of_effect ([effect])) in 
*)
  (*print_endline ("ENV now:\n" ^ List.fold_left env ~init:"" 
  ~f:(fun acc ((mn, _), _, post, _) -> acc ^ 
  match post with 
  | None -> ""
  | Some eff -> mn ^ ": " ^ string_of_effect eff ^ "\n"
  ));*)

  let (pi, spec) = effect in 
  let spec =  normalise_es spec in 

  if specialCases spec then (* to check if the expected spec is (!_(u))^*, if ture just exit *)
    Some ("if (" ^ string_of_pure_output (normalPure pi) ^ "){ return; }")
  else 

  let patch = 
  (match spec with 
  | Emp -> Some ""
  | _ -> 
    let rec auc (currectProof:es) envli : string option = 
      match envli with 
      | [] -> 
        None (* no ingradients from the env *)
      | ((fName, li::_), _, Some post, _) :: xs  -> 

        let handler = getAllVarFromES currectProof in 
        let (vb, arg) = 
          match handler with 
          | []->([], "")
          | x ::_ -> 
          (*print_string ("using " ^ x^ " to replace "^ li ^"\n"); *)
          ([(li , BVAR x)], x)
          in 
        let post = instantiateAugumentSome post vb in 
        let (result, tree) = effect_inclusion post ([(pi, currectProof)]) in 
        (*print_string (fName ^ "\n" ^ string_of_binary_tree  tree  ^ "\n"); *)

        let temp = 
          match result with 
          | [] -> Some (fName ^ "("^ arg ^"); ") 
          | (_, a, _, b):: _ -> 
            (match normalise_es a with 
            | Emp -> 
              if comparees (normalise_es b) currectProof == true 
              then auc currectProof xs 
              else 
              let recursiveRes = synthsisFromSpec (pi, b) env in 
              (match recursiveRes with 
              | None  -> None 
              | Some rest -> Some (fName ^ "(); " ^ rest))
            | _ -> auc currectProof xs 
            ) in temp
        (*match temp with 
        | None -> auc currectProof xs 
        | _ -> temp*)
      | ((fName, []), _, Some post, _) :: xs  -> 

        let post = post in 
        let (result, tree) = effect_inclusion post ([(pi, currectProof)]) in 
        (*print_string (fName ^ "\n" ^ string_of_binary_tree  tree  ^ "\n"); *)

        let temp = 
          match result with 
          | [] -> Some (fName ^ "(); ") 
          | (_, a, _, b):: _ -> 
            (match normalise_es a with 
            | Emp -> 
              if comparees (normalise_es b) currectProof == true 
              then auc currectProof xs 
              else 
              let recursiveRes = synthsisFromSpec (pi, b) env in 
              (match recursiveRes with 
              | None  -> None 
              | Some rest -> Some (fName ^ "(); " ^ rest))
            | _ -> auc currectProof xs 
            ) in temp
            
      | x :: xs  -> auc currectProof xs 
    in auc spec env) in 
    (match patch with 
    | None -> None 
    | Some fix -> 
       if entailConstrains TRUE pi == false then  
          Some ("if (" ^ string_of_pure_output (normalPure pi) ^ "){" ^ fix ^"}")
       else Some fix)


  
let computeAllthePointOnTheErrorPath (p1:pathList) (p2:pathList) : int list = 
  let correctDots = flattenList p1 in 
  let errorDots = flattenList p2 in 
  (*
  print_endline ("correctDots path:" ^ List.fold_left correctDots ~init:"" ~f:(fun acc a -> acc ^" " ^ string_of_int a)) ; 
  print_endline ("errorDots path:" ^ List.fold_left errorDots ~init:"" ~f:(fun acc a -> acc ^" " ^ string_of_int a)) ; 
*)
  let rec helper li a : bool =
    match li with 
    | [] -> false 
    | x :: xs -> if x==a then true else helper xs a
  in 
  List.fold_left errorDots ~init:[] ~f:(fun acc a -> 
  if helper correctDots a then acc else List.append acc [a]) 

let computeRange intList = 
  match intList with 
  | [] -> (0, 10000000)
  | x :: xs  -> 
    List.fold_left xs ~init:(x, x) 
    ~f:(fun (min, max) a -> 
      if a < min then (a, max) 
      else if a > max then (min, a)
      else (min, max)) 


let program_repair (info:((error_info list) * binary_tree * pathList * pathList)) specifications : (string * string) = 
  
  let headMsg = ref "" in 
  let repairMsg = ref "" in 
  let start = Unix.gettimeofday () in 
  let (error_paths, tree, correctTraces, errorTraces) = info in 
  if List.length error_paths == 0 then (!headMsg, !repairMsg)
  else 
  (* here the int int are the strat point and the end point *)
  let (error_lists:((pure * es * (int * int) * es) list)) = bugLocalisation error_paths in 

  let () = headMsg := !headMsg ^ 
    ("\n<======[Bidirectional Bug Localization & Possible Proof Repairs]======>\n\n[Repair Options]\n") in 


  let rec helper li = 
      match li with 
      | [] -> ""
      | (pathcondition, realspec, _, spec):: res  -> 
      ( string_of_pure pathcondition ^ " /\\ " ^ 
        string_of_es realspec ^ " ~~~> " ^ string_of_es spec ) ^ ";\n" ^ helper res
  in 
  let msg = helper error_lists in 

  let () = headMsg := !headMsg ^ msg in 


  let onlyErrorPostions = computeAllthePointOnTheErrorPath correctTraces errorTraces in 

  (* this is to prevent the same fixes *)


  let rec existSameRecord recordList start endNum  : bool = 
    match recordList with 
    | [] -> false 
    | (s',e'):: recordListxs -> if start ==s' (*&& endNum==e'*) then true else existSameRecord recordListxs start endNum
  in 
  
  let rec aux arg : unit  = 
      let (pathcondition, realspec, (startNum ,endNum),  spec) = arg in 
        (*
        print_endline ("init:" ^ (string_of_int startNum) ^ ", "^ (string_of_int endNum)); 
*)
        
        let dotsareOntheErrorPath = List.filter onlyErrorPostions ~f:(fun x -> x >= startNum && x <=endNum) in 
        let (lowerError, upperError) = computeRange dotsareOntheErrorPath in 
        let (startNum, endNum) = 
          let startNum' = if lowerError > startNum then lowerError else startNum in 
          let endNum' = if upperError < endNum then upperError else endNum in 
          (startNum', endNum')
        in 
        

        if existSameRecord !repairRecord startNum endNum then ()
        else 
        let () = repairRecord := (startNum ,endNum) :: (!repairRecord) in 


        (*print_endline ("post:" ^(string_of_int startNum) ^ ", "^ (string_of_int endNum)); 
*)
        modifiyTheassertionCounters (); 

        (*let startTimeStamp = Unix.gettimeofday() in*)
        let (specifications: specification list) = List.append specifications !dynamicSpec in 
        
        
        let list_of_functionCalls = synthsisFromSpec (pathcondition, spec) (specifications) in

        (*let startTimeStamp01 = Unix.gettimeofday() in*)

        let temp = 
        ("@ line " ^ string_of_int startNum ^ (*^ " to line " ^  string_of_int endNum ^ *)
        (match list_of_functionCalls with 
        | None -> 
          let (rr, _) = effect_inclusion [(TRUE, Emp)] [(TRUE, spec)] in 
          if List.length rr == 0 then  
          let () = reapiredFailedAssertions := !reapiredFailedAssertions + 1 in 
          " can be deleted." 
          else 
          " Sorry, there is no path from the environment!"
        | Some str -> 
          let () = reapiredFailedAssertions := !reapiredFailedAssertions + 1 in 
          if String.compare str "" == 0 then " can be deleted." 
          else  " can be inserted with code " ^  str)
         ^ "\n" 
         (*^ "[Searching Time] " ^ string_of_float (startTimeStamp01 -. startTimeStamp)^ " seconds.\n\n"*)
        ) in 

        let () = repairMsg := temp ^ !repairMsg in 
        ()

  in 

  let _ = List.map error_lists ~f:(fun a -> aux a) in 

  let repari_time = (Unix.gettimeofday () -. start) in 
  repairTime := !repairTime +. repari_time; 
  (!headMsg , !repairMsg)

let constructADeclRefExprStmt stmt_info expr_info (str: string): Clang_ast_t.stmt = 
  let named_decl_info = {Clang_ast_t.ni_name = str; ni_qual_name = []} in 
  let decl_ref = {Clang_ast_t.dr_kind =`Var; dr_decl_pointer=0; dr_name = Some named_decl_info; dr_is_hidden=false; dr_qual_type = None } in 
  let decl_ref_expr_info = {Clang_ast_t.drti_decl_ref= Some decl_ref; drti_found_decl_ref = Some decl_ref}

  in DeclRefExpr (stmt_info, [], expr_info, decl_ref_expr_info)
 
let constructBinaryOperatorAssign stmt_info expr_info x y : Clang_ast_t.stmt = 
  let binary_operator_info = {Clang_ast_t.boi_kind = `EQ}in 
  BinaryOperator (stmt_info, [x;y], expr_info, binary_operator_info)

let rec syh_compute_stmt_postcondition (env:(specification list)) (current:programStates) 
(future:effect option) (instr: Clang_ast_t.stmt) : programStates = 

  
  
  (*
  print_endline ((Clang_ast_proj.get_stmt_kind_string instr));
  *)
  
  let rec helper current' (li: Clang_ast_t.stmt list): programStates  = 
    
    (*
    print_string ("==> helper: ");
    let _ = List.map li ~f:(fun a-> print_string ((Clang_ast_proj.get_stmt_kind_string a)^", ")) in 
    print_endline ("");
    *)
    
    

    match li with
    | [] -> [(TRUE, Emp, 0, [])]

    | DeclStmt (_, [CStyleCastExpr(_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _)], [del]):: xs  
    | DeclStmt (_, [(CallExpr (stmt_info, stmt_list, ei))], [del]) ::xs 
    | DeclStmt (_, [(ImplicitCastExpr (_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _))], [del]) ::xs ->


      let localVar = (string_of_decl del) in 
      (*print_endline ("DeclStmt " ^ localVar); *)

      let () = handlerVar := Some (localVar) in 
      let () = variablesInScope := !variablesInScope @ [localVar] in 
      helper current' ((Clang_ast_t.CallExpr (stmt_info, stmt_list, ei))::xs)

    | (CallExpr (stmt_info, stmt_list, ei)) ::xs -> 
      (*print_endline ("I am here call");*)
      
(* STEP 0: retrive the spec of the callee *)
      let (fp, _) = stmt_intfor2FootPrint stmt_info in 

      let ((calleeName, formalLi), prec, postc, futurec) = 
        match stmt_list with 
        | [] -> assert false  
        | x::rest -> 
          (match extractEventFromFUnctionCall x rest with 
          | None -> 
            (("none", []), None, None, None)
          | Some (calleeName, acturelli) -> (* arli is the actual argument *)
            
            (*
            let () = print_string ("=========================\n") in 
            print_string (string_of_event (calleeName, acturelli) ^ ":\n");
            *)

            let spec = findSpecFrom env calleeName in 
            match spec with
            | None -> ((calleeName, []), None, None, None)
            | Some ((signiture, formalLi), prec, postc, futurec)-> 
              (*
              print_endline ("formal Arg = " ^ List.fold_left formalLi ~init:"" ~f:(fun acc a -> acc ^ "," ^a));
              print_endline ("actual Arg = " ^ List.fold_left acturelli ~init:"" ~f:(fun acc a -> acc ^ "," ^ string_of_basic_t a));
*)            print_endline ("CallingFunction: " ^ calleeName ^ string_of_foot_print fp );

              (match !handlerVar, futurec with 
              | None, _  -> () (*print_endline ("with no handler = ")*)
              | Some str, Some _ -> 
                varSet := List.append !varSet [str]
              | _, _ -> ()
                (*print_endline ("with handler = " ^ str)*)); 

              if List.length acturelli == List.length formalLi then 
                let vb = var_binding formalLi acturelli in 
                ((signiture, formalLi), 
                instantiateAugument prec vb, 
                instantiateAugument postc vb, 
                instantiateAugument futurec vb)
                (* spec instantiation *)
              else 
              (((signiture, formalLi), prec, postc, futurec))
            
          )
      in 


      (*
      let () = print_string ("=========================\n") in 
      
      
      print_string (string_of_function_sepc (prec, postc, futurec)^"\n"); 
      *)
        

(* STEP 1: check precondition *)
      let () = 
        match prec with 
        | None -> ()
        | Some prec -> 
          let info = 
            effectwithfootprintInclusion (programStates2effectwithfootprintlist current') prec in 

            let extra_info = 
            "\n~~~~~~~~~ In function: "^ !currentModule ^" ~~~~~~~~~\n" ^
            "Pre-condition checking for \'"^calleeName^"\': " in 
            (*print_endline (string_of_inclusion_results extra_info info); *)
            let (head, patches) = program_repair info env in 
            if String.compare patches "" == 0 then 
            ()
            else 
              let () = finalReport := !finalReport ^ (string_of_inclusion_results extra_info info) in 
              let () = finalReport := !finalReport ^ head in 
              let () = finalReport := !finalReport ^ ("[Patches]\n") ^ patches ^ "\n" in 
              ()
              
      in 
(* STEP 2: obtain the next state *)
      let (postc: effect option) = enforeceLineNum fp postc in 
      let (postc, futurec) = 
        match !handlerVar with 
        | None -> (postc, futurec)
        | Some handler ->  
          (instantiateAugument postc [(handler, BRET)], 
           instantiateAugument futurec [(handler, BRET)])
      in 

      let () = handlerVar := None in 


      let () = varSet := List.append !varSet (varFromEffects postc) in 
      (*print_string ("adding varset: "); 
      print_string (string_of_varSet (!varSet));
      *)

      let current'' = 
        match postc with 
        | None -> current'  
        | Some postc -> 
          concatenateTwoEffectswithFlag current' (effects2programStates postc)
      in 


(* STEP 3: compute the effect for the rest code *)
  (*print_endline ("computing restSpec"  ^ string_of_int(List.length xs));
*)

      let effectRest = 
        if (String.compare calleeName "exit") == 0 || 
           (String.compare calleeName "flexerror") == 0 || 
           (String.compare calleeName "flexfatal") == 0 then 
          (
          concatenateTwoEffectswithFlag current'' [(Ast_utility.TRUE, Emp, 1, fp)])
        else helper (current'') xs in

      

      
(* STEP 4: check the future spec of the callee *)

      let full_extension = 
        (match postc with 
        | None ->  effectRest 
        | Some postc -> 
          concatenateTwoEffectswithFlag (effects2programStates postc) effectRest
        ) in 

      (match futurec with 
      | None -> full_extension
      | Some futurec -> 
          let restSpecLHS = 
            match future with
            | None -> effectRest 
            | Some ctxfuture -> 
              (*print_endline ("+ ctx future spec: " ^ string_of_effect ctxfuture);
              *)
              concatenateTwoEffectswithoutFlag (effectRest) (effects2programStates ctxfuture)
          in 

          (*
          print_endline (" = restSpecLHS: " ^ string_of_programStates restSpecLHS);
          print_endline ("|- before RHS: " ^ string_of_effect futurec);
          *)

          (*let futurec = match findReturnValueProgramStates effectRest with 
            | None  -> futurec
            | Some str -> 
              print_endline(str);
              let vb = [(str, BRET)] in 
              instantiateAugumentSome futurec vb 
          in *)

          (*print_endline ("|- RHS: " ^ string_of_effect futurec);*)
          let info = effectwithfootprintInclusion (programStates2effectwithfootprintlist (normaliseProgramStates restSpecLHS)) futurec in 
          let extra_info = "\n~~~~~~~~~ In function: "^ !currentModule 
          ^" ~~~~~~~~~\nFuture-condition checking for \'"^calleeName^ string_of_foot_print fp ^"\': " in 
          (*print_endline (string_of_inclusion_results extra_info info); *)
          

          (let (head, patches) = program_repair info env in 
          if String.compare patches "" == 0 then 
          ()
          else 
            let () = finalReport := !finalReport ^ (string_of_inclusion_results extra_info info) in 
            let () = finalReport := !finalReport ^ head in 
            let () = finalReport := !finalReport ^ ("[Patches]\n ") ^ patches ^ "\n" in 
            ());

          full_extension
        )  
    | BinaryOperator (_, _::(ImplicitCastExpr (_, [(BinaryOperator (stmt_info1, x::(ImplicitCastExpr (_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _))::_, expr_info, binop_info))], _, _, _))::_, _, _) :: xs 
    | BinaryOperator (_, _::(ImplicitCastExpr (_, [BinaryOperator (stmt_info1, x::(CStyleCastExpr (_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _))::_, expr_info, binop_info)], _, _, _))::_, _, _) :: xs 
    | BinaryOperator (_, _::(ImplicitCastExpr (_, [BinaryOperator (stmt_info1, x::(CallExpr (stmt_info, stmt_list, ei))::_, expr_info, binop_info)], _, _, _))::_, _, _) :: xs 

    | BinaryOperator (stmt_info1, x::(ImplicitCastExpr (_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _))::_, expr_info, binop_info) :: xs 
    | BinaryOperator (stmt_info1, x::(CStyleCastExpr (_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _))::_, expr_info, binop_info) :: xs 
    | BinaryOperator (stmt_info1, x::(CallExpr (stmt_info, stmt_list, ei))::_, expr_info, binop_info) :: xs ->
      let (fp, _) = stmt_intfor2FootPrint stmt_info1 in 

    
      

      (match binop_info.boi_kind with
      | `Assign -> 


          
          let currentHandler = string_of_stmt x in 

          
          let rec checkIsGlobalVar str strLi : bool  = (* true means it is global *)
            match strLi with 
            | [] -> true 
            | x::xs -> (* x=  ptr, and str = ptr.filed *)
              if String.compare x str == 0  then false 
              else checkIsGlobalVar str xs
          in 

          let stateX = syh_compute_stmt_postcondition env current' future x in 

          
         (*
          print_endline ("=====\nCurrent handler root: " ^ (getRoot currentHandler)); 
          print_endline ("variablesInScope: " ^ List.fold_left (!variablesInScope) ~init:"" ~f:(fun acc a -> acc ^ "," ^ a)) ; 
          print_endline (string_of_bool (checkIsGlobalVar (getRoot currentHandler) !variablesInScope)); 
*)

          let rest = 
            if checkIsGlobalVar (getRoot currentHandler) !variablesInScope then 
              helper current' xs
            else 
              (let () = handlerVar := Some (currentHandler) in 
              helper current' ((Clang_ast_t.CallExpr (stmt_info, stmt_list, ei))::xs)) in 
          concatenateTwoEffectswithFlag stateX rest
       
         
      | `MulAssign | `DivAssign | `RemAssign | `AddAssign->

        print_endline ("MulAssign ...");
        helper current' xs 
      | `SubAssign | `ShlAssign | `ShrAssign | `AndAssign | `XorAssign
      | `OrAssign ->

        print_endline ("SubAssign ...");
        helper current' xs 
      | `EQ ->
  
          print_endline ("EQ ...");
          helper current' xs 
  

            
      | _ -> 
        print_endline ("rest ...");
        helper current' xs 
      )
 

    | DeclStmt (_, [x], handler::handlerRest):: xs  ->
      let _ = List.map (handler::handlerRest) ~f:(fun del -> 
        let localVar = (string_of_decl del) in 
        let () = variablesInScope := !variablesInScope @ [localVar] in 
        ()
      ) in 


          (
            match x with
            | DeclStmt _ ->  
              helper current' (x::xs)
      
            | _ ->          
            (*
            print_endline ("DeclStmt0 " ^  string_of_decl handler ^ " " ^ Clang_ast_proj.get_stmt_kind_string x ); 
*)
            let effectLi4X = syh_compute_stmt_postcondition env current' future x in 
            let effectRest = helper (concatenateTwoEffectswithFlag current' effectLi4X) xs in 
            concatenateTwoEffectswithFlag effectLi4X effectRest
          )

    | (IfStmt (stmt_info, [x;y;z], if_stmt_info)) :: xsifelse -> 
      let addTail (a:Clang_ast_t.stmt) = 
        match a with
        | CompoundStmt (c_stmt_info, c_stmt_list) ->  Clang_ast_t.CompoundStmt (c_stmt_info, List.append c_stmt_list xsifelse) 
        | _ ->  Clang_ast_t.CompoundStmt (stmt_info, a:: xsifelse)
      in 
      let statement' = Clang_ast_t.IfStmt (stmt_info, x::(List.map [y;z] ~f:(fun a -> addTail a)), if_stmt_info) in 
      syh_compute_stmt_postcondition env current' future statement'

    | DoStmt (stmt_info, [x;y])::xs  ->
      
      if String.compare (string_of_stmt y) "0" == 0 then helper current' xs
 
      else 
      
          (*
          print_endline ("DoStmt: ");
          let _ = List.map stmt_list ~f:(fun a-> print_string ((Clang_ast_proj.get_stmt_kind_string a)^", ")) in 
          print_endline ("");
          *)
        (let stmt' = List.append [x;y] xs in 
        helper current'  stmt')

    | LabelStmt (stmt_info, stmt_list, _)::xs
    | DoStmt (stmt_info, stmt_list)::xs 
    | WhileStmt (stmt_info, stmt_list)::xs -> 
      let stmt' = List.append stmt_list xs in 
      helper current'  stmt'
    | x :: xs -> 
      (*print_endline ("===================================");
      print_endline (List.fold_left (li) ~init:"" ~f:(fun acc a -> acc ^ ", " ^ (Clang_ast_proj.get_stmt_kind_string a)));
      *)
      let effectLi4X = syh_compute_stmt_postcondition env current' future x in 
      let new_history = (concatenateTwoEffectswithFlag current' effectLi4X) in 
      let effectRest = helper new_history xs in 
      concatenateTwoEffectswithFlag effectLi4X effectRest
    
      

  in 
  match instr with 
  | ReturnStmt (stmt_info, [ret]) ->
    (*print_endline ("ReturnStmt1:" ^ string_of_stmt_list [ret] " ");*)

    let (fp, _) = stmt_intfor2FootPrint stmt_info in 
    let fp1 = match fp with | [] -> None | x::_ -> Some x in 

    let optionTermToList inp = 
      match inp  with 
      | Some (Basic (BVAR t)) -> [(BVAR t)] 
      | _ -> [] 
    in 

    (match ret with
    | CallExpr (stmt_info, stmt_list, ei) ->

      let freshVar = verifier_getAfreeVar "r" in 
      let declRefExprStmt = constructADeclRefExprStmt stmt_info ei freshVar in 
      let returnStmt = Clang_ast_t.ReturnStmt(stmt_info, [declRefExprStmt]) in  
      let () = handlerVar := Some (freshVar) in 
      let stmt = Clang_ast_t.CompoundStmt (stmt_info, [ret;returnStmt]) in 
      syh_compute_stmt_postcondition env current future stmt

      
      
      
    | _ -> 
      let retTerm = stmt2Term ret in 
      let extrapure = 
        match retTerm with 
        | Some (Basic (BINT n)) -> Eq(Basic(BRET), Basic(BINT n))
        | Some (Basic (BVAR str)) -> Eq(Basic(BRET), Basic(BVAR str))
        | _ -> Ast_utility.TRUE
      in 
      let retTerm1 = optionTermToList retTerm in 

      let ev = 
        match future with
        | None -> 
          if List.length retTerm1 == 0 then Emp 
          else 
            let retTerm1 = List.map retTerm1 ~f:(fun a -> 
              match a with 
              | (BVAR str) -> (BVAR (getRoot str)) 
              | _ -> a 
              ) in
            Singleton ((("CONSUME", retTerm1)), fp1) 
        | Some _ -> Emp
      in 

      let es = Singleton (("RET", (retTerm1)), fp1) in 
      [(extrapure, Concatenate(ev, es), 1, fp)]
    )
  
  | ReturnStmt (stmt_info, stmt_list) ->
    (*print_endline ("ReturnStmt:" ^ string_of_stmt_list stmt_list " ");*)
    let (fp, _) = stmt_intfor2FootPrint stmt_info in 
    [(Ast_utility.TRUE, Emp, 1, fp)]
  
  | ForStmt (stmt_info, stmt_list)
  | UnaryOperator (stmt_info, stmt_list, _, _)
  | DefaultStmt (stmt_info, stmt_list) 
  | CaseStmt (stmt_info, stmt_list) 
  | CXXDependentScopeMemberExpr (stmt_info, stmt_list, _)  
  | CompoundStmt (stmt_info, stmt_list) -> 
    let (fp, _) = stmt_intfor2FootPrint stmt_info in 
    prefixLoction fp (helper current stmt_list)


  | IfStmt (stmt_info, stmt_list, if_stmt_info) ->
    (*print_endline ("IfElse:" ^ string_of_programStates current ^ "\n");
    *)


    let checkRelavent (conditional:  Clang_ast_t.stmt) : (((pure * (string list)) option))  = 
        (*print_string ("\n*****\ncheckRelavent: "); 
        print_string (string_of_varSet (!varSet));
        *)
        match stmt2Pure conditional with 
        | None -> (*print_string ("None; \n");*) None 
        | Some condition -> 
          
          (*
          print_endline (string_of_pure condition);
          print_endline (string_of_pure (Neg condition));
          *)
          

          let (varFromPure: string list) = varFromPure condition in 
          if twoStringSetOverlap varFromPure (!varSet) then 
          ((*print_string ("Yes; \n");*)
          Some (condition, varFromPure))
          else 
            ((*print_string ("None; \n");*)
            None )
    in 

    let extra = 
    (match stmt_list with 
    | x::ifelseRest -> 
      (match x with 
      | BinaryOperator (_, [(CallExpr (call_stmt_info, call_stmt_list, call_ei));y], _, _) 
      | BinaryOperator (_, BinaryOperator (_, [(CallExpr (call_stmt_info, call_stmt_list, call_ei));y], _, _)::_ , _, _) -> 
        let freshVar = verifier_getAfreeVar "r" in 
        let declRefExprStmt = constructADeclRefExprStmt call_stmt_info call_ei freshVar in 
        let stmtBinary = constructBinaryOperatorAssign call_stmt_info call_ei declRefExprStmt y in 
        let stmtCall = Clang_ast_t.CallExpr (call_stmt_info, call_stmt_list, call_ei) in 
        let stmtNewIFELSE = Clang_ast_t.IfStmt (stmt_info, (stmtBinary)::ifelseRest, if_stmt_info) in 
        let () = handlerVar := Some (freshVar) in 
        helper current ([stmtCall; stmtNewIFELSE])

      | _ -> 
      (match ifelseRest with 
      | [y] -> 
        let (locX, _) = maybeIntToListInt (getStmtlocation y) in 

        let (locY, locZ) = maybeIntToListInt (getStmtlocation y) in 

        (match checkRelavent x with 
        | None  -> 
          let eff4X = syh_compute_stmt_postcondition env current future x in
          let eff4Y = syh_compute_stmt_postcondition env current future y in
          let final = prefixLoction locX 
            (List.append 
            (postfixLoction locZ eff4X) 
            (prefixLoction locY (concatenateTwoEffectswithFlag eff4X eff4Y))) in 
          final


        | Some (condition, morevar) -> 

        (*let ()= varSet := (List.append !varSet morevar) in *)

        (*
        print_endline (string_of_pure condition);
        print_endline (string_of_pure (Neg condition));
*)
          let eff4X = syh_compute_stmt_postcondition env current future  x in
          let eff4Y = syh_compute_stmt_postcondition env current future  y in
          let res = prefixLoction locX 
            (List.append 
            (postfixLoction locZ (enforePure (Neg condition) eff4X))
            (prefixLoction locY (enforePure (condition) (concatenateTwoEffectswithFlag eff4X eff4Y)))) 
          in 
          res
        )
      | [y;z] -> 
        let (locX, _) = maybeIntToListInt (getStmtlocation y) in 

        let (locY, _) = maybeIntToListInt (getStmtlocation y) in 
        let (locZ, _) = maybeIntToListInt (getStmtlocation z) in 
      (*
      print_endline ("locY" ^ List.fold_left locY ~init:"" ~f:(fun acc a -> acc ^ " " ^ string_of_int a)); 
      print_endline ("locZ" ^ List.fold_left locZ ~init:"" ~f:(fun acc a -> acc ^ " " ^ string_of_int a)); 
      *)

        (match checkRelavent x with 
        | None  -> 
          let eff4X = syh_compute_stmt_postcondition env current future x in
          let eff4Y = syh_compute_stmt_postcondition env current future y in
          let eff4Z = syh_compute_stmt_postcondition env current future z in
          prefixLoction locX 
          (List.append 
            ((prefixLoction locZ (concatenateTwoEffectswithFlag eff4X eff4Z))) 
            (prefixLoction locY (concatenateTwoEffectswithFlag eff4X eff4Y)))

        | Some (condition, morevar) -> 

          let eff4X = syh_compute_stmt_postcondition env current future x in
          let eff4Y = syh_compute_stmt_postcondition env current future y in
          let eff4Z = syh_compute_stmt_postcondition env current future z in
          prefixLoction locX (List.append 
          (prefixLoction locZ (enforePure (Neg condition) (concatenateTwoEffectswithFlag eff4X eff4Z))) 
          (prefixLoction locY (enforePure (condition) (concatenateTwoEffectswithFlag eff4X eff4Y))))
        )
      | _ -> assert false 
      ))
    | _ -> assert false ) in 
    let final = extra in 
    final
    

  
  | ImplicitCastExpr (stmt_info, x::_, _, _, _) -> 
      let (fp, _) = stmt_intfor2FootPrint stmt_info in 
      prefixLoction fp (syh_compute_stmt_postcondition env current future x)
  | ArraySubscriptExpr(stmt_info, x::_, _)  
  | MemberExpr (stmt_info, x::_, _, _) -> 
    let (fp, _) =  getStmtlocation instr in 
    let varFromX = string_of_stmt x in 

    let ev = if twoStringSetOverlap [varFromX] (!varSet) then 
      Singleton ((("deref", [(BVAR(string_of_stmt x))])), fp) 
      else Emp
    in 
    let () = dynamicSpec := ((string_of_stmt instr, []), None, Some [(TRUE, ev )], None) :: !dynamicSpec in 

    let fp = match fp with | None -> [] | Some l -> [l] in 
    [(TRUE, ev, 0, fp)]

  | CallExpr _ -> helper current [instr]


  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
    let (fp, _) = stmt_intfor2FootPrint stmt_info in 

    

    (match binop_info.boi_kind with
    | `Assign -> 


    (*
    print_endline ("BinaryOperator0 " ^  string_of_stmt x ^ " " ^ Clang_ast_proj.get_stmt_kind_string y ); 
*)
      let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
      let (fp', _) = getStmtlocation instr in
      let varFromY = string_of_stmt y in 
      (*print_endline ("BinaryOperator CONSUME: " ^ varFromY);*) 

      let stateY = syh_compute_stmt_postcondition env current future y in 
      let stateX = syh_compute_stmt_postcondition env current future x in 


      let res  = 
        if twoStringSetOverlap [varFromY] (!varSet) then 
          (
          let ev = Singleton ((("CONSUME", [(BVAR(string_of_stmt y))])), fp') in 
          (Ast_utility.TRUE, ev, 0, fp))
        else 
          (Ast_utility.TRUE, Emp, 0, fp)
      in 

      concatenateTwoEffectswithFlag stateY (concatenateTwoEffectswithFlag stateX [res])

      
    | `And -> 
      helper current [x;y]

    | `Add -> 
      let stateX = syh_compute_stmt_postcondition env current future x in 
      let stateY = syh_compute_stmt_postcondition env current future y in 
      concatenateTwoEffectswithFlag stateX stateY


    | `PtrMemD | `PtrMemI | `Mul | `Div | `Rem | `Sub | `Shl | `Shr
    | `Cmp | `LT | `GT | `LE | `GE | `EQ | `NE | `Xor | `Or | `LAnd
    | `LOr -> 
      
      
      let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
      [(TRUE, Emp, 0, fp)]

    | _ -> 
      let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
      [(TRUE, Emp, 0, fp)]

  
        
    )


  | GotoStmt (stmt_infogoto, _, {Clang_ast_t.gsi_label= label_name; _}) ->
    let (fpGOTO, _) = stmt_intfor2FootPrint stmt_infogoto in 
    
    (*let rec find_stmtTillNextLable li acc =
      match li with
      | [] -> acc 
      | Clang_ast_t.LabelStmt _ :: _  -> acc 
      | x :: xs  -> find_stmtTillNextLable xs (acc@[x])
    in 
    *)
    let rec findTheLable stmtList = 
      match stmtList with 
      | [] -> []
      | Clang_ast_t.LabelStmt (stmt_infoLabel, stmt_list, label_name_1)::xs -> 
        if String.compare label_name_1 label_name == 0 then 
          let (fpLable, _) = stmt_intfor2FootPrint stmt_infoLabel in 
          match (fpGOTO, fpLable) with 
          | (a::_, b::_) -> 
            if a > b then []
            else 
            (
              let restStmt = xs (*find_stmtTillNextLable xs []*) in 
              print_endline ("=============");
              print_endline ("The stmt for are :"^ label_name);
              let _ = List.map (stmt_list@ restStmt) ~f:(fun a-> print_string ((Clang_ast_proj.get_stmt_kind_string a)^", ")) in 
              print_endline ("");
              stmt_list@ restStmt
            ) 
          | _ -> []
        else findTheLable xs 
      | _::xs -> findTheLable xs 
    in 
    let rec findStmt_ListByLable () = 
      match !currentModuleBody with 
      | None -> []
      | Some (Clang_ast_t.CompoundStmt (_, currentModuleStmts)) -> 
        findTheLable currentModuleStmts
      | _ -> []
    in 
    (*let exitsString li str =
      let temp = List.filter li ~f:(fun a -> if String.compare a str == 0 then true else false) in 
      if List.length temp > 5 then true 
      else false 
    in 
    if exitsString !currentLable label_name then 
      (let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
      [(TRUE, Emp, 0, fp)])
    else 
    *)
      (let () = currentLable := !currentLable @ [label_name] in  
      let stmt_list = findStmt_ListByLable () in 
      helper current stmt_list)

  (*
    | LabelStmt (stmt_info, stmt_list, label_name) ->
    labelStmt_trans trans_state stmt_info stmt_list label_name
 *)
  | ContinueStmt _
  | ConditionalOperator _
  | ParenExpr _ (* assert(max > min); *)
  | BreakStmt _ 
  | LabelStmt _ 
  | ImplicitCastExpr _ (*stmt_info, stmt_list, _, _, _*) 
  | MemberExpr _
  | NullStmt _
  | CharacterLiteral _ 
  | FixedPointLiteral _ 
  | FloatingLiteral _ 
  | IntegerLiteral _ 
  | StringLiteral _ 
  | RecoveryExpr _ 
  | DeclRefExpr _  
  | CStyleCastExpr _
  | WhileStmt _ 
  | ConstantExpr _ 
  (*| ForStmt _ *)
  | CXXOperatorCallExpr _ 
  | ArraySubscriptExpr _ 
  | InitListExpr _ 
  | CXXDeleteExpr _ (* delete g_logger_instance; *)
  (*| CStyleCastExpr _ *) (*  char *buf = (char * ) sw_malloc(n); *)
  | ExprWithCleanups _ (* *value = std::stoi(e);  *)
  | CXXConstructExpr _ (* va_list args; *)
  | CompoundAssignOperator _  (* retval += sw_snprintf(sw ...*)
  | CXXMemberCallExpr _ (* sw_logger()->put *)
  | DoStmt _ ->
    let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
    [(TRUE, Emp, 0, fp)]


  | DeclStmt (_, [], del::rest) -> 
    let _ = List.map ( del::rest) ~f:(fun del -> 
      let localVar = (string_of_decl del) in 
      let () = variablesInScope := !variablesInScope @ [localVar] in 
    ()) in 
    let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
    [(TRUE, Emp, 0, fp)]


      
  | SwitchStmt (_, _::x::_, _) -> 

    let rec decomposeSwitch stmt = 
      match stmt with 
      | Clang_ast_t.CompoundStmt (_, li) -> li
      | _ -> [stmt]
    in 
    let stmt_list = decomposeSwitch x in 


    let rec aux (acc:(Clang_ast_t.stmt list) list) (currentList:Clang_ast_t.stmt list) (li:Clang_ast_t.stmt list) : ((Clang_ast_t.stmt list) list) = 
      match li with 
      | [] -> List.append acc [currentList]
      | (CaseStmt a) :: xs -> aux (List.append acc [currentList]) [(Clang_ast_t.CaseStmt a)] xs 
      | (DefaultStmt a) :: xs -> aux (List.append acc [currentList]) [(Clang_ast_t.DefaultStmt a)] xs 
      | a :: xs -> aux acc ((currentList@[a])) xs 
    in 

    let stmt_list' = aux [] [] stmt_list in 


    (*
    print_string ("==> SwitchStmt: ");
    let _ = List.map stmt_list' ~f:(fun a-> 
      let _ = List.map a ~f:(fun b -> print_endline ((Clang_ast_proj.get_stmt_kind_string b)^",")) in 
      print_string ("\n ")) in 
    print_endline ("");
    *)

  
    let stateSummary = List.map stmt_list' ~f:(fun x -> helper current x) in 
    let res = flattenList stateSummary  in 
    (*print_endline ("Res for switch: \n" ^ string_of_programStates res);*)
    res



  | DeclStmt (_, _, handlers) -> 
    print_string ("DeclStmt handlers"); 

    let _ = List.map handlers ~f:(fun del -> 
      let localVar = (string_of_decl del) in 
      let () = variablesInScope := !variablesInScope @ [localVar] in 
      ()
    ) in 
    
    let (fp, _) = maybeIntToListInt (getStmtlocation instr) in 
    [(TRUE, Emp, 0, fp)]



    (*
    let (fp, fp1) =  (getStmtlocation instr) in 


    let ev = Singleton (((Clang_ast_proj.get_stmt_kind_string instr ^ " " ^ string_of_int (List.length stmt_list), [])), fp) in 
    let () = dynamicSpec := ((string_of_stmt instr, []), None, Some [(TRUE, ev )], None) :: !dynamicSpec in 

    let (fp, _) = maybeIntToListInt (fp, fp1) in 
    [(TRUE, ev, 0, fp)]
*)
    


  | UnaryExprOrTypeTraitExpr _

  | _ -> 
    let (fp, fp1) =  (getStmtlocation instr) in 
    let (fp', _) = maybeIntToListInt (fp, fp1) in 


    let ev = Singleton ((Clang_ast_proj.get_stmt_kind_string instr, []), fp) in 



    [(TRUE, ev, 0, fp')]



let syhtrim str =
  if String.compare str "" == 0 then "" else
  let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init
  in
  let len = String.length str in
  try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in
    String.sub str left (right - left + 1)
  with
  | Failure "empty" -> ""
;;

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (syhtrim line) :: input_lines file
  | _ -> assert false 
;;


let retriveLinesOfCode (source:string) : (int) = 
  let ic = open_in source in
  try
      let lines =  (input_lines ic ) in
      let rec helper (li:string list) = 
        match li with 
        | [] -> ""
        | x :: xs -> x ^ "\n" ^ helper xs 
      in       
      let line_of_code = List.length lines in 
      (line_of_code)


    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;

let show_effects_option (eff:effect option): string = 
  match eff with
  | None -> "None"
  | Some eff -> string_of_effect eff 
;;



let int_of_optionint intop = 
  match intop with 
  | None  -> (-1)
  | Some i -> i ;;



let reason_about_declaration (dec: Clang_ast_t.decl) (specifications: specification list) (source_Address:string): unit  = 

  match dec with
    | FunctionDecl (decl_info, named_decl_info, _, function_decl_info) ->
      let source_Addressnow = string_of_source_range  decl_info.di_source_range in 


      if String.compare source_Address source_Addressnow !=0 then ()
      else 
      let (l1, l2) = decl_info.di_source_range in 
      let (functionStart, functionEnd) = (int_of_optionint (l1.sl_line), int_of_optionint (l2.sl_line)) in 
      let () = currentFunctionLineNumber := (functionStart, functionEnd) in 
      (
      if functionEnd - functionStart > 285 then 
        (print_endline (string_of_int functionStart ^ " -- " ^ string_of_int functionEnd);
        ())
      else 
      match function_decl_info.fdi_body with 
      | None -> ()
      | Some stmt -> 
      let funcName = named_decl_info.ni_name in 
      let argumentNames = List.map (function_decl_info.fdi_parameters) ~f:(fun a -> string_of_decl a) in 
      let () = variablesInScope := argumentNames in 


      (*
            print_endline ("\nreasoning "^ funcName ^"\n" ); 
      print_endline (List.fold_left argumentNames ~init:"" ~f:(fun acc a -> acc ^ "," ^ a)) ; 

      *)

      let functionspec = findSpecFrom specifications funcName in 
      let (_, precondition, postcondition, futurecondition) = 
        match functionspec with
        | None -> ((funcName, []), None, None, None)
        | Some (sign, precondition, postcondition, futurecondition) 
          -> (sign, precondition, postcondition, futurecondition)
      in 

      let () = dynamicSpec := [] in 
      let () = varSet := [] in 
      
      let (defultPrecondition:programStates) = 
        match precondition with
        | None -> [(Ast_utility.TRUE, Emp (*Kleene (Any)*), 0, [])]
        | Some eff -> List.map eff ~f:(fun (p, es)->(p, es, 0, []))
      in 
      print_endline ("annalysing " ^ funcName);
      let () = currentModule := funcName in 
      let () = currentModuleBody := Some stmt in 
      let () = currentLable := [] in 

      let () = variablesInScope := [] in 
      let raw_final = (syh_compute_stmt_postcondition 
            specifications 
            defultPrecondition
            futurecondition  
            stmt) in 


      let (final:programStates) = 
          ((normaliseProgramStates
          raw_final)) in 


          (match final with 
          | [TRUE, Emp, _, _] -> ()
          | _ -> print_endline("\n=====> Actual effects of function: "^ funcName ^" ======>" );
               print_string (string_of_programStates final ^ "\n")) ;



      match postcondition with 
        | None -> () (* [(Ast_utility.TRUE, Kleene(Any))] *)
        | Some postcondition -> () 
          (* disabled the post condition checking *)
        (**
        let postcondition = match findReturnValueProgramStates final with 
          | None  -> postcondition
          | Some str -> 
            print_endline(str);
            let vb = [(str, BRET)] in 
            instantiateAugumentSome postcondition vb 
        in 
  
        print_string ("PostCondition" ^ string_of_effect postcondition ^ "\n") ;
        let final' = programStates2effectwithfootprintlist final in 
        let info = effectwithfootprintInclusion final' postcondition in 
        let extra_info = 
          "\n~~~~~~~~~ In function: "^ !currentModule ^" ~~~~~~~~~\n" ^
          "Post-condition checking: " in 
        print_endline (string_of_inclusion_results extra_info info); 

        let (error_paths, _, _, _) = info in 
        if List.length error_paths == 0 then ()
        else 
          (




          (let (head, patches) = program_repair info specifications in 
          if String.compare patches "" == 0 then 
          ()
          else 
            let () = finalReport := !finalReport ^ (string_of_inclusion_results extra_info info) in 
            let () = finalReport := !finalReport ^ head in 
            let () = finalReport := !finalReport ^ ("[Patches]\n ") ^ patches ^ "\n" in 
            ());

          ) 
        *) 

      )
    | _ -> () 
   

let retrive_basic_info_from_AST ast_decl: (string * Clang_ast_t.decl list * int) = 
    match ast_decl with
    | Clang_ast_t.TranslationUnitDecl (decl_info, decl_list, _, translation_unit_decl_info) ->
        let source =  translation_unit_decl_info.tudi_input_path in 
        let lines_of_code  = retriveLinesOfCode source in 
        (source, decl_list, lines_of_code) (*, specifications, lines_of_code, lines_of_spec, number_of_protocol *)
 
    | _ -> assert false




let retriveComments (source:string) : (string list) = 
  (*print_endline (source); *) 
  let partitions = Str.split (Str.regexp "/\*@") source in 
  (* print_endline (string_of_int (List.length partitions)); *)
  match partitions with 
  | [] -> assert false 
  | _ :: rest -> (*  SYH: Note that specification can't start from line 1 *)
  let partitionEnd = List.map rest ~f:(fun a -> Str.split (Str.regexp "@\*/")  a) in 
  let rec helper (li: string list list): string list = 
    match li with 
    | [] -> []
    | x :: xs  -> 
      (match List.hd x with
      | None -> helper xs 
      | Some head -> 
        if String.compare head "" ==0 then helper xs 
        else 
          let ele = ("/*@" ^ head ^ "@*/") in 
          (ele :: helper xs)  ) 
  in 
  let temp = helper partitionEnd in 
  temp
  


let retriveSpecifications (source:string) : (Ast_utility.specification list * int * int) = 
  let ic = open_in source in
  try
      let lines =  (input_lines ic ) in
      let rec helper (li:string list) = 
        match li with 
        | [] -> ""
        | x :: xs -> x ^ "\n" ^ helper xs 
      in 
      let line = helper lines in
      
      let partitions = retriveComments line in (*in *)
      let line_of_spec = List.fold_left partitions ~init:0 ~f:(fun acc a -> acc + (List.length (Str.split (Str.regexp "\n") a)))  in 
      (*
      if List.length partitions == 0 then ()
      else print_endline ("Global specifictaions are: ")); *)
      let user_sepcifications = List.map partitions 
        ~f:(fun singlespec -> 
          (*print_endline (singlespec ^ "\n");*)
          Parser.specification Lexer.token (Lexing.from_string singlespec)) in
      
      (*
      let _ = List.map sepcifications ~f:(fun (_ , pre, post, future) -> print_endline (string_of_function_sepc (pre, post, future) ) ) in 
      *)

      close_in ic ;                 (* 关闭输入通道 *)
      (user_sepcifications, line_of_spec, List.length partitions)

      (*
            flush stdin;                (* 现在写入默认设备 *)
      print_string (List.fold_left (fun acc a -> acc ^ forward_verification a progs) "" progs ) ; 
      *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;





let outputFinalReport str path = 

  let oc = open_out_gen [Open_append; Open_creat] 0o666 path in 
  try 
    Printf.fprintf oc "%s" str;
    close_out oc;
    ()

  with e ->                      (* 一些不可预见的异常发生 *)
    close_out_noerr oc;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
  ;; 




let do_source_file (translation_unit_context : CFrontend_config.translation_unit_context) ast =


  let which_system = if String.compare (String.sub (Sys.getcwd()) 0 5 ) "/home" == 0 then 1 else 0 in 
  let loris1_path = "/home/yahui/future_condition/infer_TempFix/"  in 
  let mac_path = "/Users/yahuis/Desktop/git/infer_TempFix/" in 
  let path = if which_system == 1  then loris1_path else mac_path  in 
  let (user_sepcifications, lines_of_spec, number_of_protocol) = retriveSpecifications (path ^ "spec.c") in 
  let output_report =  path ^ "TempFix-out/report.csv" in 
  let output_detail =  path ^ "TempFix-out/detail.txt" in 



  let tenv = Tenv.create () in
  CType_decl.add_predefined_types tenv ;
  init_global_state_capture () ;
  let source_file = translation_unit_context.CFrontend_config.source_file in
  let integer_type_widths = translation_unit_context.CFrontend_config.integer_type_widths in

  (*print_endline ("\n======================================================="); *)
  (*print_endline ("================ Here is Yahui's Code =================");*)


  let (source_Address, decl_list, lines_of_code) = retrive_basic_info_from_AST ast in
  
  let start = Unix.gettimeofday () in 

  let reasoning_Res = List.map decl_list  
    ~f:(fun dec -> reason_about_declaration dec user_sepcifications source_Address) in 
  
  let compution_time = (Unix.gettimeofday () -. start) in 


  (* Input program has  *)
(*
  let msg = 
    "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    ^ "[CURRENT REPORT]:"
    ^ source_Address ^ "\n"
    ^ string_of_int ( !totol_Lines_of_Code ) ^ " lines of code; " 
    ^ string_of_int lines_of_spec ^ " lines of specs; for " 
    ^ string_of_int (List.length user_sepcifications) ^ " protocols. "
    ^ "Analysis and repair took "^ string_of_float (compution_time)^ " seconds.\n\n"
    ^ string_of_int !totalAssertions ^ " total assertions, and " 
    ^ string_of_int !failedAssertions 
    ^ (if (!failedAssertions) == 1 then " is" else " are") 
    ^" failed; and " 
    ^  string_of_int !reapiredFailedAssertions 
    ^ (if (!reapiredFailedAssertions) == 1 then " is" else " are") 
    ^ " successfully repaired. \n"  
    ^ "Totol_execution_time: " ^ string_of_float !totol_execution_time 
    
    in    
*)

let analysisTime = compution_time -. !repairTime in 

  
let msg = 
    source_Address ^ ","
  ^ string_of_int ( lines_of_code + 1 ) ^ "," (*  lines of code;  *) 
  ^ string_of_int lines_of_spec ^ "," (*  lines of specs; *) 
  ^ string_of_int (List.length user_sepcifications) ^ "," (*  protocols.  *)
  ^ string_of_float (analysisTime)^ "," (* "Analysis took "^ , seconds.\n\n *)
  ^ string_of_float (!repairTime)^ "," (* "Repair took "^ , seconds.\n\n *)
  ^ string_of_int !failedAssertions ^ "," (* number fialed assertion *)
  ^ string_of_int !reapiredFailedAssertions ^ "\n" (* number successfully repaired. *)
  
  in 


  outputFinalReport (msg) output_report ; 
  (
  if String.compare !finalReport "" == 0  then ()
  else   
    (let () = finalReport := ("\nIn " ^ source_Address ^ ":\n") ^ !finalReport  in 
    outputFinalReport (!finalReport) output_detail)) ; 

  (*
  L.(debug Capture Verbose)
    "@\n Start building call/cfg graph for '%a'....@\n" SourceFile.pp source_file ;
  let cfg = compute_icfg translation_unit_context tenv ast in

  (*print_string("<<<SYH:Finished creating icfg>>>\n");*)


  CAddImplicitDeallocImpl.process cfg tenv ;
  CAddImplicitGettersSetters.process cfg tenv ;
  CReplaceDynamicDispatch.process cfg ;
  L.(debug Capture Verbose) "@\n End building call/cfg graph for '%a'.@\n" SourceFile.pp source_file ;
  SourceFiles.add source_file cfg (Tenv.FileLocal tenv) (Some integer_type_widths) ;
  if Config.debug_mode then Tenv.store_debug_file_for_source source_file tenv ;
  if
    Config.debug_mode || Config.testing_mode || Config.frontend_tests
    || Option.is_some Config.icfg_dotty_outfile
  then DotCfg.emit_frontend_cfg source_file cfg ;
  L.debug Capture Verbose "Stored on disk:@[<v>%a@]@." Cfg.pp_proc_signatures cfg ;
  *)
  
  ()
