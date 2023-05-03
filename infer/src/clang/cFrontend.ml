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
  print_string ("<<<SYH:cFrontend.compute_icfg>>>\n");
  print_string ("<<<SYH:look for -> cTrans.instruction_translate>>>\n");
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



let string_of_source_range ((s1, s2):Clang_ast_t.source_range) :string = 
  match (s1.sl_file, s2.sl_file) with 
  | (Some name, _) 
  | (_, Some name) -> name
  | (_, _) -> "none"


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

  | MemberExpr (stmt_info, stmt_list, _, member_expr_info) ->
    "MemberExpr " ^ string_of_stmt_list stmt_list " " 

  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    "IntegerLiteral " ^ integer_literal_info.ili_value

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

    "ParenExpr " ^ string_of_source_range  stmt_info.si_source_range
    ^ string_of_stmt_list stmt_list " " 

    
  | CStyleCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
  "CStyleCastExpr " ^ string_of_stmt_list stmt_list " " ^ ""


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

  | RecoveryExpr _ -> "RecoveryExpr"
  | BreakStmt _ -> "BreakStmt"


  | _ -> "not yet " ^ Clang_ast_proj.get_stmt_kind_string instr;;
(*  
  match stmt with 
| GotoStmt (stmt_info, _, {Clang_ast_t.gsi_label= label_name; _}) ->
  gotoStmt_trans trans_state stmt_info label_name
| LabelStmt (stmt_info, stmt_list, label_name) -> 
  labelStmt_trans trans_state stmt_info stmt_list label_name
| ArraySubscriptExpr (_, stmt_list, expr_info) -> 
  arraySubscriptExpr_trans trans_state expr_info stmt_list
  binaryOperator_trans_with_cond trans_state stmt_info stmt_list expr_info binop_info
| AtomicExpr (stmt_info, stmt_list, expr_info, atomic_info) ->
  atomicExpr_trans trans_state atomic_info stmt_info expr_info stmt_list
| CallExpr (stmt_info, stmt_list, ei) | UserDefinedLiteral (stmt_info, stmt_list, ei) ->
  callExpr_trans trans_state stmt_info stmt_list ei
| ConstantExpr (_, stmt_list, _) -> (
match stmt_list with
| [stmt] ->
    instruction_translate trans_state stmt
| stmts ->
    L.die InternalError "Expected exactly one statement in ConstantExpr, got %d"
      (List.length stmts) )
| CXXMemberCallExpr (stmt_info, stmt_list, ei) ->
  cxxMemberCallExpr_trans trans_state stmt_info stmt_list ei
| CXXOperatorCallExpr (stmt_info, stmt_list, ei) ->
  callExpr_trans trans_state stmt_info stmt_list ei
| CXXConstructExpr (stmt_info, stmt_list, expr_info, cxx_constr_info)
| CXXTemporaryObjectExpr (stmt_info, stmt_list, expr_info, cxx_constr_info) ->
  cxxConstructExpr_trans trans_state stmt_info stmt_list expr_info cxx_constr_info
    ~is_inherited_ctor:false
| CXXInheritedCtorInitExpr (stmt_info, stmt_list, expr_info, cxx_construct_inherited_expr_info)
->
  cxxConstructExpr_trans trans_state stmt_info stmt_list expr_info
    cxx_construct_inherited_expr_info ~is_inherited_ctor:true
| ObjCMessageExpr (stmt_info, stmt_list, expr_info, obj_c_message_expr_info) ->
  objCMessageExpr_trans trans_state stmt_info obj_c_message_expr_info stmt_list expr_info
| CompoundStmt (_, stmt_list) ->
  (* No node for this statement. We just collect its statement list*)
  compoundStmt_trans trans_state stmt_list
| ConditionalOperator (stmt_info, stmt_list, expr_info) ->
  (* Ternary operator "cond ? exp1 : exp2" *)
  conditionalOperator_trans trans_state stmt_info stmt_list expr_info
  ifStmt_trans trans_state stmt_info if_stmt_info
  switchStmt_trans trans_state stmt_info switch_stmt_info
| CaseStmt (stmt_info, stmt_list) ->
  caseStmt_trans trans_state stmt_info stmt_list
| DefaultStmt (stmt_info, stmt_list) ->
  defaultStmt_trans trans_state stmt_info stmt_list
| StmtExpr ({Clang_ast_t.si_source_range}, stmt_list, _) ->
  stmtExpr_trans trans_state si_source_range stmt_list
| ForStmt (stmt_info, [init; decl_stmt; condition; increment; body]) ->
  forStmt_trans trans_state ~init ~decl_stmt ~condition ~increment ~body stmt_info
| WhileStmt (stmt_info, [condition; body]) ->
  whileStmt_trans trans_state ~decl_stmt:None ~condition ~body stmt_info
| WhileStmt (stmt_info, [decl_stmt; condition; body]) ->
  whileStmt_trans trans_state ~decl_stmt:(Some decl_stmt) ~condition ~body stmt_info
  doStmt_trans trans_state ~condition ~body stmt_info
| CXXForRangeStmt (stmt_info, stmt_list) ->
  cxxForRangeStmt_trans trans_state stmt_info stmt_list
| ObjCForCollectionStmt (stmt_info, [item; items; body]) ->
  objCForCollectionStmt_trans trans_state item items body stmt_info
| NullStmt _ ->
  no_op_trans trans_state.succ_nodes
| CompoundAssignOperator (stmt_info, stmt_list, expr_info, binary_operator_info, _) ->
  binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list
  declRefExpr_trans trans_state stmt_info decl_ref_expr_info
| ObjCPropertyRefExpr (_, stmt_list, _, _) ->
  objCPropertyRefExpr_trans trans_state stmt_list
| CXXThisExpr (stmt_info, _, expr_info) ->
  cxxThisExpr_trans trans_state stmt_info expr_info
| OpaqueValueExpr (stmt_info, _, _, opaque_value_expr_info) ->
  opaqueValueExpr_trans trans_state opaque_value_expr_info
    stmt_info.Clang_ast_t.si_source_range
| PseudoObjectExpr (_, stmt_list, _) ->
  pseudoObjectExpr_trans trans_state stmt_list
  unaryExprOrTypeTraitExpr_trans trans_state unary_expr_or_type_trait_expr_info
| BuiltinBitCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _)
| CXXReinterpretCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
| CXXConstCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
| CXXStaticCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
| CXXFunctionalCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) ->
  cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_kind
| ObjCBridgedCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, objc_bridge_cast_ei) ->
  let objc_bridge_cast_kind = objc_bridge_cast_ei.Clang_ast_t.obcei_cast_kind in
  cast_exprs_trans trans_state stmt_info stmt_list expr_info ~objc_bridge_cast_kind cast_kind
  integerLiteral_trans trans_state expr_info integer_literal_info
| OffsetOfExpr (stmt_info, _, expr_info, offset_of_expr_info) ->
  offsetOf_trans trans_state expr_info offset_of_expr_info stmt_info
  stringLiteral_trans trans_state expr_info (String.concat ~sep:"" str_list)
| GNUNullExpr (_, _, expr_info) ->
  gNUNullExpr_trans trans_state expr_info
| CXXNullPtrLiteralExpr (_, _, expr_info) ->
  nullPtrExpr_trans trans_state expr_info
| ObjCSelectorExpr (_, _, expr_info, selector) ->
  objCSelectorExpr_trans trans_state expr_info selector
| ObjCEncodeExpr (_, _, expr_info, objc_encode_expr_info) ->
  objCEncodeExpr_trans trans_state expr_info objc_encode_expr_info
| ObjCProtocolExpr (_, _, expr_info, decl_ref) ->
  objCProtocolExpr_trans trans_state expr_info decl_ref
| ObjCIvarRefExpr (stmt_info, stmt_list, _, obj_c_ivar_ref_expr_info) ->
  objCIvarRefExpr_trans trans_state stmt_info stmt_list obj_c_ivar_ref_expr_info
  memberExpr_trans trans_state stmt_info stmt_list member_expr_info
  if
    is_logical_negation_of_int trans_state.context.CContext.tenv expr_info unary_operator_info
  then
    let conditional =
      Ast_expressions.trans_negation_with_conditional stmt_info expr_info stmt_list
    in
    instruction trans_state conditional
  else unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info
  returnStmt_trans trans_state stmt_info stmt_list
| ExprWithCleanups (stmt_info, stmt_list, _, _) ->
  exprWithCleanups_trans trans_state stmt_info stmt_list
  parenExpr_trans trans_state si_source_range stmt_list
| ObjCBoolLiteralExpr (_, _, expr_info, n)
| CXXBoolLiteralExpr (_, _, expr_info, n) ->
  characterLiteral_trans trans_state expr_info n
  floatingLiteral_trans trans_state expr_info float_string
| CXXScalarValueInitExpr (_, _, expr_info) ->
  cxxScalarValueInitExpr_trans trans_state expr_info
| ObjCBoxedExpr (stmt_info, stmts, info, boxed_expr_info) ->
  (* Sometimes clang does not return a boxing method (a name of function to apply), e.g.,
     [@("str")].  In that case, it uses "unknownSelector:" instead of giving up the
     translation. *)
  let sel =
    Option.value boxed_expr_info.Clang_ast_t.obei_boxing_method ~default:"unknownSelector:"
  in
  objCBoxedExpr_trans trans_state info sel stmt_info stmts
| ObjCArrayLiteral (stmt_info, stmts, expr_info, array_literal_info) ->
  objCArrayLiteral_trans trans_state expr_info stmt_info stmts array_literal_info
| ObjCDictionaryLiteral (stmt_info, stmts, expr_info, dict_literal_info) ->
  objCDictionaryLiteral_trans trans_state expr_info stmt_info stmts dict_literal_info
| ObjCStringLiteral (stmt_info, stmts, info) ->
  objCStringLiteral_trans trans_state stmt_info stmts info
  breakStmt_trans trans_state stmt_info
| ContinueStmt (stmt_info, _) ->
  continueStmt_trans trans_state stmt_info
| ObjCAtSynchronizedStmt (_, stmt_list) ->
  objCAtSynchronizedStmt_trans trans_state stmt_list
| ObjCIndirectCopyRestoreExpr (_, stmt_list, _) ->
  let control, returns =
    instructions Procdesc.Node.ObjCIndirectCopyRestoreExpr trans_state stmt_list
  in
  mk_trans_result (last_or_mk_fresh_void_exp_typ returns) control
| BlockExpr (stmt_info, _, expr_info, decl) ->
  blockExpr_trans trans_state stmt_info expr_info decl
| ObjCAutoreleasePoolStmt (stmt_info, stmts) ->
  objCAutoreleasePoolStmt_trans trans_state stmt_info stmts
| ObjCAtTryStmt (_, stmts) ->
  compoundStmt_trans trans_state stmts
| CXXTryStmt (stmt_info, try_stmts) ->
  tryStmt_trans trans_state stmt_info try_stmts
| CXXCatchStmt _ ->
  (* should by handled by try statement *)
  assert false
| ObjCAtThrowStmt (stmt_info, stmts) | CXXThrowExpr (stmt_info, stmts, _) ->
  objc_cxx_throw_trans trans_state stmt_info stmts
| ObjCAtFinallyStmt (_, stmts) ->
  compoundStmt_trans trans_state stmts
| ObjCAtCatchStmt _ ->
  compoundStmt_trans trans_state []
| PredefinedExpr (_, _, expr_info, _) ->
  stringLiteral_trans trans_state expr_info ""
| BinaryConditionalOperator (stmt_info, stmts, expr_info) ->
  binaryConditionalOperator_trans trans_state stmt_info stmts expr_info
| CXXNewExpr (stmt_info, _, expr_info, cxx_new_expr_info) ->
  cxxNewExpr_trans trans_state stmt_info expr_info cxx_new_expr_info
| CXXDeleteExpr (stmt_info, stmt_list, _, delete_expr_info) ->
  cxxDeleteExpr_trans trans_state stmt_info stmt_list delete_expr_info
| MaterializeTemporaryExpr (stmt_info, stmt_list, expr_info, _) ->
  materializeTemporaryExpr_trans trans_state stmt_info stmt_list expr_info
| CXXBindTemporaryExpr (stmt_info, stmt_list, expr_info, _) ->
  cxxBindTemporaryExpr_trans trans_state stmt_info stmt_list expr_info
| CompoundLiteralExpr (stmt_info, stmt_list, expr_info) ->
  compoundLiteralExpr_trans trans_state stmt_list stmt_info expr_info
| InitListExpr (stmt_info, stmts, expr_info) ->
  initListExpr_trans trans_state stmt_info expr_info stmts
| CXXDynamicCastExpr (stmt_info, stmts, _, _, qual_type, _) ->
  cxxDynamicCastExpr_trans trans_state stmt_info stmts qual_type
| CXXDefaultArgExpr (_, _, _, default_expr_info)
| CXXDefaultInitExpr (_, _, _, default_expr_info) ->
  cxxDefaultExpr_trans trans_state default_expr_info
| ImplicitValueInitExpr (stmt_info, _, _) ->
  implicitValueInitExpr_trans trans_state stmt_info
| GenericSelectionExpr (stmt_info, stmts, _, gse_info) -> (
match gse_info.gse_value with
| Some value ->
    instruction trans_state value
| None ->
    genericSelectionExprUnknown_trans trans_state stmt_info stmts )
| SizeOfPackExpr _ ->
  mk_trans_result (Exp.get_undefined false, StdTyp.void) empty_control
| GCCAsmStmt (stmt_info, stmts) ->
  gccAsmStmt_trans trans_state stmt_info stmts
| CXXPseudoDestructorExpr _ ->
  cxxPseudoDestructorExpr_trans ()
| CXXTypeidExpr (stmt_info, stmts, expr_info) ->
  cxxTypeidExpr_trans trans_state stmt_info stmts expr_info
| CXXStdInitializerListExpr (stmt_info, stmts, expr_info) ->
  cxxStdInitializerListExpr_trans trans_state stmt_info stmts expr_info
| LambdaExpr (stmt_info, _, expr_info, lambda_expr_info) ->
  let trans_state' = {trans_state with priority= Free} in
  lambdaExpr_trans trans_state' stmt_info expr_info lambda_expr_info
| AttributedStmt (stmt_info, stmts, attrs) ->
  attributedStmt_trans trans_state stmt_info stmts attrs
| TypeTraitExpr (_, _, expr_info, type_trait_info) ->
  booleanValue_trans trans_state expr_info type_trait_info.Clang_ast_t.xtti_value
| CXXNoexceptExpr (_, _, expr_info, cxx_noexcept_expr_info) ->
  booleanValue_trans trans_state expr_info cxx_noexcept_expr_info.Clang_ast_t.xnee_value
| VAArgExpr (_, [], expr_info) ->
  undefined_expr trans_state expr_info
| VAArgExpr (stmt_info, stmt :: _, ei) ->
  va_arg_trans trans_state stmt_info stmt ei
| ArrayInitIndexExpr _ | ArrayInitLoopExpr _ ->
  no_op_trans trans_state.succ_nodes
(* vector instructions for OpenCL etc. we basically ignore these for now; just translate the
 sub-expressions *)
| ObjCAvailabilityCheckExpr (_, _, expr_info, _) ->
  undefined_expr trans_state expr_info
| SubstNonTypeTemplateParmExpr (_, stmts, _) | SubstNonTypeTemplateParmPackExpr (_, stmts, _) ->
  let[@warning "-8"] [expr] = stmts in
  instruction trans_state expr
(* Infer somehow ended up in templated non instantiated code - right now
 it's not supported and failure in those cases is expected. *)
| CXXDependentScopeMemberExpr ({Clang_ast_t.si_source_range}, _, _) ->
  CFrontend_errors.unimplemented __POS__ si_source_range
    ~ast_node:(Clang_ast_proj.get_stmt_kind_string instr)
    "Translation of templated code is unsupported: %a"
    (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
    instr
| ForStmt ({Clang_ast_t.si_source_range}, _)
| WhileStmt ({Clang_ast_t.si_source_range}, _)
| DoStmt ({Clang_ast_t.si_source_range}, _)
| ObjCForCollectionStmt ({Clang_ast_t.si_source_range}, _) ->
  CFrontend_errors.incorrect_assumption __POS__ si_source_range "Unexpected shape for %a: %a"
    (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
    instr
    (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
    instr
| AddrLabelExpr _
| ArrayTypeTraitExpr _
| AsTypeExpr _
| CapturedStmt _
| ChooseExpr _
| CoawaitExpr _
| ConceptSpecializationExpr _
| ConvertVectorExpr _
| CoreturnStmt _
| CoroutineBodyStmt _
| CoyieldExpr _
| CUDAKernelCallExpr _
| CXXAddrspaceCastExpr _
| CXXFoldExpr _
| CXXRewrittenBinaryOperator _
| CXXUnresolvedConstructExpr _
| CXXUuidofExpr _
| DependentCoawaitExpr _
| DependentScopeDeclRefExpr _
| DesignatedInitExpr _
| DesignatedInitUpdateExpr _
| ExpressionTraitExpr _
| ExtVectorElementExpr _
| FunctionParmPackExpr _
| ImaginaryLiteral _
| IndirectGotoStmt _
| MatrixSubscriptExpr _
| MSAsmStmt _
| MSDependentExistsStmt _
| MSPropertyRefExpr _
| MSPropertySubscriptExpr _
| NoInitExpr _
| ObjCIsaExpr _
| ObjCSubscriptRefExpr _
| OMPArraySectionExpr _
| OMPArrayShapingExpr _
| OMPAtomicDirective _
| OMPBarrierDirective _
| OMPCancelDirective _
| OMPCancellationPointDirective _
| OMPCanonicalLoop _
| OMPCriticalDirective _
| OMPDepobjDirective _
| OMPDispatchDirective _
| OMPDistributeDirective _
| OMPDistributeParallelForDirective _
| OMPDistributeParallelForSimdDirective _
| OMPDistributeSimdDirective _
| OMPFlushDirective _
| OMPForDirective _
| OMPForSimdDirective _
| OMPGenericLoopDirective _
| OMPInteropDirective _
| OMPIteratorExpr _
| OMPMaskedDirective _
| OMPMaskedTaskLoopDirective _
| OMPMaskedTaskLoopSimdDirective _
| OMPMasterDirective _
| OMPMasterTaskLoopDirective _
| OMPMasterTaskLoopSimdDirective _
| OMPMetaDirective _
| OMPOrderedDirective _
| OMPParallelDirective _
| OMPParallelForDirective _
| OMPParallelForSimdDirective _
| OMPParallelGenericLoopDirective _
| OMPParallelMaskedDirective _
| OMPParallelMaskedTaskLoopDirective _
| OMPParallelMaskedTaskLoopSimdDirective _
| OMPParallelMasterDirective _
| OMPParallelMasterTaskLoopDirective _
| OMPParallelMasterTaskLoopSimdDirective _
| OMPParallelSectionsDirective _
| OMPScanDirective _
| OMPSectionDirective _
| OMPSectionsDirective _
| OMPSimdDirective _
| OMPSingleDirective _
| OMPTargetDataDirective _
| OMPTargetDirective _
| OMPTargetEnterDataDirective _
| OMPTargetExitDataDirective _
| OMPTargetParallelDirective _
| OMPTargetParallelForDirective _
| OMPTargetParallelForSimdDirective _
| OMPTargetParallelGenericLoopDirective _
| OMPTargetSimdDirective _
| OMPTargetTeamsDirective _
| OMPTargetTeamsDistributeDirective _
| OMPTargetTeamsDistributeParallelForDirective _
| OMPTargetTeamsDistributeParallelForSimdDirective _
| OMPTargetTeamsDistributeSimdDirective _
| OMPTargetTeamsGenericLoopDirective _
| OMPTargetUpdateDirective _
| OMPTaskDirective _
| OMPTaskgroupDirective _
| OMPTaskLoopDirective _
| OMPTaskLoopSimdDirective _
| OMPTaskwaitDirective _
| OMPTaskyieldDirective _
| OMPTeamsDirective _
| OMPTeamsDistributeDirective _
| OMPTeamsDistributeParallelForDirective _
| OMPTeamsDistributeParallelForSimdDirective _
| OMPTeamsDistributeSimdDirective _
| OMPTeamsGenericLoopDirective _
| OMPTileDirective _
| OMPUnrollDirective _
| PackExpansionExpr _
| ParenListExpr _
| RequiresExpr _
| SEHExceptStmt _
| SEHFinallyStmt _
| SEHLeaveStmt _
| SEHTryStmt _
| ShuffleVectorExpr _
| SourceLocExpr _
| SYCLUniqueStableNameExpr _
| TypoExpr _
| UnresolvedLookupExpr _
| UnresolvedMemberExpr _ -> 
  let (stmt_info, stmts), ret_typ =
    match Clang_ast_proj.get_expr_tuple instr with
    | Some (stmt_info, stmts, expr_info) ->
        let ret_typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
        ((stmt_info, stmts), ret_typ)
    | None ->
        let stmt_tuple = Clang_ast_proj.get_stmt_tuple instr in
        (stmt_tuple, StdTyp.void)
  in
  skip_unimplemented
    ~reason:
      (Printf.sprintf "unimplemented construct: %s, found at %s"
         (Clang_ast_proj.get_stmt_kind_string instr)
         (Clang_ast_j.string_of_source_range stmt_info.Clang_ast_t.si_source_range) )
    trans_state stmt_info ret_typ stmts

*)

  (*
  let record_li = 
    ["/Applications"; 
     "/Users/yahuis/Desktop/git/LightFTP/Source/gnutls";
     "/Users/yahuis/Desktop/git/LightFTP/Source/tinydtls"] in 
  let rec aux li:bool = 
    match li with 
    | [] ->  false 
    | x :: xs -> 
      (*print_string (str ^ "\n" ^ x ^ "\n" ^ string_of_int (String.compare (String.sub str 0 (String.length x)) x)^ "\n");
    *)
      if String.compare (String.sub str 0 (String.length x)) x  == 0 then 
      true else aux xs 
  in aux record_li
*)



let rec syh_compute_decl_pustcondition (decl: Clang_ast_t.decl) : es = 
  match decl with
  | VarDecl (_, ndi, qt, vdi) -> 
    Emp
  (* clang_prt_raw 1305- int, 901 - char *)
  | _ -> Emp

let rec dealwithBreakStmt (eff:es) (acc:es) : (es * bool) list = 
  match eff with 
  | Singleton ((str, _), _) -> 
    if String.compare str "BreakStmt" == 0 then [(acc, true)]
    else [(Concatenate (acc, eff), false)]
  | Bot   
  | Emp   
  | NotSingleton _ 
  | Any -> [(Concatenate (acc, eff), false )]
  | Concatenate (eff1, eff2) -> 
    let temp = dealwithBreakStmt eff1 acc in 
    let rec flatten li =
      match li with 
      | [] -> []
      | x :: xs -> List.append x (flatten xs)
    in 
    flatten (List.map temp ~f:(fun (acc1, b1) -> 
      (match b1 with 
      | false -> dealwithBreakStmt eff2 acc1
      | true -> [(acc1, b1)])
    ))
    
  | Disj (eff1, eff2) ->
    let temp1 = dealwithBreakStmt eff1 acc in 
    let temp2 = dealwithBreakStmt eff2 acc in 
    List.append temp1 temp2
  | Kleene effIn   -> 
    let temp = dealwithBreakStmt effIn Emp in 
    List.map temp ~f:(fun (acc1, b1) -> 
      (Concatenate(acc, Kleene(acc1)), false)
    )



let rec dealwithContinuekStmt (eff:es) (acc:es): (es * bool) list =
  match eff with 
  | Singleton ((str, _), _) -> 
    if String.compare str "ContinueStmt" == 0 then [(acc, true)]
    else [(Concatenate (acc, eff), false)]
  | Bot   
  | Emp   
  | NotSingleton _ 
  | Any -> [(Concatenate (acc, eff), false )]
  | Concatenate (eff1, eff2) -> 
    let temp = dealwithContinuekStmt eff1 acc in 
    let rec flatten li =
      match li with 
      | [] -> []
      | x :: xs -> List.append x (flatten xs)
    in 
    flatten (List.map temp ~f:(fun (acc1, b1) -> 
      (match b1 with 
      | false -> dealwithContinuekStmt eff2 acc1
      | true -> [(acc1, b1)])
    ))
    
  | Disj (eff1, eff2) ->
    let temp1 = dealwithContinuekStmt eff1 acc in 
    let temp2 = dealwithContinuekStmt eff2 acc in 
    List.append temp1 temp2
  | Kleene effIn   -> 
    let temp = dealwithContinuekStmt effIn Emp in 
    List.map temp ~f:(fun (acc1, b1) -> 
      (Concatenate(acc, Kleene(acc1)), false)
    )



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
  match instr with 
  | ImplicitCastExpr (_, x::_, _, _, _) 
  | MemberExpr (_, x::_, _, _) 
  | CStyleCastExpr (_, x::_, _, _, _) 
  | ParenExpr (_, x::_, _) -> stmt2Term x
  
  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
  (match binop_info.boi_kind with
  | `Add -> stmt2Term_helper "+" (stmt2Term x) (stmt2Term y) 
  | `Sub -> stmt2Term_helper "" (stmt2Term x) (stmt2Term y) 
  | _ -> None 
  )
  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    Some (Basic(BINT (int_of_string(integer_literal_info.ili_value))))

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
  | _ -> Some (Basic(BVAR(Clang_ast_proj.get_stmt_kind_string instr))) 


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

let concatenateTwoEffect (eff1:effect) (eff2:effect) : effect = 
  let (mixLi:(((pure * es) * (pure * es)) list)) = cartesian_product eff1 eff2 in 
  List.map mixLi ~f:(fun ((pi1, es1), (pi2, es2)) -> 
    let (trace:es)  = Concatenate(es1, es2) in 
    match (pi1, pi2) with
    | (TRUE, p2) -> (p2, trace)
    | (p1, TRUE) -> (p1, trace)
    | (_, _) -> (PureAnd (pi1, pi2), trace)
  )

let conjunctPure (pi1:pure) (pi2:pure): pure = 
  match (pi1, pi2) with
    | (TRUE, p2) -> (p2)
    | (p1, TRUE) -> (p1)
    | (_, _) -> (PureAnd (pi1, pi2))


let concatenateTwoEffectswithFlag (effectLi4X: programStates) (effectRest: programStates): programStates = 
  let mixLi = cartesian_product effectLi4X effectRest in 
  (*
  print_string (string_of_int (List.length effectLi4X) ^ 
  " x " ^  string_of_int (List.length effectRest)^ "\n");

  print_string (string_of_int (List.length mixLi) ^ "\n");
  *)

  let temp = List.map mixLi ~f:(
    fun ((pi1, eff_x, t_x, fp1),  (pi2, eff_y, t_y, fp2)) -> 
      if t_x > 0 then (pi1, eff_x, t_x, fp1)
      else
      (conjunctPure pi1 pi2, Concatenate (eff_x, eff_y),  t_y, List.append fp1 fp2)
  ) in 
  normaliseProgramStates temp
  
  
let enforePure (p:pure) (eff:programStates) : programStates = 
  List.map eff ~f:(fun (p1, es, f, fp) ->(PureAnd(p1, p), es, f, fp)) 


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
  (*print_endline (Clang_ast_proj.get_stmt_kind_string instr);
  *)

  match instr with 
  | BinaryOperator (stmt_info, x::y::_, expr_info, binop_info)->
    (match binop_info.boi_kind with
    | `LT -> stmt2Pure_helper "<" (stmt2Term x) (stmt2Term y) 
    | `GT -> stmt2Pure_helper ">" (stmt2Term x) (stmt2Term y) 
    | `GE -> stmt2Pure_helper ">=" (stmt2Term x) (stmt2Term y) 
    | `LE -> stmt2Pure_helper "<=" (stmt2Term x) (stmt2Term y) 
    | `EQ -> stmt2Pure_helper "=" (stmt2Term x) (stmt2Term y) 
    | `NE -> stmt2Pure_helper "!=" (stmt2Term x) (stmt2Term y) 
    | _ -> None 
    )

  | ImplicitCastExpr (_, x::_, _, _, _) -> stmt2Pure x
  | UnaryOperator (stmt_info, x::_, expr_info, op_info)->
    (match op_info.uoi_kind with
    | `Not -> 
      (match stmt2Pure x with 
      | None -> None 
      | Some p -> Some (Neg p))
    | _ -> None 
    )
  | ParenExpr (_, x::_, _) -> stmt2Pure x
  
  | _ -> None (* Some (Gt (Var(Clang_ast_proj.get_stmt_kind_string instr), Var (""))) *)


  
let prefixLoction (li: int list) (state:programStates) : programStates= 
  List.map state ~f:(fun (a, b, c, d) -> (a, b, c, List.append li d))

let getStmtlocation (instr: Clang_ast_t.stmt) : int option =
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
  | SwitchStmt (stmt_info, _, _)
  | CXXOperatorCallExpr (stmt_info, _, _)

  | CStyleCastExpr (stmt_info, _, _, _, _)  ->
    let (sl1, sl2) = stmt_info.si_source_range in 
    sl1.sl_line 

  | _ -> None 

let maybeIntToListInt l = match l with | None -> [] | Some l -> [l] 


let stmt_intfor2FootPrint (stmt_info:Clang_ast_t.stmt_info): int list = 
  let (sl1, sl2) = stmt_info.si_source_range in 
    let (lineLoc:int option) = sl1.sl_line in 
    maybeIntToListInt lineLoc


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
  | _ -> Clang_ast_proj.get_decl_kind_string decl


let rec var_binding (formal:string list) (actual: basic_type list) : bindings = 
  match (formal, actual) with 
  | (x::xs, v::ys) -> (x, v) :: (var_binding xs ys)
  | _ -> []
  ;;

let rec synthsisFromSpec (effect:(pure * es)) (env:(specification list)) : string option =  
  print_string ("synthsisFromSpec " ^ string_of_effect ([effect]) ^ "\n"); 
  let (pi, spec) = effect in 
  let spec =  normalise_es spec in 
  let patch = (match spec with 
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
        (* print_string (fName ^ "\n" ^ string_of_binary_tree  tree  ^ "\n"); *)

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
      | x :: xs  -> auc currectProof xs 
    in auc spec env) in 
    (match patch with 
    | None -> None 
    | Some fix -> 
       if entailConstrains TRUE pi == false then  
          Some ("if (" ^ string_of_pure (normalPure pi) ^ "){" ^ fix ^"}")
       else Some fix)


  
let computeAllthePointOnTheErrorPath (p1:pathList) (p2:pathList) : int list = 
  let correctDots = flattenList p1 in 
  let errorDots = flattenList p2 in 
  let rec helper li a : bool =
    match li with 
    | [] -> false 
    | x :: xs -> if x==a then true else helper xs a
  in 
  List.fold_left errorDots ~init:[] ~f:(fun acc a -> 
  if helper correctDots a then acc else List.append acc [a]) 


let program_repair (info:((error_info list) * binary_tree * pathList * pathList)) specifications : unit = 
  let (error_paths, tree, correctTraces, errorTraces) = info in 
  if List.length error_paths == 0 then ()
  else 
  let (error_lists:((pure * es * (int * int) * es) list)) = bugLocalisation error_paths in 
  print_endline ("\n<======[Bidirectional Bug Localisation & Possible Proof Repairs]======>\n\n[Repair Options] ");

  let rec helper li = 
      match li with 
      | [] -> ""
      | (pathcondition, realspec, _, spec):: res  -> 
      ( string_of_pure pathcondition ^ " /\\ " ^ 
        string_of_es realspec ^ " ~~~> " ^ string_of_es spec ) ^ ";\n" ^ helper res
  in 
  let msg = helper error_lists in 
  print_endline (msg);
  
  print_endline("[Patches] ");
  let rec auc li = 
      match li with 
      | [] -> ""
      | (pathcondition, realspec, (startNum ,endNum ),  spec):: res  -> 
        let onlyErrorPostions = computeAllthePointOnTheErrorPath correctTraces errorTraces in 
        let dotsareOntheErrorPath = List.filter onlyErrorPostions ~f:(fun x -> x >= startNum && x <=endNum) in 
        let (startNum, endNum) = 
          if List.length dotsareOntheErrorPath == 0 then (startNum, endNum)
          else List.fold_left dotsareOntheErrorPath ~init:(endNum, startNum) 
        ~f:(fun (min', max') x -> 
          if x < min' then (x, max')
          else if x > max' then (min', x)
          else (min', max')
          ) in 

        let startTimeStamp = Unix.gettimeofday() in
        let (specifications: specification list) = List.append specifications !dynamicSpec in 
        
        
        let list_of_functionCalls = synthsisFromSpec (pathcondition, spec) (specifications) in

        let startTimeStamp01 = Unix.gettimeofday() in

        ("@ line " ^ string_of_int startNum ^ " to line " ^  string_of_int endNum ^ 
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
          else  " can be inserted with code " ^  str ^ ".")
         ^ "\n\n" ^ auc res ^ "[Searching Time] " ^ string_of_float (startTimeStamp01 -. startTimeStamp)^ " seconds.\n"
        ) 
  in 
  let msg = auc error_lists in 
  print_endline (msg)

 

let rec syh_compute_stmt_postcondition (env:(specification list)) (current:programStates) 
(future:effect option) (instr: Clang_ast_t.stmt) : programStates = 

  (*
  print_endline ((Clang_ast_proj.get_stmt_kind_string instr));
  *)
  let rec helper current' (li: Clang_ast_t.stmt list): programStates  = 
    print_string ("==> helper: ");
    let _ = List.map li ~f:(fun a-> print_string ((Clang_ast_proj.get_stmt_kind_string a)^", ")) in 
    print_endline ("");
    

    match li with
    | [] -> [(TRUE, Emp, 0, [])]

    | DeclStmt (_, [CStyleCastExpr(_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _)], [del]):: xs  
    | DeclStmt (_, [(CallExpr (stmt_info, stmt_list, ei))], [del]) ::xs 
    | DeclStmt (_, [(ImplicitCastExpr (_, [(CallExpr (stmt_info, stmt_list, ei))], _, _, _))], [del]) ::xs ->

      let () = handlerVar := Some (string_of_decl del) in 
      helper current' ((Clang_ast_t.CallExpr (stmt_info, stmt_list, ei))::xs)

    | (CallExpr (stmt_info, stmt_list, ei)) ::xs -> 
      (*print_endline ("I am here call");*)
      
(* STEP 0: retrive the spec of the callee *)
      let fp = stmt_intfor2FootPrint stmt_info in 

      let ((calleeName, formalLi), prec, postc, futurec) = 
        match stmt_list with 
        | [] -> assert false  
        | x::rest -> 
          (match extractEventFromFUnctionCall x rest with 
          | None -> (("none", []), None, None, None)
          | Some (calleeName, acturelli) -> (* arli is the actual argument *)
            
            (*
            let () = print_string ("=========================\n") in 
            print_string (string_of_event (calleeName, acturelli) ^ ":\n");
            *)

            let spec = findSpecFrom env calleeName in 
            match spec with
            | None -> (("none", []), None, None, None)
            | Some ((signiture, formalLi), prec, postc, futurec)-> 
              if List.length acturelli == List.length formalLi then 
                let vb = var_binding formalLi acturelli in 
                ((signiture, formalLi), 
                instantiateAugument prec vb, 
                instantiateAugument postc vb, 
                instantiateAugument futurec vb)
                (* spec instantiation *)
              else 
              ((signiture, formalLi), prec, postc, futurec)
            
          )
      in 
      (*
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
            "~~~~~~~~~ In function: "^ !currentModule ^" ~~~~~~~~~\n" ^
            "Pre-condition checking for \'"^calleeName^"\': " in 
            print_endline (string_of_inclusion_results extra_info info); 
            program_repair info env; 

            
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

      let post' = 
        match postc with 
        | None -> current'  
        | Some postc -> 
          concatenateTwoEffectswithFlag current' (effects2programStates postc)
      in 


(* STEP 3: compute the effect for the rest code *)
  print_endline ("computing restSpec"  ^ string_of_int(List.length xs));

      let effectRest = helper (post') xs in
      
(* STEP 4: check the future spec of the callee *)
        (match futurec with 
        | None -> 
        (*print_endline ("None");*)

          (match postc with 
          | None ->  effectRest 
          | Some postc -> 
            concatenateTwoEffectswithFlag (effects2programStates postc) effectRest
          )
        | Some futurec -> 
          print_endline ("restSpec" ^ string_of_programStates effectRest);

          let restSpec = 
            match future with
            | None -> effectRest 
            | Some future -> concatenateTwoEffectswithFlag effectRest (effects2programStates future)
          in 

          
          print_endline ("futurec" ^ string_of_effect futurec);


          let info = effectwithfootprintInclusion (programStates2effectwithfootprintlist (normaliseProgramStates restSpec)) futurec in 
          let extra_info = "~~~~~~~~~ In function: "^ !currentModule ^" ~~~~~~~~~\nFuture-condition checking for \'"^calleeName^"\': " in 
          print_endline (string_of_inclusion_results extra_info info); 
          
          program_repair info env; 

          (match postc with 
          | None ->  effectRest 
          | Some postc -> 
            concatenateTwoEffectswithFlag (effects2programStates postc) effectRest
          )
        )  

    | DeclStmt (_, [x], _):: xs  
    | x ::xs -> 
      
      let effectLi4X = syh_compute_stmt_postcondition env current' future x in 
      let effectRest = helper (concatenateTwoEffectswithFlag current' effectLi4X) xs in 
      concatenateTwoEffectswithFlag effectLi4X effectRest

  in 
  match instr with 
  | ReturnStmt (stmt_info, [ret]) ->
    let extrapure = 
      match stmt2Term ret with 
      | Some (Basic (BINT n)) -> Eq(Basic(BVAR "ret"), Basic(BINT n))
      | Some (Basic (BVAR str)) -> Eq(Basic(BVAR "ret"), Basic(BVAR str))
      | _ -> Ast_utility.TRUE
    in 
    let fp = stmt_intfor2FootPrint stmt_info in 
    (*print_string ("Return: "); 
    let _ = List.map stmt_list ~f:(fun a -> 
      let temp = stmt2Term a in 
      match temp with 
      | None -> print_string ("none")
      | Some temp -> print_string (string_of_terms temp)
    ) in 
    print_string ("\n\n");
    *)
    [(extrapure, Emp, 1, fp)]
  
  | ReturnStmt (stmt_info, stmt_list) ->
    let fp = stmt_intfor2FootPrint stmt_info in 
    [(Ast_utility.TRUE, Emp, 1, fp)]
  

  | CompoundStmt (stmt_info, stmt_list) -> 
    let fp = stmt_intfor2FootPrint stmt_info in 
    prefixLoction fp (helper current stmt_list)


  | IfStmt (stmt_info, stmt_list, if_stmt_info) ->
    let fp = stmt_intfor2FootPrint stmt_info in 

    let checkRelavent (conditional:  Clang_ast_t.stmt) : (((pure* (string list)) option))  = 
        (*print_string ("\n*****\ncheckRelavent: "); 
        print_string (string_of_varSet (!varSet));
        *)
        match stmt2Pure conditional with 
        | None -> (*print_string ("None; \n");*) None 
        | Some condition -> 
          print_endline (string_of_pure condition);
          print_endline (string_of_pure (Neg condition));
          

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
    | [x; y] -> 
      let locY = maybeIntToListInt (getStmtlocation y) in 
      (match checkRelavent x with 
      | None  -> 
        let eff4X = syh_compute_stmt_postcondition env current future x in
        let eff4Y = syh_compute_stmt_postcondition env current future y in
        let final = prefixLoction fp 
          (List.append 
          (eff4X) 
          (prefixLoction locY (concatenateTwoEffectswithFlag eff4X eff4Y))) in 
        (*let () = print_string ("if else [x, y] None: " ^ string_of_programStates final^ "\n") in 
        *)
        final


      | Some (condition, morevar) -> 

        (*let ()= varSet := (List.append !varSet morevar) in *)
        let eff4X = syh_compute_stmt_postcondition env current future  x in
        let eff4Y = syh_compute_stmt_postcondition env current future  y in
        prefixLoction fp (List.append (enforePure (Neg condition) eff4X) 
        (prefixLoction locY (enforePure (condition) (concatenateTwoEffectswithFlag eff4X eff4Y))))
        )
    | [x;y;z] -> 
      let locY = maybeIntToListInt (getStmtlocation y) in 
      let locZ = maybeIntToListInt (getStmtlocation z) in 
      (match checkRelavent x with 
      | None  -> 
        let eff4X = syh_compute_stmt_postcondition env current future x in
        let eff4Y = syh_compute_stmt_postcondition env current future y in
        let eff4Z = syh_compute_stmt_postcondition env current future z in
        prefixLoction fp 
        (List.append 
          ((prefixLoction locZ (concatenateTwoEffectswithFlag eff4X eff4Z))) 
          (prefixLoction locY (concatenateTwoEffectswithFlag eff4X eff4Y)))

      | Some (condition, morevar) -> 
        (*let ()= varSet := (List.append !varSet morevar) in *)

        let eff4X = syh_compute_stmt_postcondition env current future x in
        let eff4Y = syh_compute_stmt_postcondition env current future y in
        let eff4Z = syh_compute_stmt_postcondition env current future z in
        prefixLoction fp (List.append 
        (prefixLoction locZ (enforePure (Neg condition) (concatenateTwoEffectswithFlag eff4X eff4Z))) 
        (prefixLoction locY (enforePure (condition) (concatenateTwoEffectswithFlag eff4X eff4Y))))
        )



    | _ -> assert false ) in 
    let final = extra in 
    (*print_string ("IfStmt:" ^ string_of_programStates final^"\n"); *)
    final
    

  

  



  | UnaryOperator _ (*stmt_info, stmt_list, _, _*)   
  | ImplicitCastExpr _ (*stmt_info, stmt_list, _, _, _*) 
  | BinaryOperator _ (*stmt_info, stmt_list, _, _*)
  | MemberExpr _
  | NullStmt _
  | CharacterLiteral _ 
  | FixedPointLiteral _ 
  | FloatingLiteral _ 
  | IntegerLiteral _ 
  | StringLiteral _ 
  | RecoveryExpr _ 
  | DeclRefExpr _  
  | DeclStmt (_, [], _) 
  | WhileStmt _ 
  | SwitchStmt _ 
  | ParenExpr _ (* assert(max > min); *)
  | ForStmt _ 
  | CallExpr _ (* nested calls:  if (swoole_timer_is_available()) {    *)
  | CXXOperatorCallExpr _ 
  | CXXDeleteExpr _ (* delete g_logger_instance; *)
  (*| CStyleCastExpr _ *) (*  char *buf = (char * ) sw_malloc(n); *)
  | ExprWithCleanups _ (* *value = std::stoi(e);  *)
  | CXXConstructExpr _ (* va_list args; *)
  | CompoundAssignOperator _  (* retval += sw_snprintf(sw ...*)
  | CXXMemberCallExpr _ (* sw_logger()->put *)
  | CXXConstructExpr _ (* va_list va_list; *)
  | DoStmt _ ->

    let (lineLoc:int option) = getStmtlocation instr in 
    (*[(TRUE, Singleton( "ret", lineLoc), 1)]*)
    let fp = match lineLoc with | None -> [] | Some l -> [l] in 

    [(TRUE, Emp, 0, fp)]


  | DeclStmt (_, stmt_list, _) -> 

    let ev = Singleton (((Clang_ast_proj.get_stmt_kind_string instr ^ " " ^ string_of_int (List.length stmt_list), [])), getStmtlocation instr) in 
    let () = dynamicSpec := ((string_of_stmt instr, []), None, Some [(TRUE, ev )], None) :: !dynamicSpec in 
    let (lineLoc:int option) = getStmtlocation instr in 

    let fp = match lineLoc with | None -> [] | Some l -> [l] in 
    [(TRUE, ev, 0, fp)]

    


  | ContinueStmt _
  | BreakStmt _
  | ParenExpr _
  | ArraySubscriptExpr _
  | UnaryExprOrTypeTraitExpr _
  | CStyleCastExpr _ 
  | _ -> 
    let ev = Singleton ((Clang_ast_proj.get_stmt_kind_string instr, []), getStmtlocation instr) in 
    let () = dynamicSpec := ((string_of_stmt instr, []), None, Some [(TRUE, ev )], None) :: !dynamicSpec in 
    let (lineLoc:int option) = getStmtlocation instr in 

    let fp = match lineLoc with | None -> [] | Some l -> [l] in 
    [(TRUE, ev, 0, fp)]



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
  

(* lines of code, lines of sepc, number_of_protocol *)
let retriveSpecifications (source:string) : (specification list * int * int * int) = 
  let ic = open_in source in
  try
      let lines =  (input_lines ic ) in
      let rec helper (li:string list) = 
        match li with 
        | [] -> ""
        | x :: xs -> x ^ "\n" ^ helper xs 
      in 
      let line = helper lines in
      
      let line_of_code = List.length lines in 
      let partitions = retriveComments line in (*in *)
      let line_of_spec = List.fold_left partitions ~init:0 ~f:(fun acc a -> acc + (List.length (Str.split (Str.regexp "\n") a)))  in 
      let sepcifications = List.map partitions 
        ~f:(fun singlespec -> 
          print_endline ("Global specifictaions are: ");
          print_endline (singlespec ^ "\n");
          Parser.specification Lexer.token (Lexing.from_string singlespec)) in
      
      (*
      let _ = List.map sepcifications ~f:(fun (_ , pre, post, future) -> print_endline (string_of_function_sepc (pre, post, future) ) ) in 
      *)

      (sepcifications, line_of_code, line_of_spec, List.length partitions)
      (*
      
      print_string (List.fold_left (fun acc a -> acc ^ forward_verification a progs) "" progs ) ; 
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *)
      *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;

let show_effects_option (eff:effect option): string = 
  match eff with
  | None -> "None"
  | Some eff -> string_of_effect eff 
;;



(* 



      (* 
      "[Pre  Condition] " ^ show_effects_option precondition ^"\n"^ 
      "[Post Condition] " ^ string_of_effect postcondition ^"\n"^ 
      "[Future Condition] " ^ show_effects_option futurecondition ^"\n"^ 
      "[Inferred Post Effects] " ^ string_of_programStates ( final)  ^"\n"^
      "[Inferring Time] " ^ string_of_float ((startTimeStamp01 -. startTimeStamp) *.1000000.0)^ " us" ^"\n" ^
      
      *)
              
^ 


        (*Clang_ast_proj.get_decl_kind_string dec *)
    (*| ObjCInterfaceDecl _ -> "ObjCInterfaceDecl"
    | ObjCProtocolDecl _ -> "ObjCProtocolDecl"
    | ObjCCategoryDecl _ -> "ObjCCategoryDecl"
    | ObjCCategoryImplDecl _ -> "ObjCCategoryImplDecl"
    | ObjCImplementationDecl _ -> "ObjCImplementationDecl"
    | CXXMethodDecl _ -> "CXXMethodDecl"
    | CXXConstructorDecl _ -> "CXXConstructorDecl"
    | CXXConversionDecl _ -> "CXXConversionDecl"
    | CXXDestructorDecl _ -> "CXXDestructorDecl"
    | VarDecl _ -> "VarDecl"
    | ClassTemplateSpecializationDecl _ -> "ClassTemplateSpecializationDecl"
    | CXXRecordDecl _ -> "CXXRecordDecl"
    | RecordDecl _ -> "RecordDecl"
    | _ -> "not yet" ^ Clang_ast_proj.get_decl_kind_string dec
    *)
*)


let reason_about_declaration (dec: Clang_ast_t.decl) (specifications: specification list) (source_Address:string): unit  = 

  match dec with
    | FunctionDecl (decl_info, named_decl_info, _, function_decl_info) ->
      let source_Addressnow = string_of_source_range  decl_info.di_source_range in 

      if String.compare source_Address source_Addressnow !=0 then ()
      else 
      
      (
      match function_decl_info.fdi_body with 
      | None -> ()
      | Some stmt -> 
      let funcName = named_decl_info.ni_name in 

      (* print_endline ("\nreasoning "^ funcName ^"\n" ); *)

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
        | None -> [(Ast_utility.TRUE, Kleene (Any), 0, [])]
        | Some eff -> List.map eff ~f:(fun (p, es)->(p, es, 0, []))
      in 
      let () = currentModule := funcName in 
      let (final:programStates) = 
          ((normaliseProgramStates
          (syh_compute_stmt_postcondition 
            specifications 
            defultPrecondition
            futurecondition  
            stmt))) in 

      (match final with 
      | [TRUE, Emp, _, _] -> ()
      | _ -> print_string("=====> Actual effects of function: "^ funcName ^" ======>\n" );
             print_string (string_of_programStates final ^ "\n")) ;

      

      match postcondition with 
        | None -> () (* [(Ast_utility.TRUE, Kleene(Any))] *)
        | Some postcondition -> 
        (
        let final = programStates2effectwithfootprintlist final in 
        let info = effectwithfootprintInclusion final postcondition in 
        let extra_info = 
          "~~~~~~~~~ In function: "^ !currentModule ^" ~~~~~~~~~\n" ^
          "Post-condition checking: " in 
        print_endline (string_of_inclusion_results extra_info info);
        program_repair info specifications; 
        ) 

      )
    | _ -> () 
   


let retrive_basic_info_from_AST ast_decl: (string * Clang_ast_t.decl list * specification list * int * int * int) = 
    match ast_decl with
    | Clang_ast_t.TranslationUnitDecl (decl_info, decl_list, _, translation_unit_decl_info) ->
        let source =  translation_unit_decl_info.tudi_input_path in 
        let (specifications, lines_of_code, lines_of_spec, number_of_protocol) = retriveSpecifications source in 
        (source, decl_list, specifications, lines_of_code, lines_of_spec, number_of_protocol)
 
    | _ -> assert false

let do_source_file (translation_unit_context : CFrontend_config.translation_unit_context) ast =
  print_string("<<<SYH:cFrontend.do_source_file>>>\n");

  let tenv = Tenv.create () in
  CType_decl.add_predefined_types tenv ;
  init_global_state_capture () ;
  let source_file = translation_unit_context.CFrontend_config.source_file in
  let integer_type_widths = translation_unit_context.CFrontend_config.integer_type_widths in

  print_endline ("\n=======================================================");
  print_endline ("================ Here is Yahui's Code =================");


  let (source_Address, decl_list, specifications, lines_of_code, lines_of_spec, number_of_protocol) = retrive_basic_info_from_AST ast in
  
  
  
  let () = totol_Lines_of_Spec := !totol_Lines_of_Spec + lines_of_spec in 

  let () = totol_Lines_of_Code := !totol_Lines_of_Code + lines_of_code in 
  let () = totol_specifications := List.append !totol_specifications specifications in 
  let start = Unix.gettimeofday () in 

  let reasoning_Res = List.map decl_list  
    ~f:(fun dec -> reason_about_declaration dec !totol_specifications source_Address) in 
  
  let compution_time = (Unix.gettimeofday () -. start) in 
  (* Input program has  *)
  let msg = 
    "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    ^ "[FINAL REPORT]:"
    ^ source_Address ^ "\n"
    ^ string_of_int ( !totol_Lines_of_Code ) ^ " lines of code;\n" 
    ^ string_of_int !totol_Lines_of_Spec ^ " lines of specs; for " 
    ^ string_of_int (List.length !totol_specifications)(*number_of_protocol*) ^ " protocols.\n"
    ^ "Analysis and repair took "^ string_of_float (compution_time)^ " seconds.\n\n"
    ^ string_of_int !totalAssertions ^ " total assertions, and " 
    ^ string_of_int !failedAssertions 
    ^ (if (!failedAssertions) == 1 then " is" else " are") 
    ^" failed; and " 
    ^  string_of_int !reapiredFailedAssertions 
    ^ (if (!reapiredFailedAssertions) == 1 then " is" else " are") 
    ^ " successfully repaired."  
    ^ "\nIn total, there are " 
    ^ string_of_int !proofObligations ^ " proof obligations, and "
    ^ string_of_int (!proofObligations - !failedProofObligations) 
    ^ (if ((!proofObligations - !failedProofObligations) ) == 1 then " is" else " are") ^" valid.\n"

    
    in 

  print_endline (msg); 


  let () = totol_execution_time := !totol_execution_time +. compution_time in 
  print_endline ("totol_execution_time: " ^ string_of_float !totol_execution_time); 
  print_endline ("\n============ Here is the end of Yahui's Code ============\n" 
                ^ "=========================================================\n");
  


  (*
  L.(debug Capture Verbose)
    "@\n Start building call/cfg graph for '%a'....@\n" SourceFile.pp source_file ;
  let cfg = compute_icfg translation_unit_context tenv ast in
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
