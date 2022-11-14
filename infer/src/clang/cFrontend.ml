(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
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


let string_of_source_location (s: Clang_ast_t.source_location):string = 
    match s.sl_file with 
  | Some name-> name ^ " "
  | None -> "none " 

let string_of_source_range ((s1, s2):Clang_ast_t.source_range) :string = 
  string_of_source_location s1 (*^ string_of_source_location s2 *)


let rec string_of_decl (dec :Clang_ast_t.decl) : string = 
  match dec with 
  | VarDecl (_, ndi, qt, vdi) -> 
    ndi.ni_name ^ "::" ^ Clang_ast_extend.type_ptr_to_string qt.qt_type_ptr
    ^" "^ (match vdi.vdi_init_expr with 
    | None -> ""
    | Some stmt -> string_of_stmt stmt)

    (* clang_prt_raw 1305- int, 901 - char *)
  | _ ->  Clang_ast_proj.get_decl_kind_string dec

and string_of_stmt (instr: Clang_ast_t.stmt) : string = 
  let rec helper_decl li sep = 
    match li with 
  | [] -> ""
  | [x] -> string_of_decl  x 
  | x::xs -> string_of_decl  x ^ sep ^ helper_decl xs sep
  in 
  let rec helper li sep = 
    match li with 
  | [] -> ""
  | [x] -> string_of_stmt x 
  | x::xs -> string_of_stmt x ^ sep ^ helper xs sep
  in 
  match instr with 
  | ReturnStmt (stmt_info, stmt_list) ->
    "ReturnStmt " ^ helper stmt_list " " ^ "\n"

  | IntegerLiteral (_, stmt_list, expr_info, integer_literal_info) ->
    integer_literal_info.ili_value

  | UnaryOperator (stmt_info, stmt_list, expr_info, unary_operator_info) ->
    "IntegerLiteral " ^ helper stmt_list " " ^ "\n"
  
  | ImplicitCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
    "" ^ helper stmt_list " " ^ "\n"

  | DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) ->
    "(DeclRefExpr)"^
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
    ^ helper stmt_list " " ^ "\n"

    
  | CStyleCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) -> 
  "CStyleCastExpr " ^ helper stmt_list " " ^ "\n"



    
  | CompoundStmt (_, stmt_list) -> helper stmt_list ";\n" 

  | BinaryOperator (stmt_info, stmt_list, expr_info, binop_info) -> 
    helper stmt_list (" "^ Clang_ast_proj.string_of_binop_kind binop_info.boi_kind ^" ")  ^"\n"

  | DeclStmt (stmt_info, stmt_list, decl_list) -> 
  "DeclStmt " (*  ^ helper stmt_list " " ^ "\n"^
    "/\\ " *) ^ string_of_int stmt_info.si_pointer^ " " ^ helper_decl decl_list " " ^ "\n" 
  
  | CallExpr (stmt_info, stmt_list, ei) -> 
    "CallExpr " ^  helper stmt_list " " 

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
| IfStmt (stmt_info, _, if_stmt_info) ->
  ifStmt_trans trans_state stmt_info if_stmt_info
| SwitchStmt (stmt_info, _, switch_stmt_info) ->
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
| DoStmt (stmt_info, [body; condition]) ->
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
| UnaryExprOrTypeTraitExpr (_, _, _, unary_expr_or_type_trait_expr_info) ->
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
| StringLiteral (_, _, expr_info, str_list) ->
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
| MemberExpr (stmt_info, stmt_list, _, member_expr_info) ->
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
| CharacterLiteral (_, _, expr_info, n)
| CXXBoolLiteralExpr (_, _, expr_info, n) ->
  characterLiteral_trans trans_state expr_info n
| FixedPointLiteral (_, _, expr_info, float_string)
| FloatingLiteral (_, _, expr_info, float_string) ->
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
| BreakStmt (stmt_info, _) ->
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
| RecoveryExpr _
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

let isLibFunction str : bool = 
  let record_li = 
    ["/Applications"; 
     "/Users/yahuis/Desktop/git/LightFTP/Source/gnutls"] in 
  let rec aux li:bool = 
    match li with 
    | [] ->  false 
    | x :: xs -> 
      (*print_string (str ^ "\n" ^ x ^ "\n" ^ string_of_int (String.compare (String.sub str 0 (String.length x)) x)^ "\n");
    *)
      if String.compare (String.sub str 0 (String.length x)) x  == 0 then 
      true else aux xs 
  in aux record_li



let do_source_file (translation_unit_context : CFrontend_config.translation_unit_context) ast =
  print_string("<<<SYH:cFrontend.do_source_file>>>\n");

  let tenv = Tenv.create () in
  CType_decl.add_predefined_types tenv ;
  init_global_state_capture () ;
  let source_file = translation_unit_context.CFrontend_config.source_file in
  let integer_type_widths = translation_unit_context.CFrontend_config.integer_type_widths in

  print_string ("\n=======================================================\n");
  print_string ("================ Here is Yahui's Code =================\n");
  let syh_pp_Clang_ast_t_decl ast_decl: string = 
    match ast_decl with
    | Clang_ast_t.TranslationUnitDecl (_, decl_list, _, _) ->
          (*match info.Clang_ast_t.tudi_input_kind with
          | `IK_C ->
              "CFrontend_config.C"
          | _ ->
              assert false*) 
          let rec helper (li:Clang_ast_t.decl list) = 
            match li with  
            | [] -> ""
            | dec::rest -> 
            (
              match dec with
              | FunctionDecl (decl_info, named_decl_info, _, function_decl_info) ->
                let source = string_of_source_range  decl_info.di_source_range in 
                if not (isLibFunction (source))  then 
                (print_string (named_decl_info.ni_name ^ ":\n");
                print_string (source ^ ":\n");
                match function_decl_info.fdi_body with 
                | None -> ""
                | Some stmt -> string_of_stmt stmt
                )
                else ""
              | _ -> "" (*Clang_ast_proj.get_decl_kind_string dec *)
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
            )  ^ helper rest 
          in helper decl_list
        (*
        let source_file = SourceFile.from_abs_path info.Clang_ast_t.tudi_input_path in
        init_global_state_for_capture_and_linters source_file ;

        let lang =
          match info.Clang_ast_t.tudi_input_kind with
          | `IK_C ->
              CFrontend_config.C
          | `IK_CXX ->
              CFrontend_config.CPP
          | `IK_ObjC ->
              CFrontend_config.ObjC
          | `IK_ObjCXX ->
              CFrontend_config.ObjCPP
          | _ ->
              assert false
        in
        let integer_type_widths =
          let widths = info.Clang_ast_t.tudi_integer_type_widths in
          { Typ.IntegerWidths.char_width= widths.itw_char_type
          ; short_width= widths.itw_short_type
          ; int_width= widths.itw_int_type
          ; long_width= widths.itw_long_type
          ; longlong_width= widths.itw_longlong_type }
        in
        let is_objc_arc_on = info.Clang_ast_t.tudi_is_objc_arc_on in
        {CFrontend_config.source_file; lang; integer_type_widths; is_objc_arc_on}*)
    | _ ->
        assert false
  in 

  print_string (syh_pp_Clang_ast_t_decl ast ^ "\n");

  print_string ("\n============ Here is the end of Yahui's Code ============\n");
  print_string ("=========================================================\n\n");


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
