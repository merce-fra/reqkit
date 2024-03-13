(** converts a requirements to SUP event*)
let rec sup_event_of_exp ast = 
  match ast with
  | Ast_types.Not( e ) -> Sup_types.Not (sup_event_of_exp e)
  | Ast_types.And (e1, e2) -> Sup_types.And (sup_event_of_exp e1, sup_event_of_exp e2)
  | Ast_types.Or (e1, e2) -> Sup_types.Or (sup_event_of_exp e1, sup_event_of_exp e2)
  | Ast_types.Bool_const(b)-> Sup_types.Constant(b)
  | Ast_types.Var(s) -> Sup_types.Var(s)
  | _ -> raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (Parse.print_exp_as_string ast)))



let rec vmt_event_of_exp ast = 
  match ast with
  | Ast_types.Not( e ) -> Sup_types.Not (vmt_event_of_exp e)
  | Ast_types.And (e1, e2) -> Sup_types.And (vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.Or (e1, e2) -> Sup_types.Or (vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.Bool_const(b)-> Sup_types.Constant(b)
  | Ast_types.Int_const(i)-> Sup_types.IntConstant(i)
  | Ast_types.Real_const(f)-> Sup_types.RealConstant(f)
  | Ast_types.Var(s) -> Sup_types.Var(s)
  | Ast_types.Plus(e1,e2) -> Sup_types.Plus(vmt_event_of_exp e1,vmt_event_of_exp e2)
  | Ast_types.Minus(e1,e2) -> Sup_types.Minus(vmt_event_of_exp e1,vmt_event_of_exp e2)
  | Ast_types.Multiply(e1,e2) -> Sup_types.Multiply(vmt_event_of_exp e1,vmt_event_of_exp e2)
  | Ast_types.Divide(e1,e2) -> Sup_types.Divide(vmt_event_of_exp e1,vmt_event_of_exp e2)
  | Ast_types.Eq(e1,e2) -> Sup_types.Eq(vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.NotEq(e1,e2) -> Sup_types.NotEq(vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.Geq(e1,e2) -> Sup_types.Geq(vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.Gt(e1,e2) -> Sup_types.Gt(vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.Leq(e1,e2) -> Sup_types.Leq(vmt_event_of_exp e1, vmt_event_of_exp e2)
  | Ast_types.Lt(e1,e2) -> Sup_types.Lt(vmt_event_of_exp e1, vmt_event_of_exp e2)
  | _ -> raise (Invalid_argument ("This node is not supported in VMT conversion " ^ (Parse.print_exp_as_string ast)))


  