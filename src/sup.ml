open Sup_types
(** converts a Time to int*)
let time_to_int t =
  match  t with
  | Time(v) -> v

(** prints en event *)
let rec print_event fmt e =
  match e with 
  | Var (s) -> Format.fprintf fmt "%s " s
  | Not (e) -> Format.fprintf fmt "!(" ; print_event fmt e ; Format.fprintf fmt ")"
  | Or (e1,e2) -> Format.fprintf fmt "(" ; print_event fmt e1 ; Format.fprintf fmt " || " ; print_event fmt e2 ; Format.fprintf fmt ")"
  | And (e1,e2) -> Format.fprintf fmt "(" ; print_event fmt e1 ; Format.fprintf fmt " && " ; print_event fmt e2 ; Format.fprintf fmt ")"
  | Eq(e1,e2) -> Format.fprintf fmt "(" ; print_event fmt e1 ; Format.fprintf fmt " == "; print_event fmt e2 ; Format.fprintf fmt ")"
  | NotEq(e1,e2) -> Format.fprintf fmt "(" ; print_event fmt e1 ; Format.fprintf fmt " != "; print_event fmt e2 ; Format.fprintf fmt ")"
  | Constant(b) -> if b then  Format.fprintf fmt "true" else  Format.fprintf fmt "false" 
  
(** prints a trigger *)
let print_trigger fmt t =
  print_event fmt t.tse; Format.fprintf fmt " ";
  print_event fmt t.tc; Format.fprintf fmt " ";
  print_event fmt t.tee; Format.fprintf fmt " ";
  Format.fprintf fmt  "%d %d " (time_to_int t.tmin) (time_to_int t.tmax) 
  
(** prints a delay *)
let print_delay fmt d =
  Format.fprintf fmt  "%d %d " (time_to_int d.lmin) (time_to_int d.lmax) 

(** prints an action *)
let print_action fmt a = 
  print_event fmt a.ase; Format.fprintf fmt " ";
  print_event fmt a.ac; Format.fprintf fmt " ";
  print_event fmt a.aee; Format.fprintf fmt " ";
  Format.fprintf fmt  "%d %d " (time_to_int a.amin) (time_to_int a.amax) 

(** print a SUP requirement *)
let print fmt (sup : sup_req) =
  print_trigger fmt sup.t;
  print_delay  fmt sup.d;
  print_action fmt sup.a

(** converts a requirements to SUP*)
let rec event_of_exp ast = 
  match ast with
  | Ast_types.Not( e ) -> Not (event_of_exp e)
  | Ast_types.And (e1, e2) -> And (event_of_exp e1, event_of_exp e2)
  | Ast_types.Or (e1, e2) -> Or (event_of_exp e1, event_of_exp e2)
  | Ast_types.Eq (e1, e2) -> Eq (event_of_exp e1, event_of_exp e2)
  | Ast_types.NotEq (e1, e2) -> NotEq (event_of_exp e1, event_of_exp e2)
  | Ast_types.Bool_const(b)-> Constant(b)
  | Ast_types.Var(s) -> Var(s)
  | _ -> raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (Parse.print_exp_as_string ast)))

(** check if a variable of name [s] is in the variable declaration hashtable.
    If so and if constant, the value is returned with a 10 multiplicator for SUP Time infos *)
let get_value_from_tbl (vars : (string,Ast_types.declaration) Hashtbl.t) (s: string) =
  if (Hashtbl.mem vars s) then
    begin
      let v =  Hashtbl.find vars s in 
        match v with
        | Constant ( _, Const_int (i)) -> 10*i
        | Constant ( _,Const_real (f)) -> int_of_float(10.0 *. f)
        | _->  raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (Parse.print_declaration_as_string v)))   
    end
  else
   raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (s)))   

(** converts a hold expression into an integer usable for Time *)
let const_of_h vars ast : int =
  match ast with
  | Ast_types.Holds_for_at_least (v)
  | Ast_types.Holds_afterward_for_at_least (v)
  | Ast_types.Holds_for_less_than (v) 
  | Ast_types.At_most (v)
  | Ast_types.Holds_after_at_most (v) ->   
    begin
      match v with
      | Ast_types.Int_const (i)-> 10*i
      | Ast_types.Real_const (f) -> int_of_float(10.0 *. f)
      | Ast_types.Var (s) -> get_value_from_tbl vars s  
      | _ ->  raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (Parse.print_exp_as_string v)))        
    end
  | _ -> raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (Parse.print_hold_as_string ast)))

(** converts Ast_types requirements into SUP requirements. Due to the
    semantic differences, an Ast_types requirements may be expressed with 
    several SUPs *)
let convert_sup1 vars req =
  ( req, 
    match req with
    | Ast_types.Globally( Ast_types.Always( Ast_types.If( Ast_types.Prop(e1,Ast_types.Holds), Ast_types.Prop(e2, Holds_for_at_least (e3))))) ->
        [{t={tse=(event_of_exp e1) ; tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(0); tmax= Time(0)}; 
          d={lmin=Time(0); lmax= Time(0)};
          a={ase=Constant(true); ac=Constant(true); aee=(event_of_exp e2); amin = Time(0); amax= Time(const_of_h vars (Holds_for_at_least (e3)))} 
        }] 
    | Ast_types.Globally( Ast_types.Always( Ast_types.If( Ast_types.Prop(e1,Ast_types.Holds), Ast_types.Prop(e2, Holds_after_at_most(e3)) ))) ->
      [{t={tse=(event_of_exp e1) ; tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(0); tmax= Time(0)}; 
        d={lmin=Time(0); lmax= Time(0)};
        a={ase=Constant(true); ac=Constant(true); aee=(event_of_exp e2); amin = Time(0); amax= Time(const_of_h vars (Holds_after_at_most (e3)))}
      };
      {t={tse=(event_of_exp e1) ; tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(0); tmax= Time(0)}; 
        d={lmin=Time(0); lmax= Time(0)};
        a={ase=Constant(true); ac=Constant(true); aee=(Not(event_of_exp e2)); amin =Time(1+const_of_h vars (Holds_after_at_most (e3))); amax = Time(-1)}
      }]
    | Ast_types.Globally( Ast_types.Never( Ast_types.Prop(e1, Holds) )) ->
      [{t={tse=(Not(event_of_exp e1)) ; tc=(Not(event_of_exp e1)); tee=(Not(event_of_exp e1)); tmin = Time(0); tmax= Time(0)}; 
        d={lmin=Time(0); lmax= Time(0)};
        a={ase=Constant(true); ac=Constant(true); aee=Constant(true); amin = Time(0); amax= Time(-1)}
      }]
    | Ast_types.Globally( Ast_types.Always(Ast_types.If( Ast_types.Prop(e1,Ast_types.Holds), Ast_types.Prop(e2, Holds_for_less_than(e3))))) ->
      [{t={tse=(event_of_exp e1); tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(0); tmax= Time(0)}; 
        d={lmin=Time(0); lmax= Time(0)};
        a={ase=Constant(true); ac=Constant(true);aee=(event_of_exp e2); amin = Time(0); amax= Time( (const_of_h vars (Holds_for_less_than (e3)) -1))}
      }]
    | Ast_types.Globally( Ast_types.Always(Ast_types.If( Ast_types.Prop(e1,Ast_types.Holds), Ast_types.Prop(e2, Holds)))) ->
      [{t={tse=(event_of_exp e1); tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(0); tmax= Time(0)}; 
        d={lmin=Time(0); lmax= Time(0)};
        a={ase=Constant(true); ac=Constant(true);aee=(event_of_exp e2); amin = Time(0); amax= Time(1)}
      }] 
    | Ast_types.Globally( Ast_types.Always( Ast_types.Prop(e1,Ast_types.Holds))) ->
      [{t={tse=(event_of_exp e1); tc=Constant(true); tee=Constant(true); tmin = Time(0); tmax= Time(0)}; 
        d={lmin=Time(0); lmax= Time(0)};
        a={ase=Constant(true); ac=Constant(true);aee=Constant(true); amin = Time(0); amax= Time(-1)}
      }] 
    | Ast_types.Globally( Ast_types.Always( Ast_types.If( Ast_types.Prop(e1,Ast_types.Holds), Ast_types.Toggles(e2, e3, Ast_types.At_most(h))) )) ->
        let t1 = And(event_of_exp e1, And(event_of_exp e2, event_of_exp e3)) in
        let a1 = Not(event_of_exp e3) in
        let t2 = And(event_of_exp e1, And(event_of_exp e2, Not(event_of_exp e3))) in
        let a2 = event_of_exp e3 in
        let amax = const_of_h vars (Ast_types.At_most(h)) in 
        [{t={tse=t1; tc=t1; tee=t1; tmin = Time(0); tmax= Time(0)}; 
          d={lmin=Time(0); lmax= Time(0)};
          a={ase=a1; ac=a1;aee=a1; amin = Time(0); amax= Time(amax)}
        };
        {t={tse=t2; tc=t2; tee=t2; tmin = Time(0); tmax= Time(0)}; 
          d={lmin=Time(0); lmax= Time(0)};
          a={ase=a2; ac=a2;aee=a2; amin = Time(0); amax= Time(amax)}
        }] 
    | _-> raise (Invalid_argument ("This node is not supported in SUP conversion " ^ (Parse.print_req_as_string req))) 
  )

(** replace in the hash table the non boolean expressions *)
let replace_hashtbl vars hashtbl new_exp =
  let s = Hashtbl.length hashtbl in
  let var_name = "generated"^string_of_int(s) in
  let var_content = Parse.print_exp_as_string new_exp in
  let var_decl =  Ast_types.Input (var_name, Ast_types.Bool) in
  Hashtbl.add vars var_name var_decl;
  if Hashtbl.mem hashtbl var_content then 
    Hashtbl.find hashtbl var_content
  else
    (Hashtbl.add hashtbl var_content (Ast_types.Var(var_name)); Ast_types.Var(var_name))

(** extracts and replaces the non boolean expression of an Ast_types requirement by boolean expression
    Some info can be lost in the conversion *)
let extract_non_bool_exp vars hashtbl req_content = 
  let rec aux_req ast  = 
    match ast with
    | Ast_types.Prop (e, h) ->   Ast_types.Prop (aux_exp e, aux_hold h ) 
    | Ast_types.Globally (r) -> Ast_types.Globally(aux_req r)
    | Ast_types.Always(r) -> Ast_types.Always(aux_req r)
    | Ast_types.Never(r) ->  Ast_types.Always(aux_req r)
    | Ast_types.After (e, r) ->  Ast_types.After(aux_exp e, aux_req r)
    | Ast_types.After_at_most (r, e) -> Ast_types.After_at_most(aux_req r, aux_exp e)
    | Ast_types.Before (e, r )-> Ast_types.Before(aux_exp e, aux_req r)
    | Ast_types.Between (e1, e2, r) -> Ast_types.Between(aux_exp e1, aux_exp e2, aux_req r)
    | Ast_types.After_until (e1, e2, r) -> Ast_types.After_until(aux_exp e1, aux_exp e2, aux_req r)
    | Ast_types.If(r1,r2) -> Ast_types.If(aux_req r1, aux_req r2)
    | Ast_types.Toggles(e1, e2, h) -> Ast_types.Toggles( aux_exp e1, aux_exp e2, aux_hold h)
  and aux_exp ast  =
    match ast with
    | Plus (_,_) 
    | Minus (_, _) 
    | Multiply (_, _)
    | Divide (_,_) -> ast
    | Lt (e1,e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in replace_hashtbl vars hashtbl (Lt(e1p,e2p)) 
    | Gt (e1,e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in replace_hashtbl vars hashtbl (Gt(e1p,e2p)) 
    | Leq (e1,e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in replace_hashtbl vars hashtbl (Leq(e1p,e2p)) 
    | Geq (e1, e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in replace_hashtbl vars hashtbl (Geq(e1p,e2p)) 
    | Eq (e1, e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in replace_hashtbl vars hashtbl (Eq(e1p,e2p)) 
    | NotEq (e1, e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in replace_hashtbl vars hashtbl (NotEq(e1p,e2p)) 
    | Or(e1,e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in Or(e1p,e2p) 
    | And (e1,e2) -> let e1p = aux_exp e1 in let e2p =aux_exp e2 in And(e1p,e2p) 
    | Not (e) -> let ep = aux_exp e in Not(ep) 
    | _ -> ast
  and aux_hold ast  =
    match ast with
    | Empty
    | Holds 
    | Holds_afterward
    | Previously_held -> ast
    | Holds_for_at_least (e) -> Holds_for_at_least (aux_exp e)
    | Holds_after_at_most(e) -> Holds_after_at_most(aux_exp e)
    | Holds_afterward_for_at_least (e) -> Holds_afterward_for_at_least (aux_exp e)
    | Holds_for_less_than (e) -> Holds_for_less_than(aux_exp e)
    | Holds_at_list_every (e) -> Holds_at_list_every(aux_exp e)
    | Holds_and_succeded_by(e) -> Holds_and_succeded_by( aux_exp e )
    | At_most (e) -> At_most( aux_exp e )
  in aux_req req_content

(** converts all non boolean expression to boolean ones. If several
    identical expressions appears in the same or different Ast_types
    requirements, the same boolean expression is used *)
let remove_non_bool_exp (parse_t : Parse.t) hashtbl = 
  let req_hashtable = Hashtbl.create 200 in
  Hashtbl.iter (fun req_id req_content  ->  Hashtbl.add req_hashtable req_id (extract_non_bool_exp parse_t.vars hashtbl req_content) ) parse_t.reqs;
  let res : Parse.t = {vars = parse_t.vars; reqs = req_hashtable} in
  res
  
(** prints a list of SUP requirements *)
let print fmt sup_list = 
  List.iter (fun sup -> ( print fmt sup; Format.fprintf fmt "@\n")) sup_list

(** converts an AST_types requirements list into a SUP requirement list*)
let of_req parse_t  =
  let hashtbl = Hashtbl.create 200 in
  let parse_t_with_only_bool = remove_non_bool_exp parse_t hashtbl in
  Hashtbl.fold (fun _ req_content acc ->  (convert_sup1 parse_t.vars req_content) :: acc ) parse_t_with_only_bool.reqs [] 
 
