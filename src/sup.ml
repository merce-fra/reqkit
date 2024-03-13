open Sup_types
(** converts a Time to int*)
let time_to_int t =
  match  t with
  | Time(v) -> v


(** prints en event *)
let rec print_event fmt e =
  match e with 
  | Var (s) -> Format.fprintf fmt "%s " s
  | Not (e) -> Format.fprintf fmt "Not(" ; print_event fmt e ; Format.fprintf fmt ")"
  | Or (e1,e2) -> Format.fprintf fmt "Or(" ; print_event fmt e1 ; Format.fprintf fmt " , " ; print_event fmt e2 ; Format.fprintf fmt ")"
  | And (e1,e2) -> Format.fprintf fmt "And(" ; print_event fmt e1 ; Format.fprintf fmt " , " ; print_event fmt e2 ; Format.fprintf fmt ")"
  | Constant(b) -> if b then  Format.fprintf fmt "True" else  Format.fprintf fmt "False" 
  | _ -> assert false (*this shall not happen as an exception is raised when matching un handled variants when creating the event*)
  
(** prints a trigger *)
let print_trigger fmt t =
  print_event fmt t.tse; Format.fprintf fmt ", ";
  print_event fmt t.tc; Format.fprintf fmt ", ";
  print_event fmt t.tee; Format.fprintf fmt ", ";
  Format.fprintf fmt  "%d, %d, " (time_to_int t.tmin) (time_to_int t.tmax) 
  
(** prints a delay *)
let print_delay fmt d =
  Format.fprintf fmt  "%d, %d, " (time_to_int d.lmin) (time_to_int d.lmax) 

(** prints an action *)
let print_action fmt a = 
  print_event fmt a.ase; Format.fprintf fmt ", ";
  print_event fmt a.ac; Format.fprintf fmt ", ";
  print_event fmt a.aee; Format.fprintf fmt ", ";
  Format.fprintf fmt  "%d, %d " (time_to_int a.amin) (time_to_int a.amax) 

(** print a SUP requirement *)
let print fmt (sup : sup_req) first =
  if not first then Format.fprintf fmt ",@\n";
  Format.fprintf fmt "[ ";
  print_trigger fmt sup.t;
  print_delay  fmt sup.d;
  print_action fmt sup.a;
  Format.fprintf fmt "]"


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
let rec convert_sup1 vars (intermediate_hashtbl :(string,string) Hashtbl.t) req  event_of_exp =
  let hold_after_one_tick = Ast_types.Holds_after_at_most(Ast_types.Real_const(0.1)) in

  let convert_sup_intermediate_variable intermediate_hashtbl req_var generated_var =
    (*if !req_var && !generated_var at t then at t+1 !generated_var *)
    let e1_ = event_of_exp (Ast_types.And(Ast_types.Not(req_var),Ast_types.Not(generated_var))) in
    let t = {tse=e1_; tc=e1_; tee=e1_; tmin = Time(0); tmax= Time(0)} in
    let d = {lmin=Time(1); lmax=Time(1)} in
    let a = {ase=Constant(true); ac=Constant(true); aee=event_of_exp(Ast_types.Not(generated_var)); amin=Time(0); amax=Time(0)} in
    let new_req = Ast_types.Globally(Ast_types.Always(Ast_types.If( Ast_types.Prop(req_var, Ast_types.Holds), Ast_types.Prop(generated_var, Ast_types.Holds_afterward)))) in      
    let (_ , res)   = convert_sup1 vars intermediate_hashtbl new_req event_of_exp in
    res @ [{t=t;d=d;a=a}] 
  in

  let convert_sup_toggle_intermediate_variable intermediate_hashtbl req_var_start_toggle req_var_end_toggle generated_var =
    (*if !req_var_start_toggle && !req_var_end_toggle && !generated_var at t then at t+1 !generated_var *)
    let e1_ = event_of_exp (Ast_types.And(Ast_types.And(Ast_types.Not(req_var_start_toggle),Ast_types.Not(req_var_end_toggle)),Ast_types.Not(generated_var))) in
    let t1 = {tse=e1_; tc=e1_; tee=e1_; tmin = Time(0); tmax= Time(0)} in
    let d = {lmin=Time(1); lmax=Time(1)} in
    let a1 = {ase=Constant(true); ac=Constant(true); aee=event_of_exp(Ast_types.Not(generated_var)); amin=Time(0); amax=Time(0)} in

    (*if !req_var_start_toggle && !req_var_end_toggle && generated_var at t then at t+1 generated_var *)
    let e2_ = event_of_exp (Ast_types.And(Ast_types.And(Ast_types.Not(req_var_start_toggle),Ast_types.Not(req_var_end_toggle)),generated_var)) in
    let t2 = {tse=e2_; tc=e2_; tee=e2_; tmin = Time(0); tmax= Time(0)} in
    let a2 = {ase=Constant(true); ac=Constant(true); aee=event_of_exp(generated_var); amin=Time(0); amax=Time(0)} in

    (*creates the requirements related to the toggle itself*)
    let  new_req1 = Ast_types.Globally(Ast_types.Always(Ast_types.If(Ast_types.Prop(Ast_types.And(req_var_start_toggle,Ast_types.Not(generated_var)), Ast_types.Holds),Ast_types.Toggles(req_var_start_toggle, generated_var, Ast_types.Holds)))) in
    let  new_req2 = Ast_types.Globally(Ast_types.Always(Ast_types.If(Ast_types.Prop(Ast_types.And(req_var_end_toggle,generated_var), Ast_types.Holds),Ast_types.Toggles(req_var_end_toggle, generated_var, Ast_types.Holds)))) in
    let (_ , res1)   = convert_sup1 vars intermediate_hashtbl new_req1 event_of_exp in
    let (_ , res2)   = convert_sup1 vars intermediate_hashtbl new_req2 event_of_exp in
    res1 @ res2 @ [{t=t1;d=d;a=a1};{t=t2;d=d;a=a2}]
  in

  let get_intermediate_var intermediate_hashtbl req_var =
    let i = Hashtbl.length intermediate_hashtbl in 
    let var_name = "intermediate"^(string_of_int i) in
    Hashtbl.add intermediate_hashtbl var_name "";
    match req_var with
    | first::[] -> (Ast_types.Var(var_name), convert_sup_intermediate_variable intermediate_hashtbl first (Ast_types.Var(var_name))) 
    | first::second::[] -> (Ast_types.Var(var_name), convert_sup_toggle_intermediate_variable  intermediate_hashtbl first second (Ast_types.Var(var_name))) 
    | _ -> assert false
  in 

  let convert_globally_always vars intermediate_hashtbl req =
    match req with
    | Ast_types.If( Ast_types.Prop(e1,Ast_types.Holds), r) ->
      begin
        let e1_ = event_of_exp e1 in
        let t = {tse=e1_; tc=e1_; tee=e1_; tmin = Time(0); tmax= Time(0)} in
        let d0 = { lmin=Time(0); lmax=Time(0)} in       
        let d1 = { lmin=Time(1); lmax=Time(1)} in       
        match r with
        | Ast_types.Prop(e2, Holds_for_at_least (e3)) -> 
              [{t=t;d=d0;a={ase=(event_of_exp e2); ac=(event_of_exp e2); aee=(event_of_exp e2); amin = Time(const_of_h vars (Holds_for_at_least (e3))); amax= Time(const_of_h vars (Holds_for_at_least (e3)))}}]
        | Ast_types.Prop(e2, Holds_after_at_most(e3)) -> 
              [{t=t;d=d0;a={ase=Constant(true); ac=Constant(true); aee=(event_of_exp e2); amin = Time(0); amax= Time(const_of_h vars (Holds_after_at_most (e3)))}}]
        | Ast_types.Prop(e2, Holds_for_less_than(e3)) ->
              [{t=t;d=d0;a={ase=Constant(true); ac=Constant(true); aee=(Not(event_of_exp e2)); amin = Time(0); amax= Time((const_of_h vars (Holds_for_less_than (e3)))-1)}}]
        | Ast_types.Prop(e2, Holds) ->
              [{t=t;d=d0;a={ase=(event_of_exp e2); ac=(event_of_exp e2); aee=(event_of_exp e2); amin = Time(0); amax= Time(0)}}]
        | Ast_types.Prop(e2, Holds_afterward) ->
              [{t=t;d=d1;a={ase=(event_of_exp e2); ac=(event_of_exp e2); aee=(event_of_exp e2); amin = Time(-1); amax= Time(-1)}}]
        | Ast_types.Toggles(e2, e3, Ast_types.At_most(h))->
            begin
              let t1 = And(event_of_exp e1, And(event_of_exp e2, event_of_exp e3)) in
              let d0 = {lmin=Time(0); lmax= Time(0)} in
              let a1 = Not(event_of_exp e3) in
              let t2 = And(event_of_exp e1, And(event_of_exp e2, Not(event_of_exp e3))) in
              let a2 = event_of_exp e3 in
              let amax = const_of_h vars (Ast_types.At_most(h)) in 
              [{t={tse=t1; tc=t1; tee=t1; tmin = Time(0); tmax= Time(0)}; 
                d=d0;
                a={ase=a1; ac=a1;aee=a1; amin = Time(0); amax= Time(amax)}
              };
              {t={tse=t2; tc=t2; tee=t2; tmin = Time(0); tmax= Time(0)}; 
                d=d0;
                a={ase=a2; ac=a2;aee=a2; amin = Time(0); amax= Time(amax)}
              }] 
            end
        | Ast_types.Toggles(e2, e3, Ast_types.Holds)->
          begin
            let t1 = And(event_of_exp e1, And(event_of_exp e2, event_of_exp e3)) in
            let a1 = Not(event_of_exp e3) in
            let t2 = And(event_of_exp e1, And(event_of_exp e2, Not(event_of_exp e3))) in
            let a2 = event_of_exp e3 in
            [{t={tse=t1; tc=t1; tee=t1; tmin = Time(0); tmax= Time(0)}; 
              d={lmin=Time(0); lmax= Time(0)};
              a={ase=a1; ac=a1;aee=a1; amin = Time(0); amax= Time(0)}
            };
            {t={tse=t2; tc=t2; tee=t2; tmin = Time(0); tmax= Time(0)}; 
              d={lmin=Time(0); lmax= Time(0)};
              a={ase=a2; ac=a2;aee=a2; amin = Time(0); amax= Time(0)}
            }] 
          end
        | Ast_types.Prop(e2,Ast_types.Previously_held) ->
          ( 
          let (intermediate_e2, reqs_intermediate_e2) = get_intermediate_var intermediate_hashtbl [e2] in
          let new_req2 = Ast_types.Globally(Ast_types.Always(Ast_types.If( Ast_types.Prop(e1, Ast_types.Holds), Ast_types.Prop(intermediate_e2, hold_after_one_tick)))) in
          let (_, res2) = convert_sup1 vars intermediate_hashtbl new_req2 event_of_exp in
          reqs_intermediate_e2 @ res2 
           )
        | _ -> raise (Invalid_argument "")

      end
    | Ast_types.Prop(e1, Holds) ->
      begin
        let e1_ = event_of_exp e1 in
      [{t={tse=Constant(true); tc=Constant(true); tee=Constant(true); tmin = Time(0); tmax= Time(0)};
        d={ lmin=Time(0); lmax=Time(0)};
        a={ase=Constant(true); ac=e1_; aee=Constant(true); amin = Time(-1); amax= Time(-1)}}]
      end
    | Ast_types.If (Ast_types.Prop(e1,Ast_types.Holds_for_at_least(e2)), Ast_types.Prop(e3,Ast_types.Holds_afterward) ) ->
        ( let tt = const_of_h vars (Ast_types.Holds_for_at_least(e2)) in 
          [{t={tse=(event_of_exp e1); tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(tt); tmax= Time(tt)}; 
              d={lmin=Time(1); lmax= Time(1)};
              a={ase=(event_of_exp e3); ac=(event_of_exp e3);aee=(event_of_exp e3); amin = Time(-1); amax= Time(-1)}
          }] )
    | Ast_types.If (Ast_types.Prop(e1,Ast_types.Holds_for_at_least(e2)), Ast_types.Prop(e3,Ast_types.Holds_afterward_for_at_least(e4)) ) ->
      ( let tt = const_of_h vars (Ast_types.Holds_for_at_least(e2)) in 
        let at = const_of_h vars (Ast_types.Holds_for_at_least(e4)) in 
        [{t={tse=(event_of_exp e1); tc=(event_of_exp e1); tee=(event_of_exp e1); tmin = Time(tt); tmax= Time(tt)}; 
            d={lmin=Time(1); lmax= Time(1)};
            a={ase=(event_of_exp e3); ac=(event_of_exp e3);aee=(event_of_exp e3); amin = Time(at); amax= Time(at)}
        }] )
    | Ast_types.If (Ast_types.Prop(e1,Ast_types.Holds_and_succeded_by(e2)), Ast_types.Prop(e3,Ast_types.Previously_held) ) ->
      (
        let (intermediate_e3, reqs_intermediate_e3) = get_intermediate_var intermediate_hashtbl [e3] in
        let (intermediate_e2, reqs_intermediate_e2) = get_intermediate_var intermediate_hashtbl [e2] in       
        let new_req3 = Ast_types.Globally(Ast_types.Always(Ast_types.Prop( Ast_types.And(Ast_types.Not(intermediate_e2),And(e1, intermediate_e3)), Ast_types.Holds))) in
        let (_, res3) = convert_sup1 vars intermediate_hashtbl new_req3 event_of_exp in
        reqs_intermediate_e2 @ reqs_intermediate_e3 @ res3
      )
  | _ -> raise (Invalid_argument "") in

  let convert_globally_never req = 
    match req with
    | Ast_types.Prop(e1, Holds) -> 
      let new_req1 = Ast_types.Globally(Ast_types.Always(Ast_types.Prop(Ast_types.Not(e1),Ast_types.Holds))) in
      let (_, res1) = convert_sup1 vars intermediate_hashtbl new_req1 event_of_exp in
      res1
    | _-> raise (Invalid_argument "") in

  let  convert_after_always e1 vars intermediate_hashtbl req =
    let (intermediate_e1, reqs_intermediate_e1) = get_intermediate_var intermediate_hashtbl [e1] in
    let new_req2 = (
      match req with
      | Ast_types.If(Ast_types.Prop(e2,Ast_types.Holds), r) ->
          Ast_types.Globally(Ast_types.Always(Ast_types.If( Ast_types.Prop( Ast_types.And(e2 , intermediate_e1), Ast_types.Holds), r)))  
      | Ast_types.If(Ast_types.Prop(e2,Ast_types.Holds_for_at_least(e3)), r) ->
          Ast_types.Globally(Ast_types.Always(Ast_types.If( Ast_types.Prop( Ast_types.And(e2 , intermediate_e1), Ast_types.Holds_for_at_least(e3)),r)))  
      | _-> raise (Invalid_argument "")
    ) in 
    let (_, res2) = convert_sup1 vars intermediate_hashtbl new_req2 event_of_exp in
    reqs_intermediate_e1 @ res2 in

    let convert_after_until_never e1 e2 vars intermediate_hashtbl req = 
      match req with 
      | Ast_types.Prop(_,Ast_types.Holds) -> begin
          let (intermediate_e1_e2, req_intermediate_e1_e2) = get_intermediate_var intermediate_hashtbl [e1;e2] in
          let  new_req3 = (
            match req with 
            | Ast_types.Prop(e3,Ast_types.Holds) ->
                Ast_types.Globally(Ast_types.Never( Ast_types.Prop( Ast_types.And(e3 , intermediate_e1_e2), Ast_types.Holds)))  
            | _ -> (raise (Invalid_argument "") ) )in 
          let (_, res3) = convert_sup1 vars intermediate_hashtbl new_req3 event_of_exp in
          req_intermediate_e1_e2 @ res3 
            end
      |  _ -> (raise (Invalid_argument "")  )
    in
    
  let convert_after_until_always e1 e2 vars intermediate_hashtbl req = 
    match req with 
    | Ast_types.If(Ast_types.Prop(_,Ast_types.Holds), _) 
    | Ast_types.Prop(_,Ast_types.Holds) -> begin
        let (intermediate_e1_e2, req_intermediate_e1_e2) = get_intermediate_var intermediate_hashtbl [e1;e2] in
        let  new_req3 = (
          match req with 
          | Ast_types.If(Ast_types.Prop(e3,Ast_types.Holds), r) ->
              Ast_types.Globally(Ast_types.Always(Ast_types.If( Ast_types.Prop( Ast_types.And(e3 , intermediate_e1_e2), Ast_types.Holds),  r)))  
          | Ast_types.Prop(e3,Ast_types.Holds) ->
              Ast_types.Globally(Ast_types.Always( Ast_types.Prop( Ast_types.And(e3 , intermediate_e1_e2), Ast_types.Holds)))  
          | _ -> (raise (Invalid_argument "2") ) )in 
        let (_, res3) = convert_sup1 vars intermediate_hashtbl new_req3 event_of_exp in
        req_intermediate_e1_e2 @ res3 
          end
    |  _ -> (raise (Invalid_argument "1")  )
  in

  let convert_before_always e1 vars intermediate_hashtbl req =
    let (intermediate_e1, reqs_intermediate_e1) = get_intermediate_var intermediate_hashtbl [e1] in
    let new_req2 = (match req with 
    | Ast_types.Prop(e2,Ast_types.Holds) ->
        Ast_types.Globally(Ast_types.Always(Ast_types.If(Ast_types.Prop( Ast_types.And(e1 , Ast_types.Not(intermediate_e1)), Ast_types.Holds),  Ast_types.Prop(e2,Ast_types.Holds))))
    | Ast_types.If( Ast_types.Prop(e2,Ast_types.Holds),r) ->
        Ast_types.Globally(Ast_types.Always(Ast_types.If(Ast_types.Prop(And(e2,Not(intermediate_e1)),Ast_types.Holds),r)))
    |_ -> raise (Invalid_argument "") )in
    let (_, res2) = convert_sup1 vars intermediate_hashtbl new_req2 event_of_exp in
    reqs_intermediate_e1 @ res2 in

  let convert_between_always e1 e2 vars intermediate_hashtbl req =
    match req with
    |  Ast_types.Prop(e3,Ast_types.Holds) -> (
      let (intermediate_e1_e2, req_intermediate_e1_e2) = get_intermediate_var intermediate_hashtbl [e1;e2] in
      let (intermediate_e3_e2, req_intermediate_e3_e2) = get_intermediate_var intermediate_hashtbl [e3 ;e2] in
      let new_req3 = Ast_types.Globally(Ast_types.Always(Ast_types.If(Ast_types.Prop(e2, Ast_types.Holds), Ast_types.Prop(Ast_types.And(intermediate_e1_e2, intermediate_e3_e2), Ast_types.Holds)))) in
      let (_, res3) = convert_sup1 vars intermediate_hashtbl new_req3 event_of_exp in
      req_intermediate_e1_e2 @ res3 @ req_intermediate_e3_e2 )
    (*|  Ast_types.If(Ast_types.Prop(e3,Ast_types.Holds), r) -> (
      let (intermediate_e3_e2, req_intermediate_e3_e2) = get_intermediate_var intermediate_hashtbl [e3 ;e2] in
      match r with
      | Ast_types.Prop(e4,Ast_types.Holds_after_at_most(e5)) ->
      | Ast_types.Prop(e4,Ast_types.Holds_for_at_least(e5)) ->
      | Ast_types.Prop(e4,Ast_types.Holds_for_less_than(e5)) ->
      | _-> raise (Invalid_argument "")
      let (_, res3) = convert_sup1 vars intermediate_hashtbl new_req3 in
      let (_, res7) = convert_sup1 vars intermediate_hashtbl new_req7 in
      (*res1 @*) res2 @ res3 @ res4 @ res5 @ res6 @ res7)*)
    | _-> raise (Invalid_argument "") in
   
  try 
    (* need to use a local hashtable because when an intermediate variable is generated, it is
       automatically added to the hashtable. However if the SUP is eventually not handled by
       the tool, it generates an intermediate variable is SMV file that is never used and
       the python tool bug. So if the SUP is handled, the new entries of the local hashtable are 
       copied into the global one *)
    let current_intermediate_hashtable = Hashtbl.copy intermediate_hashtbl in 
    Format.printf "Treating %s @." (Parse.print_req_as_string req);
    let res = (
      match req with
      | Ast_types.Globally( Ast_types.Always( r) ) -> convert_globally_always vars current_intermediate_hashtable r
      | Ast_types.Globally( Ast_types.Never(r)) -> convert_globally_never r
      | Ast_types.After(e1, Ast_types.Always( r)) -> convert_after_always  e1 vars current_intermediate_hashtable r
      | Ast_types.After_until( e1, e2, Ast_types.Always(r)) -> convert_after_until_always e1 e2 vars current_intermediate_hashtable r
      | Ast_types.After_until( e1, e2, Ast_types.Never(r)) -> convert_after_until_never e1 e2 vars current_intermediate_hashtable r
      | Ast_types.Before(e1, Ast_types.Always(r)) -> convert_before_always e1 vars current_intermediate_hashtable r
      | Ast_types.Between(e1, e2, Ast_types.Always(r)) -> convert_between_always e1 e2 vars current_intermediate_hashtable r
      | _-> raise (Invalid_argument ("This requirement is not supported in SUP conversion " ^ (Parse.print_req_as_string req))) 
    ) in 
        Hashtbl.iter (fun key value -> if (not (Hashtbl.mem intermediate_hashtbl key)) then Hashtbl.add intermediate_hashtbl key value) current_intermediate_hashtable;
       (req,res)
  with Invalid_argument _ -> raise (Invalid_argument ("This requirement is not supported in SUP conversion " ^ (Parse.print_req_as_string req))) 

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
    | Ast_types.Next_step(r) -> Ast_types.Next_step( aux_req r)
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
let print fmt sup_list first last = 
  if not first then Format.fprintf fmt ",";
  List.iteri (fun i sup -> ( print fmt sup (i = 0))) sup_list;
  if not last then Format.fprintf fmt "@\n"
  
module SMap = Map.Make(String)

(** converts an AST_types requirements list into a SUP requirement list*)
let of_req_with_non_bool parse_t = 
  let open Parse in
  Parse.print_vars parse_t;
  (* hash table used to handle some requirements that needs additionnal inputs to be converted into SUP*)
  let intermediated_hashtbl = Hashtbl.create 200 in
  let fmt = Format.get_err_formatter() in 
  let tmp = Hashtbl.fold (fun req_id req_content acc ->  begin
    try
     (req_id, convert_sup1 parse_t.vars intermediated_hashtbl req_content Ast_convert.vmt_event_of_exp) :: acc 
    with Invalid_argument msg -> (Format.fprintf fmt "WARNING : requirement with ID %s was not converted : %s @\n" req_id msg; acc)
  end) parse_t.reqs [] in 
  let bool_intermediate_variables = (List.of_seq (Hashtbl.to_seq_keys intermediated_hashtbl)) in
  (bool_intermediate_variables,SMap.of_list tmp)


(** converts an AST_types requirements list into a SUP requirement list*)
let of_req parse_t  =
  (* hash table used to handle generated variables that replaces non boolean expressions *)
  let generated_hashtbl = Hashtbl.create 200 in
  (* hash table used to handle some requirements that needs additionnal inputs to be converted into SUP*)
  let intermediated_hashtbl = Hashtbl.create 200 in
  let fmt = Format.get_err_formatter() in 
  let parse_t_with_only_bool = remove_non_bool_exp parse_t generated_hashtbl in
  let tmp = Hashtbl.fold (fun req_id req_content acc ->  begin
    try
     (req_id, convert_sup1 parse_t.vars intermediated_hashtbl req_content Ast_convert.sup_event_of_exp) :: acc 
    with Invalid_argument msg -> (Format.fprintf fmt "WARNING : requirement with ID %s was not converted : %s @\n" req_id msg; acc)
  end) parse_t_with_only_bool.reqs [] in 
  let bool_generated_variables = (List.fold_left (fun acc e -> (Parse.print_exp_as_string e)::acc) [] (List.of_seq (Hashtbl.to_seq_values generated_hashtbl))) in
  let bool_intermediate_variables = (List.of_seq (Hashtbl.to_seq_keys intermediated_hashtbl)) in
  (bool_generated_variables,bool_intermediate_variables,SMap.of_list tmp)
 

let generate_sup_file fmt (t:Parse.t)  =
  let list_initial_bool_variables = Parse.extract_bool_variables t.vars in
  (* and generateed ones + SUP requirements*)
  let (generated_variables, intermediate_variables, sup_reqs) = of_req t in
  (* print variables*)
  let all_variables = generated_variables@intermediate_variables@list_initial_bool_variables in
  Format.fprintf fmt "from z3 import *@\n";
  Format.fprintf fmt "ALPHA = 30@\n";
  Format.fprintf fmt "BETA = 10@\n";
  Format.fprintf fmt "MAX_PTRACE=20@\n";
  List.iter (fun v -> Format.fprintf fmt "%s = Bool('%s')@\n" v v) all_variables;
  (* print requirements *)
  Format.fprintf fmt "REQ_SET=[";
  let l = ( SMap.to_list sup_reqs) in
  List.iteri (fun i  (_ ,(_,sup_list ) ) -> print fmt sup_list (i=0) (i=((List.length l)-1))) l ;
  Format.fprintf fmt "]@\n"; 
  Format.fprintf fmt "COND_INIT = ["; 
  List.iteri (fun i v -> ( (if i>0 then Format.fprintf fmt ","); Format.fprintf fmt "%s == False" v )) intermediate_variables;
  Format.fprintf fmt "]@\n"
