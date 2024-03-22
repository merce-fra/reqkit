(** [generate_counter fmt] generates the time counter that is used for initialization in the formatter [fmt]*)
let generate_counter fmt =
  Format.fprintf fmt ";this is the global time counter@\n";
  Format.fprintf fmt "(declare-fun t () Int)@\n";
  Format.fprintf fmt "(declare-fun tn () Int)@\n";
  Format.fprintf fmt "(define-fun .sv0 () Int (! t :next tn))@\n";
  Format.fprintf fmt "(define-fun .init () Bool (! (= t 1) :init true))@\n";
  Format.fprintf fmt "(define-fun .trans () Bool (! (= tn (+ t 1)) :trans true))@\n";
  Format.fprintf fmt "(define-fun .p0 () Bool (! (> t 0) :invar-property 0))@\n"

(** [generate_state fmt] defines the states list in the formatter [fmt]*)
let generate_state fmt =
  Format.fprintf fmt ";this is the states for SUP@\n";
  Format.fprintf fmt "(define-fun IDLE () Int 0)@\n";
  Format.fprintf fmt "(define-fun TRIG () Int 1)@\n";
  Format.fprintf fmt "(define-fun DELAY () Int 2)@\n";
  Format.fprintf fmt "(define-fun ACTION () Int 3)@\n";
  Format.fprintf fmt "(define-fun ERR () Int 4)@\n"

(** [time_to_string t] convert the time [t] to string, for now only integer time units *)
let time_to_string t =
  match  t with
  | Sup_types.Time(v) -> string_of_int v
  
(** current invariant unique id *)
let invariant_index = ref 0

(** [generate_invariant fmt content_start content_end] generates an invariant with a unique id in the formatter [fmt]
    [content_start] is what is before the invar property definition and [content_end] is what is after*)
let generate_invariant fmt content_start content_end =
  invariant_index := !invariant_index + 1;
  Format.fprintf fmt "%s@\n" (content_start ^ " :invar-property " ^ (string_of_int !invariant_index) ^content_end)

(** [generate_SUP_content fmt sup_index req_name tmin tmax lmin lmax amin amax] generates the states machine for the state and counter of the SUP 
    with index [sup_index] and name [req_name] in the formatter [fmt]
    The SUP trigger start event is [tmin]
    The SUP trigger end event is [tmax]
    The SUP delay min is [lmin]
    The SUP delay max is [lmax]
    The SUP action start event is [amin]
    The SUP action end event is [amax]
     *)
let generate_SUP_content fmt sup_index req_name tmin tmax lmin lmax amin amax=
  let state_name = "state_"^req_name^"_"^(string_of_int sup_index) in
  let counter_name = "c_"^req_name^"_"^(string_of_int sup_index) in
  let tse_name = "tse_"^req_name^"_"^(string_of_int sup_index) in
  let tee_name = "tee_"^req_name^"_"^(string_of_int sup_index) in
  let tc_name = "tc_"^req_name^"_"^(string_of_int sup_index) in
  let tmin_name = "tmin_"^req_name^"_"^(string_of_int sup_index) in
  let tmax_name = "tmax_"^req_name^"_"^(string_of_int sup_index) in
  let lmin_name = "lmin_"^req_name^"_"^(string_of_int sup_index) in
  let lmax_name = "lmax_"^req_name^"_"^(string_of_int sup_index) in
  let ase_name = "ase_"^req_name^"_"^(string_of_int sup_index) in
  let aee_name = "aee_"^req_name^"_"^(string_of_int sup_index) in
  let ac_name = "ac_"^req_name^"_"^(string_of_int sup_index) in
  let amin_name = "amin_"^req_name^"_"^(string_of_int sup_index) in
  let amax_name = "amax_"^req_name^"_"^(string_of_int sup_index) in
  
  let tmin_s = time_to_string tmin in
  let tmax_s = time_to_string tmax in
  let lmin_s = time_to_string lmin in
  let lmax_s = time_to_string lmax in
  let amin_s = time_to_string amin in
  let amax_s = time_to_string amax in

  Format.fprintf fmt ";these are the function to access SUP attributes (state, time counter, trigger/delay/action time@\n";
  Format.fprintf fmt "(declare-fun %s () Int)@\n" state_name;
  Format.fprintf fmt "(declare-fun %s_n () Int)@\n" state_name;
  Format.fprintf fmt "(declare-fun %s () Int)@\n" counter_name;
  Format.fprintf fmt "(declare-fun %s_n () Int)@\n" counter_name;
  Format.fprintf fmt "(define-fun %s () Int %s)@\n" tmin_name tmin_s;
  Format.fprintf fmt "(define-fun %s () Int %s)@\n" tmax_name tmax_s;
  Format.fprintf fmt "(define-fun %s () Int %s)@\n" lmin_name lmin_s;
  Format.fprintf fmt "(define-fun %s () Int %s)@\n" lmax_name lmax_s;
  Format.fprintf fmt "(define-fun %s () Int %s)@\n" amin_name amin_s;
  Format.fprintf fmt "(define-fun %s () Int %s)@\n" amax_name amax_s;
  Format.fprintf fmt "@\n";

  Format.fprintf fmt ";these are the function to explicit SUP time counter initial value and transition@\n";
  Format.fprintf fmt ";for counter and state, transition resulting in -1 shall not happen (this is an error in the state machine transcription in vmtlib)@\n";
  Format.fprintf fmt ";for counter, transition resulting in -ERR means there is a requirement that is not satisfied @\n";
  Format.fprintf fmt ";for state, transition resulting in ERR means there is a requirement that is not satisfied @\n";
  Format.fprintf fmt "(define-fun .%s_sv0 () Int (!  %s :next %s_n))@\n" counter_name counter_name counter_name;
  Format.fprintf fmt "(define-fun .%s_init () Bool (! (= %s 0) :init true))@\n" counter_name counter_name;
  let counter_trans = "(define-fun ."^counter_name^"_trans () Bool (! (= "^counter_name^"_n 
                        ( ite  	(= "^state_name^"  IDLE)
                          (ite (not "^tse_name^") 0 0)
                          (ite (= "^state_name^"  TRIG)
                            ( ite 	( or ( and (not "^tee_name^") (not "^tc_name^")) (and (not "^tc_name^") (< "^counter_name^" "^tmin_name^")) (and (not "^tee_name^") (>= "^counter_name^" "^tmax_name^" )) (> "^counter_name^"  "^tmax_name^" )) 
                              0
                              (ite ( and "^tc_name^" (< "^counter_name^"  "^tmax_name^" ) ( or ( not "^tee_name^")  (< "^counter_name^"  "^tmin_name^" )))
                                (+ "^counter_name^" 1)
                                (ite (and "^tee_name^" (<= "^tmin_name^" "^counter_name^") (<= "^counter_name^"  "^tmax_name^" )) 
                                  0
                                  (- 1)
                                            
                                )
                              )
                            )
                            (ite (= "^state_name^"  DELAY)
                              (ite (and (not "^ase_name^") (>= "^counter_name^" "^lmax_name^" )) 
                                (- ERR)
                                (ite (and (< "^counter_name^" "^lmax_name^" ) (or (not "^ase_name^" )(< "^counter_name^" "^lmin_name^")))
                                    (+ "^counter_name^" 1)
                                    (ite (and "^ase_name^" (<= "^lmin_name^" "^counter_name^") (<= "^counter_name^" "^lmax_name^" ))
                                        0
                                        (- 1)
                                    )
                                )
                              )
                              (ite (= "^state_name^"  ACTION)
                                (ite (and "^aee_name^" (<= "^amin_name^"  "^counter_name^") (<= "^counter_name^" "^amax_name^" ))
                                    0
                                    (ite (or (and (not "^ac_name^") (not "^aee_name^")) (and (not "^ac_name^") (< "^counter_name^" "^amin_name^")) (and (not "^aee_name^") (>= "^counter_name^" "^amax_name^")) (> "^counter_name^" "^amax_name^"))
                                      (- ERR)
                                      (ite (and "^ac_name^" (< "^counter_name^" "^amax_name^") (or (not "^aee_name^") (< "^counter_name^" "^amin_name^")))
                                            (+ "^counter_name^" 1)
                                            (- 1)
                                      )
                                    )
                                )
                                (- 1)
                              )
                            )
                          )
                        )
                      ) :trans true))" in
  Format.fprintf fmt "%s@\n" counter_trans;
  generate_invariant fmt ("(define-fun ."^counter_name^"_p0 () Bool (! (>= "^counter_name^" 0) ")  "))" ;

  Format.fprintf fmt "@\n";

  Format.fprintf fmt ";these are the function to explicit SUP state initial value and transition@\n";
  Format.fprintf fmt "(define-fun .%s_sv0 () Int (!  %s :next %s_n))@\n" state_name state_name state_name;
  Format.fprintf fmt "(define-fun .%s_init () Bool (! (= %s 1) :init true))@\n" state_name state_name;
  let state_trans = "(define-fun ."^state_name^"_trans () Bool (! (= "^state_name^"_n 
                        ( ite  	(= "^state_name^"  IDLE)
                          (ite (not "^tse_name^") IDLE TRIG)
                          (ite (= "^state_name^"  TRIG )
                            ( ite 	( or ( and (not "^tee_name^") (not "^tc_name^")) (and (not "^tc_name^") (< "^counter_name^" "^tmin_name^")) (and (not "^tee_name^") (>= "^counter_name^" "^tmax_name^" )) (> "^counter_name^"  "^tmax_name^" )) 
                              IDLE 
                              (ite ( and "^tc_name^" (< "^counter_name^"  "^tmax_name^" ) ( or ( not "^tee_name^")  (< "^counter_name^"  "^tmin_name^" )))
                                TRIG
                                (ite (and "^tee_name^" (<= "^tmin_name^" "^counter_name^") (<= "^counter_name^"  "^tmax_name^" )) 
                                  DELAY
                                  (- 1) 
                                )
                              )
                            )
                            (ite (= "^state_name^"  DELAY)
                              (ite (and (not "^ase_name^") (>= "^counter_name^" "^lmax_name^" )) 
                                ERR
                                (ite (and (< "^counter_name^" "^lmax_name^" ) (or (not "^ase_name^" )(< "^counter_name^" "^lmin_name^")))
                                    DELAY
                                    (ite (and "^ase_name^" (<= "^lmin_name^" "^counter_name^") (<= "^counter_name^" "^lmax_name^" ))
                                        ACTION
                                        (- 1)
                                    )
                                )
                              )
                              (ite (= "^state_name^"  ACTION)
                                (ite (and "^aee_name^" (<= "^amin_name^"  "^counter_name^") (<= "^counter_name^" "^amax_name^" ))
                                    IDLE
                                    (ite (or (and (not "^ac_name^") (not "^aee_name^")) (and (not "^ac_name^") (< "^counter_name^" "^amin_name^")) (and (not "^aee_name^") (>= "^counter_name^" "^amax_name^")) (> "^counter_name^" "^amax_name^"))
                                      ERR
                                      (ite (and "^ac_name^" (< "^counter_name^" "^amax_name^") (or (not "^aee_name^") (< "^counter_name^" "^amin_name^")))
                                            ACTION
                                            (- 1)
                                      )
                                    )
                                )
                                (- 1)
                              )
                            )
                          )
                        )
                      ) :trans true))" in
  Format.fprintf fmt "%s@\n" state_trans;
  generate_invariant fmt ("(define-fun ."^state_name^"_p0 () Bool (! (and (>= "^state_name^" IDLE)  (< "^state_name^" ERR)) ")  "))" ;
  Format.fprintf fmt "\n"

(** [generate_var_decl fmt decl] generates the declaration of variable [decl] used in the requirements in the formatter [fmt] *)
let generate_var_decl fmt decl =  
  match decl with 
  (* for constant force the value using assert*)
  |Ast_types.Constant (name, Const_bool(value)) -> Format.fprintf fmt "(declare-fun %s () Bool)@\n(assert(= %s %b))@\n"  name name value
  |Ast_types.Constant (name, Const_int(value)) -> Format.fprintf fmt "(declare-fun %s () Int)@\n(assert(= %s %d))@\n"  name name value
  |Ast_types.Constant (name, Const_real(value)) -> Format.fprintf fmt "(declare-fun %s () Real)@\n(assert(= %s %f))@\n"  name name value
  (* for variable let the value free to change *)
  |Ast_types.Input (name, Bool) 
  |Ast_types.Output (name, Bool) 
  |Ast_types.Internal (name, Bool) -> Format.fprintf fmt "(declare-fun %s () Bool)@\n" name
  |Ast_types.Input (name, Int) 
  |Ast_types.Output (name, Int) 
  |Ast_types.Internal (name, Int) -> Format.fprintf fmt "(declare-fun %s () Int)@\n" name
  |Ast_types.Input (name, Real) 
  |Ast_types.Output (name, Real) 
  |Ast_types.Internal (name, Real) -> Format.fprintf fmt "(declare-fun %s () Real)@\n" name
  
(** [generate_intermediate_var_decl fmt var_name ] generates the declaration of an intermediate variable [var_name]
    and initialized it to false in the formatter [fmt] *)
let generate_intermediate_var_decl fmt (var_name :string)=
  Format.fprintf fmt "(declare-fun %s () Bool)@\n" var_name;
  Format.fprintf fmt "(define-fun .%s_init () Bool (! (= %s false) :init true))@\n" var_name var_name

(** [generate_event_content fmt event] generates the content of an [event] into the formatter [fmt] *)
let rec generate_event_content fmt event =
  match event with
  | Sup_types.Var (s) -> Format.fprintf fmt "%s " s
  | Sup_types.Not (e) -> Format.fprintf fmt "(not " ; generate_event_content fmt e ; Format.fprintf fmt ")"
  | Sup_types.Or (e1,e2) -> Format.fprintf fmt "(or " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.And (e1,e2) -> Format.fprintf fmt "(and " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Constant(b) -> if b then  Format.fprintf fmt "true" else  Format.fprintf fmt "false" 
  | Sup_types.IntConstant(i)-> Format.fprintf fmt " %d " i
  | Sup_types.RealConstant(f)-> Format.fprintf fmt " %d " (int_of_float (f *. 10.0))
  | Sup_types.Plus(e1,e2) -> Format.fprintf fmt "(+ " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Minus(e1,e2) -> Format.fprintf fmt "(- " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Multiply(e1,e2) -> Format.fprintf fmt "(* " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Divide(e1,e2) -> Format.fprintf fmt "(/ " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Eq(e1,e2) -> Format.fprintf fmt "(= " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Geq(e1,e2) -> Format.fprintf fmt "(>= " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Gt(e1,e2) -> Format.fprintf fmt "(> " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Leq(e1,e2) -> Format.fprintf fmt "(<= " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.Lt(e1,e2) -> Format.fprintf fmt "(< " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt ")"
  | Sup_types.NotEq(e1,e2) -> Format.fprintf fmt "(not (= " ; generate_event_content fmt e1 ; Format.fprintf fmt "  " ; generate_event_content fmt e2 ; Format.fprintf fmt "))"

(** [generate_event_declaration fmt sup_index prefix event] generates for a SUP with index [sup_index] a function 
    which return the evaluation of an [event] in the formatter [fmt]. This generation is identical for triggers, actions... 
    The [prefix] allows to indicate in the name of the function which event is handled *)
let generate_event_declaration fmt sup_index prefix (event: Sup_types.event) = 
  let var_name = prefix^"_"^(string_of_int sup_index) in
  Format.fprintf fmt "(define-fun %s () Bool " var_name;
  generate_event_content fmt event;
  Format.fprintf fmt ")@\n" 

(** [generate_sup fmt req_name sup_index sup] generates the conversion in the formatter [fmt] of a SUP with name [req_name] and index 
    [sup_index] into VMT lib format *)
let generate_sup fmt req_name sup_index (sup : Sup_types.sup_req)=
  let t = sup.t in 
  generate_event_declaration fmt sup_index ("tse_"^req_name) t.tse;
  generate_event_declaration fmt sup_index ("tc_"^req_name)  t.tc;
  generate_event_declaration fmt sup_index ("tee_"^req_name) t.tee;
  let a = sup.a in
  generate_event_declaration fmt sup_index ("ase_"^req_name) a.ase;
  generate_event_declaration fmt sup_index ("ac_"^req_name)  a.ac;
  generate_event_declaration fmt sup_index ("aee_"^req_name) a.aee;
  generate_SUP_content fmt sup_index req_name t.tmin t.tmax sup.d.lmin sup.d.lmax a.amin a.amax
  
  
(** [generate_sup_list fmt req_name (_, req_sups_list)] generates the conversion in the formatter [fmt] of the SUP 
    list [req_sups_list] corresponding to a parsed requirement with name [req_name] in SUP format into VMT lib format *)
let generate_sup_list fmt req_name (_, req_sups_list) =
  Format.fprintf fmt ";generation of the SUP state machine for requirement %s @\n" req_name;
  List.iteri (fun i sup -> generate_sup fmt req_name i sup) req_sups_list

(** [generate_requirements fmt t] generates the conversion of the parsed requirements [t] in SUP format into VMT lib format
    in the formatter [fmt] *)
let generate_requirements fmt (t:Parse.t) = 
  (*print original variables*)
  Format.fprintf fmt "@\n;these are the variables used in triggers and actions@\n";
  let vars = t.vars in
  let vars_decl = List.of_seq ( Hashtbl.to_seq_values vars ) in
  List.iter (fun decl -> generate_var_decl fmt decl) vars_decl;
  let ((generated_variables:string list), sup_map) =  Sup.of_req_with_non_bool t in 

  (*print generated variables used to convert requirements to SUP*)
  if (List.length generated_variables) > 0 then (
    Format.fprintf fmt "@\n;these are generated variables to model Before, After ... requirements @\n";
    List.iter (fun var_name -> (generate_intermediate_var_decl fmt var_name)) generated_variables
  );

  (*print the SUPs*)
  Format.fprintf fmt "@\n;these are generated SUPs @\n";
  Sup.SMap.iter (fun key value -> generate_sup_list fmt key value) sup_map
 
(** [generate_vmt_file fmt t] generates a file in the vmt-lib format containing the parsed requirements [t] in the formatter [fmt]*)
let generate_vmt_file fmt t=
  generate_state fmt;
  generate_counter fmt;
  generate_requirements fmt t;
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "(check-sat)@\n"

 