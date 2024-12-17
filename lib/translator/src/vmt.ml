open Sups

(** [generate_state fmt] defines the states list in the formatter [fmt]. Depending
    on [args] it can be encoded either with integers or booleans *)
let generate_state fmt (args: Input_args.t) =
  match args.state_enc with
  | IntegerEncoding -> (
        Format.fprintf fmt ";this is the states for SUP@\n";
        Format.fprintf fmt "(define-fun IDLE () Int 0)@\n";
        Format.fprintf fmt "(define-fun TRIG () Int 1)@\n";
        Format.fprintf fmt "(define-fun DELAY () Int 2)@\n";
        Format.fprintf fmt "(define-fun ACTION () Int 3)@\n";
        Format.fprintf fmt "(define-fun ERR () Int 4)@\n";
        Format.fprintf fmt ";some helper functions for SUP status @\n";
        Format.fprintf fmt "(define-fun is_IDLE ( (s Int) ) Bool (= s IDLE))@\n";
        Format.fprintf fmt "(define-fun is_TRIG ( (s Int) ) Bool (= s TRIG))@\n";
        Format.fprintf fmt "(define-fun is_DELAY ( (s Int) ) Bool (= s DELAY))@\n";
        Format.fprintf fmt "(define-fun is_ACTION ( (s Int) ) Bool (= s ACTION))@\n";
        Format.fprintf fmt "(define-fun is_ERR ( (s Int) ) Bool (= s ERR))@\n"
        )
  | BooleanEncoding -> (
        Format.fprintf fmt ";this boolean variables are used to define the state of the SUP";
        Format.fprintf fmt ";  bool err, loc0, loc1;
         ;
         ;  (err, loc0, loc1)
         ;  000 IDLE
         ;  001 TRIG
         ;  010 DELAY
         ;  011 ACTION
         ;  1__ ERR
        ";
        Format.fprintf fmt ";some helper functions for SUP status @\n";
        Format.fprintf fmt "(define-fun is_IDLE ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err) (not loc0) (not loc1)))@\n";
        Format.fprintf fmt "(define-fun is_TRIG ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err) (not loc0) loc1))@\n";
        Format.fprintf fmt "(define-fun is_DELAY ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err) loc0 (not loc1)))@\n";
        Format.fprintf fmt "(define-fun is_ACTION ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err)  loc0  loc1 ))@\n";
        Format.fprintf fmt "(define-fun is_ERR ( (err Bool) ) Bool (= true err) )@\n"
  )

(** [time_to_string t] convert the time [t] to string, for now only integer time units *)
let time_to_string t clock_t=
  let open Input_args in
  let s = (match t with
  | Sup_types.GreaterThan(v)
  | Sup_types.LesserThan(v)
  | Time(v) 
      -> if v >= 0 then string_of_int v else  raise (Failure "Negative time are not handled in vmt")
  ) in
  if clock_t = RealClock then ("(to_real "^s^")") else s

  
(** current invariant unique id *)
let invariant_index = ref 0

(** [generate_invariant fmt content_start content_end] generates an invariant with a unique id in the formatter [fmt]
    [content_start] is what is before the invar property definition and [content_end] is what is after*)
let generate_invariant fmt content_start content_end =
  Format.fprintf fmt "%s@\n" (content_start ^ " :invar-property " ^ (string_of_int !invariant_index) ^content_end);
  invariant_index := !invariant_index + 1

(** [generate_variable_and_its_next_value fmt name typ initial_value is_clock] generates the smt code for the variable [name]
    of type [typ] with an [initial_value]. If it is a clock, [is_clock] shall be true *)
let generate_variable_and_its_next_value fmt name typ initial_value is_clock =
  let next = if is_clock then "nextclock" else "next" in
  Format.fprintf fmt "(declare-fun %s () %s)@\n" name typ;
  Format.fprintf fmt "(declare-fun %s_n () %s)@\n" name typ;
  Format.fprintf fmt "(define-fun .%s_sv0 () %s (!  %s :%s %s_n))@\n" name typ name next name;
  Format.fprintf fmt "(define-fun .%s_init () Bool (! (= %s %s) :init true))@\n" name name initial_value

(** [generate_SUP_status_variables_and_func fmt state_name args] generates the smt code to get and set 
    the status of a SUP with name [state_name]. Input [args] are given in order to handle
    the encoding of the SUP states (either boolean or integer) *)
let generate_SUP_status_variables_and_func fmt state_name args =
  let open Input_args in
  match args.state_enc with
  |IntegerEncoding -> (generate_variable_and_its_next_value fmt state_name "Int" "IDLE" false;
                      Format.fprintf fmt "(define-fun is_%s_IDLE () Bool (is_IDLE %s) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun is_%s_TRIG () Bool (is_TRIG %s) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun is_%s_DELAY () Bool (is_DELAY %s) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun is_%s_ACTION () Bool (is_ACTION %s) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun is_%s_ERR () Bool (is_ERR %s) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun set_%s_IDLE () Bool (is_IDLE %s_n) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun set_%s_TRIG () Bool (is_TRIG %s_n) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun set_%s_DELAY () Bool (is_DELAY %s_n) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun set_%s_ACTION () Bool (is_ACTION %s_n) )@\n" state_name state_name;
                      Format.fprintf fmt "(define-fun set_%s_ERR () Bool (is_ERR %s_n) )@\n" state_name state_name
                      
                      )
  |BooleanEncoding -> ( generate_variable_and_its_next_value fmt (state_name^"_err") "Bool" "false" false;
                        generate_variable_and_its_next_value fmt (state_name^"_loc0") "Bool" "false" false;
                        generate_variable_and_its_next_value fmt (state_name^"_loc1") "Bool" "false" false;
                        Format.fprintf fmt "(define-fun is_%s_IDLE () Bool (is_IDLE %s_err %s_loc0 %s_loc1) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun is_%s_TRIG () Bool (is_TRIG %s_err %s_loc0 %s_loc1) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun is_%s_DELAY () Bool (is_DELAY %s_err %s_loc0 %s_loc1) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun is_%s_ACTION () Bool (is_ACTION %s_err %s_loc0 %s_loc1) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun is_%s_ERR () Bool (is_ERR %s_err) )@\n" state_name state_name;
                        Format.fprintf fmt "(define-fun set_%s_IDLE () Bool (is_IDLE %s_err_n %s_loc0_n %s_loc1_n) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun set_%s_TRIG () Bool (is_TRIG %s_err_n %s_loc0_n %s_loc1_n) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun set_%s_DELAY () Bool (is_DELAY %s_err_n %s_loc0_n %s_loc1_n) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun set_%s_ACTION () Bool (is_ACTION %s_err_n %s_loc0_n %s_loc1_n) )@\n" state_name state_name state_name state_name;
                        Format.fprintf fmt "(define-fun set_%s_ERR () Bool (is_ERR %s_err_n) )@\n" state_name state_name
                      )

(** [generate_counter_lt_min counter_name tmin args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmin]. [tmin] is converted to real or integer 
    depending the option in [args]. If tmin is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_lt_min counter_name tmin args (*to_leave_state*) _ =
  let open Input_args in
  let op = (match tmin with
  |  Sup_types.GreaterThan (_)-> "<="
  | _-> "<") in 
  try
    let t = time_to_string tmin args.clock_t in
    "("^op^" " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> (*if to_leave_state then " false " else " true "*) " true "

(** [generate_counter_ge_min counter_name tmin args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmin]. [tmin] is converted to real or integer 
    depending the option in [args]. If tmin is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
and generate_counter_ge_min counter_name tmin args (*to_leave_state*) _  =
    let open Input_args in
    let op = (match tmin with
    |  Sup_types.GreaterThan (_)-> ">"
    | _-> ">=") in 
    try
      let t = time_to_string tmin args.clock_t in
      "("^op^" " ^counter_name ^ " "^ t ^" )"
    with Failure _ -> (*if to_leave_state then " false " else " true "*) " false "


(** [generate_counter_gt_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
and generate_counter_gt_max counter_name tmax args (*to_leave_state*) _ =
  let open Input_args in
  let op = (match tmax with
  |  Sup_types.LesserThan (_)-> ">="
  | _-> ">") in 
  try
    let t = time_to_string tmax args.clock_t in
    "("^op^" " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> (*if to_leave_state then " false " else " true "*) " false "

(** [generate_counter_le_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_le_max counter_name tmax args (*to_leave_state*) _ =
  let open Input_args in
  let op = (match tmax with
  |  Sup_types.LesserThan (_)-> "<"
  | _-> "<=") in 
  try
    let t = time_to_string tmax args.clock_t in
    "("^op^" " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> (*if to_leave_state then " false " else " true "*) " true "

(** [generate_counter_ge_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_ge_max counter_name tmax args (*to_leave_state*) _ =
  let open Input_args in
  try
    let t = time_to_string tmax args.clock_t in
    "(>= " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> (*if to_leave_state then " false " else " true "*) " false "
  
(** [generate_counter_lt_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_lt_max counter_name tmax args (*to_leave_state*) _ =
  let open Input_args in
  try
    let t = time_to_string tmax args.clock_t in
    "(< " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> (*if to_leave_state then " false " else " true "*) " true "
  
(** [is_t_nul t] returns true if [t] is equal to 0*)
let is_t_nul t =
  (match t with
  | Sup_types.Time(v) 
      -> if v = 0 then true else false
  | Sup_types.GreaterThan(_)
  | Sup_types.LesserThan (_)
    -> false
  ) 

(* let is_t_infinite t =
  (match t with
  | Sup_types.Time(v) 
      -> if v = -1 then true else false
  | Sup_types.GreaterThan(_)
  | Sup_types.LesserThan (_)
    -> false
  ) *)

let need_vacuity_prop req_name args need_vacuity =
  let open Input_args in
  need_vacuity && ((List.mem req_name args.check_non_vacuity) || (List.mem "__all__" args.check_non_vacuity))

  
(** [generate_SUP_content fmt sup_index req_name tmin tmax lmin lmax amin amax] generates the states machine for the state and counter of the SUP 
    with index [sup_index] and name [req_name] in the formatter [fmt]
    The SUP trigger start event is [tmin]
    The SUP trigger end event is [tmax]
    The SUP delay min is [lmin]
    The SUP delay max is [lmax]
    The SUP action start event is [amin]
    The SUP action end event is [amax]
     *)
let generate_SUP_status fmt sup_index req_name (args : Input_args.t)  =
  let state_name = "state_"^req_name^"_"^(string_of_int sup_index) in
  let counter_name = "c_"^req_name^"_"^(string_of_int sup_index) in

  let t0 = time_to_string (Time 0) args.clock_t in

  Format.fprintf fmt ";these are the function to access SUP attributes (state, time counter, trigger/delay/action time@\n";
  generate_SUP_status_variables_and_func fmt state_name args;
  Format.fprintf fmt "@\n";

  let clock_type = (match args.clock_t with
  |IntegerClock-> "Int"
  |RealClock -> "Real") in
  (* defines the variables needed for the SUP state machine*)
  Format.fprintf fmt ";these are the function to explicit SUP time counter initial value and transition@\n";
  generate_variable_and_its_next_value fmt counter_name clock_type t0 true;
  Format.fprintf fmt "%s@\n" ("(define-fun "^ counter_name^"_unchanged () Bool (= "^counter_name^"_n "^ counter_name^ "))");
  Format.fprintf fmt "%s@\n" ("(define-fun "^ counter_name^"_reset () Bool (= "^counter_name^"_n "^ t0 ^ "))");
  Format.fprintf fmt "@\n";
  "is_" ^ state_name ^ "_ERR"


let generate_SUP_content fmt sup_index req_name tmin tmax lmin lmax amin amax (args : Input_args.t) candidate_to_vacuity =
  let state_name = "state_"^req_name^"_"^(string_of_int sup_index) in
  let counter_name = "c_"^req_name^"_"^(string_of_int sup_index) in
  let vacuity_name = if need_vacuity_prop req_name args candidate_to_vacuity then "vacuity_"^req_name^"_"^(string_of_int sup_index) else "" in
  let tse_name = "tse_"^req_name^"_"^(string_of_int sup_index) in
  let tee_name = "tee_"^req_name^"_"^(string_of_int sup_index) in
  let tc_name = "tc_"^req_name^"_"^(string_of_int sup_index) in
  let ase_name = "ase_"^req_name^"_"^(string_of_int sup_index) in
  let aee_name = "aee_"^req_name^"_"^(string_of_int sup_index) in
  let ac_name = "ac_"^req_name^"_"^(string_of_int sup_index) in
  
  (*if specified in the command line, check the non vacuity*)
  if (String.length vacuity_name > 0) then
    begin
      Format.fprintf fmt "\n;these are the function to explicit SUP non vacuity initial value and transition@\n";
      Format.fprintf fmt "(declare-fun %s () Bool)@\n" vacuity_name ;
      Format.fprintf fmt "(declare-fun %s_n () Bool)@\n" vacuity_name ;
      Format.fprintf fmt "(define-fun .%s_sv0 () %s (!  %s :next %s_n))@\n" vacuity_name "Bool" vacuity_name vacuity_name;
      Format.fprintf fmt "(define-fun .%s_init () Bool (! (= %s true) :init true))@\n" vacuity_name vacuity_name;
      Format.fprintf fmt "(define-fun %s_unchanged () Bool (= %s_n  %s ))@\n" vacuity_name vacuity_name vacuity_name;
      let invar_prop = "(define-fun ."^vacuity_name^"_prop () Bool (! (or "^vacuity_name^ " (not .all_sup_status ) )" in 
                          generate_invariant fmt invar_prop "))" ;
                          Format.fprintf fmt "@\n"
    end;
  

  (* if not (is_t_infinite tmax) then   
    begin
    let t = time_to_string tmax args.clock_t in
    generate_invariant fmt ("(define-fun ."^state_name^"_locinvar1 () Bool (! (=> is_"^state_name^"_TRIG (<= "^counter_name^" "^t^")) ") "))"; Format.fprintf fmt "@\n"
    end;

  if not (is_t_infinite lmax) then
    begin  
    let t = time_to_string lmax args.clock_t in
    generate_invariant fmt ("(define-fun ."^state_name^"_locinvar2 () Bool (! (=> is_"^state_name^"_DELAY (<= "^counter_name^" "^t^")) ") "))"; Format.fprintf fmt "@\n"
    end;

  if not (is_t_infinite amax) then  
    begin
    let t = time_to_string amax args.clock_t in
    generate_invariant fmt ("(define-fun ."^state_name^"_locinvar0 () Bool (! (=> is_"^state_name^"_ACTION (<= "^counter_name^" "^t^")) ") "))"; Format.fprintf fmt "@\n"
    end; *)

  let vacuity_constraint = (if (String.length vacuity_name) = 0 then "" else  "(not "^vacuity_name^ "_n )") in
  let vacuity_unchanged = (if (String.length vacuity_name) = 0 then "" else  " "^vacuity_name^ "_unchanged ") in
  
  (* variables used for multiple transition in one tick*)
  let tmin_is_nul = is_t_nul tmin in
  let amin_is_nul = is_t_nul amin in
  let lmin_is_nul = is_t_nul lmin in
  let tmax_is_nul = is_t_nul tmax in
  let amax_is_nul = is_t_nul amax in
  let lmax_is_nul = is_t_nul lmax in

  (*define all guards*)
  (* Without next clock constraint on "no_clock" *)
  Format.fprintf fmt "; guards of the SUP transitions + clock reset/not changed@\n";
  Format.fprintf fmt "(define-fun stay_idle_%s () Bool ( and (not %s) %s_unchanged ))@\n"  state_name tse_name counter_name  ;
  Format.fprintf fmt "(define-fun idle_to_trig_%s () Bool ( and %s %s_reset  ))@\n" state_name tse_name  counter_name  ;
  Format.fprintf fmt "(define-fun trig_to_idle_%s () Bool ( and (%s) %s_reset))@\n" state_name (" or ( and (not "^tee_name^") (not "^tc_name^")) (and (not "^tc_name^") "^(generate_counter_lt_min counter_name tmin args true)^" ) (and (not "^tee_name^") "^ (generate_counter_ge_max counter_name tmax args true) ^") "^ (generate_counter_gt_max counter_name tmax args true)) counter_name  ;
  Format.fprintf fmt "(define-fun stay_trig_%s () Bool  ( and ( %s) %s_unchanged )) @\n" state_name (" and "^tc_name^" "^(generate_counter_lt_max counter_name tmax args false)^" ( or ( not "^tee_name^")  "^(generate_counter_lt_min counter_name tmin args false)^")") counter_name ;
  Format.fprintf fmt "(define-fun trig_to_delay_%s () Bool ( and (%s) %s_reset ))@\n" state_name ("and "^tee_name^" "^(generate_counter_ge_min counter_name tmin args true)^" "^ (generate_counter_le_max counter_name tmax args true))  counter_name  ;
  Format.fprintf fmt "(define-fun stay_delay_%s () Bool (  and (%s) %s_unchanged ))@\n" state_name ("and "^(generate_counter_lt_max counter_name lmax args false)^ " ( or (not "^ ase_name^") " ^ (generate_counter_lt_min counter_name lmin args false) ^")" ) counter_name;
  Format.fprintf fmt "(define-fun delay_to_err_%s () Bool ( and (%s) %s_unchanged ))@\n" state_name ("and (not "^ase_name^") " ^ (generate_counter_ge_max counter_name lmax args true) ) counter_name;
  Format.fprintf fmt "(define-fun stay_act_%s () Bool ( and (%s) %s_unchanged ))@\n" state_name ("and "^ac_name^"  "^ (generate_counter_lt_max counter_name amax args false) ^ " (or (not "^aee_name^") "^ (generate_counter_lt_min counter_name amin args  false)^ ")") counter_name;
  Format.fprintf fmt "(define-fun act_to_err_%s () Bool ( and (%s) %s_unchanged ))@\n" state_name ("or (and (not "^ac_name^") (not "^aee_name^")) (and (not "^ac_name^") "^ (generate_counter_lt_min counter_name amin args true) ^") (and (not "^aee_name^") "^ (generate_counter_ge_max counter_name amax args true)^") "^ (generate_counter_gt_max counter_name amax args true)) counter_name;
  Format.fprintf fmt "(define-fun act_to_idle_%s () Bool ( and (%s) %s %s_reset ))@\n" state_name ("and "^aee_name^" "^(generate_counter_ge_min counter_name amin args true) ^" "^ (generate_counter_le_max counter_name amax args true)) vacuity_constraint counter_name ;
  Format.fprintf fmt "(define-fun delay_to_act_%s () Bool ( and (%s) %s_reset ))@\n" state_name (" and "^ase_name^" " ^ (generate_counter_ge_min counter_name lmin args true) ^" "^(generate_counter_le_max counter_name lmax args true)) counter_name ;
  Format.fprintf fmt "; guards of the SUP transitions without clock delay@\n";

  if tmin_is_nul then
    Format.fprintf fmt "(define-fun trig_to_delay_no_clock_%s () Bool %s )@\n" state_name tee_name
  else 
    Format.fprintf fmt "(define-fun trig_to_delay_no_clock_%s () Bool false )@\n" state_name;

  if lmax_is_nul then 
    Format.fprintf fmt "(define-fun delay_to_err_no_clock_%s () Bool %s )@\n" state_name ("(not "^ase_name^") ")
  else
    Format.fprintf fmt "(define-fun delay_to_err_no_clock_%s () Bool false)@\n" counter_name;  

  Format.fprintf fmt "(define-fun idle_to_trig_no_clock_%s () Bool %s)@\n" state_name tse_name;

  if lmin_is_nul then
    Format.fprintf fmt "(define-fun delay_to_act_no_clock_%s () Bool %s )@\n" state_name ase_name
  else
    Format.fprintf fmt "(define-fun delay_to_act_no_clock_%s () Bool false )@\n" state_name;
  if amax_is_nul then (
    Format.fprintf fmt "(define-fun act_to_err_no_clock_%s () Bool %s )@\n" state_name (" (not "^aee_name^") ");
    Format.fprintf fmt "(define-fun act_to_idle_no_clock_%s () Bool ( and true %s %s ))@\n" state_name aee_name vacuity_constraint
  ) else if amin_is_nul then (
    Format.fprintf fmt "(define-fun act_to_err_no_clock_%s () Bool (and (not %s) (not %s)) )@\n" state_name ac_name aee_name;
    Format.fprintf fmt "(define-fun act_to_idle_no_clock_%s () Bool ( and true %s %s ))@\n" state_name aee_name vacuity_constraint
  ) else (
    Format.fprintf fmt "(define-fun act_to_err_no_clock_%s () Bool (not %s) )@\n" state_name ac_name;
    Format.fprintf fmt "(define-fun act_to_idle_no_clock_%s () Bool false)@\n" state_name
  );
  
  (*SUP state transition and initialization*)
  Format.fprintf fmt "\n; SUP transitions\n";
  
  (* in case of possible multiple transitions in one tick, to model single transition and be deterministic, 
    it is needed in the guard to check that the next transition guard is not valid immediately *)
  let check_stop_after_idle = 
    if tmax_is_nul then 
      "(or (not " ^ tse_name ^ ") (not " ^ tee_name ^"))"
    else if tmin_is_nul then 
      "(or (not " ^ tse_name ^ ") (and (not " ^ tee_name ^") (not " ^ tc_name ^ ")))"
    else 
      "(not " ^ tse_name ^ ")"
  in
  let check_stop_after_trig = 
    if tmax_is_nul then 
      "(and " ^ tee_name ^" (not trig_to_delay_no_clock_"^state_name^"))"
    else if tmin_is_nul then 
      "(and "^ tc_name ^" (not trig_to_delay_no_clock_"^state_name^"))"
    else 
      tc_name
  in
  let check_stop_after_delay = 
    if lmax_is_nul then 
      "false" (* ~ase /\ ase *)
    else if lmin_is_nul then 
      "(not delay_to_act_no_clock_" ^state_name^") "
    else 
      "" 
    in
  let check_stop_after_action = 
    if amax_is_nul then 
      "false" (* ~aee /\ aee *)
    else if amin_is_nul then
      "(and (or " ^ ac_name ^ " " ^ aee_name ^ ") (not act_to_idle_no_clock_" ^state_name^") )"
    else ac_name
  in
  (*transition function*)
  let state_trans = 
    (if (tmin_is_nul && lmin_is_nul && amin_is_nul && tmax_is_nul && amax_is_nul && lmax_is_nul) then
      "(define-fun ."^state_name^"_trans () Bool (!  ( and" ^
      "\n; all constants are 0 so the state machine is simpler"
      ^"\n; TODO in this case, all clock and state variables can be removed altogether, except for the variable err"
      ^"\n  (=> is_" ^ state_name ^ "_ERR set_" ^ state_name ^ "_ERR) ; make the err state absorbing"
      ^"\n  (or "
      ^"\n    (and " ^ tse_name ^ " " ^ tee_name ^ " (not (and " ^ ase_name ^ " " ^ aee_name ^ ")) set_" ^ state_name ^ "_ERR "  ^ vacuity_unchanged ^ ")"
      ^"\n    (and (not is_" ^ state_name ^ "_ERR) (not (and " ^ tse_name ^ " " ^ tee_name ^ ")) set_" ^ state_name ^ "_IDLE " ^ vacuity_unchanged ^ ")"
      ^"\n    (and (not is_" ^ state_name ^ "_ERR)" ^ tse_name ^ " " ^ tee_name ^ " " ^ ase_name ^ " " ^ aee_name ^ " set_" ^ state_name ^ "_IDLE " ^ vacuity_constraint ^ ")"
      ^"\n  )"
      ^"\n) :trans true))" 
      else
        begin
        "(define-fun ."^state_name^"_trans () Bool (!  (or" ^
        "\n\n; Combined transitions"^
        "\n; IDLE -> TRIG -> DELAY -> ACT -> IDLE\n"^
        "(and is_" ^ state_name ^ "_IDLE idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^" delay_to_act_no_clock_" ^state_name^" act_to_idle_no_clock_" ^state_name^" set_"^state_name^"_IDLE" ^ vacuity_constraint ^")"^
        "\n; IDLE -> TRIG -> DELAY -> ERR \n"^
        "(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")" ^
        "\n; IDLE -> TRIG -> DELAY -> ACT -> ERR \n"^
        "(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"  delay_to_act_no_clock_" ^state_name^"  act_to_err_no_clock_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")"^
        "\n; IDLE -> TRIG -> DELAY -> ACT \n"^
        "(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"  delay_to_act_no_clock_" ^state_name^ " " ^ check_stop_after_action ^ " set_"^state_name^"_ACTION" ^vacuity_unchanged^")" ^
        "\n; IDLE -> TRIG -> DELAY \n"^
        "(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"  " ^ check_stop_after_delay ^ " set_"^state_name^"_DELAY" ^vacuity_unchanged^")" ^
        "\n; IDLE -> TRIG \n"^
        "(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^" " ^ check_stop_after_trig ^ " set_"^state_name^"_TRIG" ^vacuity_unchanged^")" ^

        "\n; TRIG -> DELAY -> ACT -> IDLE -> TRIG \n"^
        "(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_no_clock_" ^state_name^"   act_to_idle_no_clock_" ^state_name^"  idle_to_trig_" ^state_name^"  set_"^state_name^"_TRIG" ^ vacuity_constraint ^ ") " ^
        "\n; TRIG -> DELAY -> ACT -> IDLE \n"^
        "(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_no_clock_" ^state_name^"   act_to_idle_no_clock_" ^state_name^" " ^ check_stop_after_idle ^ " set_"^state_name^"_IDLE" ^ vacuity_constraint ^ ") " ^
        "\n; TRIG -> DELAY -> ACT \n"^
        "(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_no_clock_" ^state_name^"   " ^ check_stop_after_action ^ " set_"^state_name^"_ACTION" ^vacuity_unchanged^")" ^
        "\n; TRIG -> DELAY -> ERR \n"^
        "(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_err_no_clock_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")" ^
        "\n; TRIG -> DELAY \n"^
        "(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  " ^ check_stop_after_delay ^ "  set_"^state_name^"_ERR" ^vacuity_unchanged^")" ^

        "\n; DELAY -> ACT -> IDLE -> TRIG -> DELAY \n"^
        "(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_idle_no_clock_" ^state_name^"   idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"   set_"^state_name^"_DELAY " ^ vacuity_constraint ^ ")"^
        "\n; DELAY -> ACT -> IDLE -> TRIG \n"^
        "(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_idle_no_clock_" ^state_name^"   idle_to_trig_" ^state_name^" " ^ check_stop_after_trig ^ " set_"^state_name^"_TRIG" ^ vacuity_constraint ^ ")"^
        "\n; DELAY -> ACT -> IDLE \n"^
        "(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_idle_no_clock_" ^state_name^"   " ^ check_stop_after_idle ^ " set_"^state_name^"_IDLE" ^ vacuity_constraint ^ ")"^
        "\n; DELAY -> ACT \n"^
        "(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   " ^ check_stop_after_action ^ " set_"^state_name^"_ACTION" ^vacuity_unchanged^")"^
        "\n; DELAY -> ACT -> ERR \n"^
        "(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_err_no_clock_" ^state_name^" set_"^state_name^"_ERR" ^ vacuity_constraint ^ ")"^

        "\n; ACT -> IDLE -> TRIG -> DELAY -> ACT \n"^
        "(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"  delay_to_act_no_clock_" ^state_name^"  set_"^state_name^"_ACTION" ^ vacuity_constraint ^ ")" ^
        "\n; ACT -> IDLE -> TRIG -> DELAY \n"^
        "(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"  " ^ check_stop_after_delay ^ " set_"^state_name^"_DELAY" ^ vacuity_constraint ^ ")" ^
        "\n; ACT -> IDLE -> TRIG  \n"^
        "(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  " ^ check_stop_after_trig ^ " set_"^state_name^"_TRIG" ^ vacuity_constraint ^ ")" ^
        "\n; ACT -> IDLE -> TRIG -> DELAY -> ERR\n"^
        "(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  trig_to_delay_no_clock_" ^state_name^"   delay_to_err_no_clock_" ^state_name^" set_"^state_name^"_ERR" ^ vacuity_unchanged ^ ")" 
        ^
        "\n\n; Single transitions:
        (and is_" ^ state_name ^ "_IDLE  stay_idle_" ^state_name^"  set_"^state_name^"_IDLE " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  " ^ check_stop_after_trig ^ " set_"^state_name^"_TRIG " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_TRIG  trig_to_idle_" ^state_name^" (not idle_to_trig_no_clock_" ^ state_name ^ ") set_"^state_name^"_IDLE " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_TRIG  stay_trig_" ^state_name^"  set_"^state_name^"_TRIG " ^vacuity_unchanged^") 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  "^ check_stop_after_delay ^" set_"^state_name^"_DELAY " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_DELAY  stay_delay_" ^state_name^"  set_"^state_name^"_DELAY " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"  " ^ check_stop_after_action ^ " set_"^state_name^"_ACTION " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_ACTION  stay_act_" ^state_name^"  set_"^state_name^"_ACTION " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  set_"^state_name^"_IDLE" ^vacuity_constraint^")
        (and is_" ^ state_name ^ "_DELAY  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_ACTION  act_to_err_" ^state_name^"   set_"^state_name^"_ERR " ^vacuity_unchanged^")
        ) :trans true))" 
      end
      )  in   
  Format.fprintf fmt "%s@\n" state_trans;
  "is_" ^ state_name ^ "_ERR"

(** [generate_var_decl fmt decl] generates the declaration of variable [decl] used in the requirements in the formatter [fmt] *)
let generate_var_decl fmt decl =  
  let open Reqs in 
  match decl with 
  (* for constant force the value using assert*)
  |Ast_types.Constant (_, Const_bool(_)) (*-> Format.fprintf fmt "(declare-fun %s () Bool)@\n(assert(= %s %b))@\n"  name name value*)
  |Ast_types.Constant (_, Const_int(_)) (*-> Format.fprintf fmt "(declare-fun %s () Int)@\n(assert(= %s %d))@\n"  name name value*)
  |Ast_types.Constant (_, Const_real(_)) -> () (*Format.fprintf fmt "(declare-fun %s () Real)@\n(assert(= %s (to_real %d)))@\n"  name name (int_of_float value)*)
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
let generate_intermediate_var_decl fmt (var_name :string) =
  generate_variable_and_its_next_value fmt var_name "Bool" "false" false;
  Format.fprintf fmt "(define-fun %s_unchanged () Bool (= %s_n  %s ))\n" var_name var_name var_name

(** [generate_generated_var_decl fmt var_name ] generates the declaration of a generated variable [var_name] that replaces a
    non boolean expression in the formatter [fmt] *)
let generate_generated_var_decl fmt (var_name :string) =
  Format.fprintf fmt "(declare-fun %s () Bool)@\n" var_name
    
(** [generate_event_content fmt event] generates the content of an [event] into the formatter [fmt] *)
let rec generate_event_content fmt event use_to_real =
  let open Input_args in
  match event with
  | Sup_types.Var (s) -> Format.fprintf fmt "%s " s
  | Sup_types.Not (e) -> Format.fprintf fmt "(not " ; generate_event_content fmt e use_to_real; Format.fprintf fmt ")"
  | Sup_types.Or (e1,e2) -> Format.fprintf fmt "(or " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.And (e1,e2) -> Format.fprintf fmt "(and " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Constant(b) -> if b then  Format.fprintf fmt "true" else  Format.fprintf fmt "false" 
  | Sup_types.IntConstant(i,multiplier)-> if use_to_real = RealClock then (Format.fprintf fmt "(to_real %d) " (i)) else (Format.fprintf fmt " %d " (multiplier*i))
  | Sup_types.RealConstant(f,multiplier)-> if use_to_real = RealClock then (Format.fprintf fmt   "(to_real %d) " (int_of_float f)) else (Format.fprintf fmt " %d " (int_of_float ((float_of_int multiplier)*.f)))
  | Sup_types.Plus(e1,e2) -> Format.fprintf fmt "(+ " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Minus(e1,e2) -> Format.fprintf fmt "(- " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Multiply(e1,e2) -> Format.fprintf fmt "(* " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Divide(e1,e2) -> Format.fprintf fmt "(/ " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Eq(e1,e2) -> Format.fprintf fmt "(= " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Geq(e1,e2) -> Format.fprintf fmt "(>= " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Gt(e1,e2) -> Format.fprintf fmt "(> " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Leq(e1,e2) -> Format.fprintf fmt "(<= " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.Lt(e1,e2) -> Format.fprintf fmt "(< " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt ")"
  | Sup_types.NotEq(e1,e2) -> Format.fprintf fmt "(not (= " ; generate_event_content fmt e1 use_to_real; Format.fprintf fmt "  " ; generate_event_content fmt e2 use_to_real; Format.fprintf fmt "))"

(** [get_var_name prefix sup_index] generates the name of a variable according to a [prefix] (requirement name) and a [sup_index] as 
    a requirement can be modeled using several SUPs. *)
let get_var_name prefix sup_index =
  prefix^"_"^(string_of_int sup_index) 

(** [convert_event_with_next event] updates the content of the action [event] to use the variable for the next clock step*)
let rec convert_event_with_next event =
  match event with
  | Sup_types.Var (s) -> if String.starts_with ~prefix:"intermediate" s then Sup_types.Var ( s^"_n") else event
  | Sup_types.Not (e) -> Sup_types.Not(convert_event_with_next e)
  |_ -> event


(** [generate_event_declaration fmt sup_index prefix event] generates for a SUP with index [sup_index] a function 
    which return the evaluation of an [event] in the formatter [fmt]. This generation is identical for triggers, actions... 
    The [prefix] allows to indicate in the name of the function which event is handled. In case of action event
    with intermediate variable used to model After, Before ... requirements, those need to be updated and not 
    read (use of [convert_event_with_next] function)*)
let generate_event_declaration fmt sup_index prefix (event: Sup_types.event) use_to_real is_action = 
  let var_name = get_var_name prefix sup_index in
  Format.fprintf fmt "(define-fun %s () Bool " var_name;
  let event_with_next = (if is_action then convert_event_with_next event else event) in 
  generate_event_content fmt event_with_next use_to_real;
  Format.fprintf fmt ")@\n"


(** [generate_sup fmt req_name sup_index sup] generates the conversion in the formatter [fmt] of a SUP with name [req_name] and index 
    [sup_index] into VMT lib format *)
let generate_sup fmt req_name sup_index (sup : Sup_types.sup_req) (args: Input_args.t) generate_content=


  let candidate_to_vacuity = sup.vacuity in
  if not generate_content then
    begin
    generate_SUP_status fmt sup_index req_name args
    end
  else
    begin
      Format.fprintf fmt "; sup  " ; (Sup.print_sup fmt sup args);  Format.fprintf fmt "@\n";
      let t = sup.t in 
      generate_event_declaration fmt sup_index ("tse_"^req_name) t.tse args.clock_t false;
      generate_event_declaration fmt sup_index ("tc_"^req_name)  t.tc args.clock_t false;
      generate_event_declaration fmt sup_index ("tee_"^req_name) t.tee args.clock_t false;
      let a = sup.a in
      generate_event_declaration fmt sup_index ("ase_"^req_name) a.ase args.clock_t true;
      generate_event_declaration fmt sup_index ("ac_"^req_name)  a.ac args.clock_t true;
      (*let var_name = get_var_name ("ac_"^req_name) sup_index in
      Format.fprintf fmt "(declare-fun %s_n () Bool)@\n" var_name;
      Format.fprintf fmt "(define-fun .%s_sv0 () Bool (! %s :next %s_n))@\n" var_name var_name var_name ;*)
      generate_event_declaration fmt sup_index ("aee_"^req_name) a.aee args.clock_t true;
      generate_SUP_content fmt sup_index req_name t.tmin t.tmax sup.d.lmin sup.d.lmax a.amin a.amax args candidate_to_vacuity
      end
  
  
(** [generate_sup_list fmt req_name (_, req_sups_list)] generates the conversion in the formatter [fmt] of the SUP 
    list [req_sups_list] corresponding to a parsed requirement with name [req_name] in SUP format into VMT lib format *)
let generate_sup_list fmt req_name (_, req_sups_list) (args : Input_args.t) generate_content =
  (*generate sups*)
  Format.fprintf fmt "@\n\n\n;generation of the SUP state machine for requirement %s @\n" req_name;
  List.fold_left (fun acc sup -> (generate_sup fmt req_name (List.length acc) sup args generate_content)::acc) [] req_sups_list
  


(** [generate_requirements_ fmt args sup_map generated_variables intermediate_variables] generates the requirements of [sup_map] 
    in sup format into VMT lib in the formatter [fmt]. [generated_variables] and [intermediate_variables] are variables that were 
    generated to model the requirement format into sup format. When reading sup file, thoses list are empties *)
let generate_requirements_ fmt args sup_map generated_variables intermediate_variables= 
  let open Input_args in
  let l = ( Sup.SMap.to_list sup_map) in
  let l_names = List.map fst l in
  List.iter 
    (fun req_id -> 
      if not (List.mem req_id l_names) then
        raise (Invalid_argument (Printf.sprintf "%s is undefined" req_id) )
    ) args.check_non_vacuity;
    
    
  (*print generated variables used to convert non boolean expressions into boolean*)
  if (List.length generated_variables) > 0 then(    
    Format.fprintf fmt "@\n;these are generated variables to replace non boolean expressions @\n";
    List.iter (fun var_name -> (generate_generated_var_decl fmt var_name)) generated_variables
  );

  (*print the SUPs in a string and get error functions*)
  let list_error_func = Sup.SMap.fold (fun key value acc -> (generate_sup_list fmt key value args false)@acc) sup_map [] in

  (*creates an invariant property with all error status funtions*)
  Format.fprintf fmt "@\n;these are error functions @\n";
  let all_func_error = List.fold_left (fun acc s -> (" (not "^s ^") ")^acc) "" list_error_func in
  let invar_prop = "(define-fun .all_sup_status () Bool (! (and true true "^all_func_error^")" in 
  generate_invariant fmt invar_prop "))" ;
  Format.fprintf fmt "@\n";

  (*print generated variables used to convert requirements to SUP*)
  if (List.length intermediate_variables) > 0 then (
    Format.fprintf fmt "@\n;these are generated variables to model Before, After ... requirements @\n";
    List.iter (fun var_name -> (generate_intermediate_var_decl fmt var_name)) intermediate_variables
  );

  (*print the SUPs*)
  Format.fprintf fmt "@\n;these are generated SUPs @\n";
  ignore(Sup.SMap.fold (fun key value acc -> (generate_sup_list fmt key value args true)@acc) sup_map [])




(** [generate_requirements fmt t] generates the conversion of the parsed requirements [t] in SUP format into VMT lib format
    in the formatter [fmt] *)
let generate_requirements fmt (t:Reqs.Parse.t) (args : Input_args.t) = 
  (*print original variables*)
  Format.fprintf fmt "@\n;these are the variables used in triggers and actions@\n";
  let vars = t.vars in
  let vars_decl = List.of_seq ( Hashtbl.to_seq_values vars ) in
  List.iter (fun decl -> generate_var_decl fmt decl) vars_decl;
  let ( generated_variables,(intermediate_variables:string list), sup_map) = Sup.of_req t args in
  generate_requirements_ fmt args sup_map generated_variables intermediate_variables

  
(** [generate_vmt_file fmt t] generates a file in the vmt-lib format containing the parsed requirements [t] in the formatter [fmt]*)
let generate_vmt_file fmt t (args : Input_args.t)=
  generate_state fmt args;
  generate_requirements fmt t args;
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "(assert true)@\n"

(** [generate_sup_var_decl fmt decl] generates the declaration [decl] in the VMT lib format in the formatter [fmt]*)
let generate_sup_var_decl fmt decl =
  let open Sup_types in
  match decl with | Decl(name,_,_)-> Format.fprintf fmt "(declare-fun %s () Bool)@\n" name

(** [var_init_to_bool var_init] generates as a string the initialisation of the variable described in [var_init]*)
let var_init_to_string var_init=
  let open Sup_types in 
  match var_init with
  | VarInit(n, true)-> n
  | VarInit(n, false) -> "(not "^n^")"

(** [var_init_to_bool var_init] generates the function that initializes the inputs [var_init] from the COND_INIT section of the SUP
in the VMT lib format in the formatter [fmt]*)
let generate_sup_var_init fmt init = 
  Format.fprintf fmt "(define-fun .init () Bool (! (= %s true) :init true))"
  (match init with 
  | [] -> "true"
  | hd::[] ->  var_init_to_string hd
  | hd::tail ->  (List.fold_left (fun acc v -> " (and "^ acc ^" "^(var_init_to_string v)^")") (var_init_to_string hd) tail)
  )

(** [generate_requirements_from_sup fmt t args] generates a file in the vmt-lib format containing the parsed sups [t] in the formatter [fmt]*)
let generate_requirements_from_sup fmt t args = 
  let open Sup_types in
  (* generate variable declaration *)
  List.iter( fun decl -> generate_sup_var_decl fmt decl) t.decls;
  (* generate variable initialization *)
  generate_sup_var_init fmt t.inits;
  (* convert the sup requirement list into a map with arbitrary names for each sup*)
  let sup_map = List.fold_left ( fun acc s -> let id= ("ID_"^(string_of_int (Sup.SMap.cardinal acc))) in Sup.SMap.add id  (id,[s]) acc) Sup.SMap.empty t.reqs in
  generate_requirements_ fmt args sup_map [] []

(** [generate_vmt_file_from_sup fmt t] generates a file in the vmt-lib format containing the parsed SUP requirements [t] in the formatter [fmt] *)
let generate_vmt_file_from_sup fmt t (args : Input_args.t)=
  generate_state fmt args;
  generate_requirements_from_sup fmt t args;
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "(assert true)@\n"