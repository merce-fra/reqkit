
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
  invariant_index := !invariant_index + 1;
  Format.fprintf fmt "%s@\n" (content_start ^ " :invar-property " ^ (string_of_int !invariant_index) ^content_end)

(** [generate_variable_and_its_next_value fmt name typ initial_value is_clock] generates the smt code for the variable [name]
    of type [typ] with an [initial_value]. If it is a clock, [is_clock] shall be true *)
let generate_variable_and_its_next_value fmt name typ initial_value is_clock check_rt_consistency=
let next = if is_clock && not check_rt_consistency then "nextclock" else "next" in
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
  |IntegerEncoding -> (generate_variable_and_its_next_value fmt state_name "Int" "IDLE" false args.check_rt_consistency;
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
  |BooleanEncoding -> ( generate_variable_and_its_next_value fmt (state_name^"_err") "Bool" "false" false args.check_rt_consistency;
                        generate_variable_and_its_next_value fmt (state_name^"_loc0") "Bool" "false" false args.check_rt_consistency;
                        generate_variable_and_its_next_value fmt (state_name^"_loc1") "Bool" "false" false args.check_rt_consistency;
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
let rec generate_counter_lt_min counter_name tmin args to_leave_state =
  let open Input_args in
  let op = (match tmin with
  |  Sup_types.GreaterThan (_)-> "<="
  | _-> "<") in 
  try
    let t = time_to_string tmin args.clock_t in
    "("^op^" " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> if to_leave_state then " false " else " true "

(** [generate_counter_ge_min counter_name tmin args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmin]. [tmin] is converted to real or integer 
    depending the option in [args]. If tmin is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
and generate_counter_ge_min counter_name tmin args to_leave_state  =
    "(not "^(generate_counter_lt_min counter_name tmin args to_leave_state) ^")"

(** [generate_counter_gt_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
and generate_counter_gt_max counter_name tmax args to_leave_state =
  let open Input_args in
  let op = (match tmax with
  |  Sup_types.LesserThan (_)-> ">="
  | _-> ">") in 
  try
    let t = time_to_string tmax args.clock_t in
    "("^op^" " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> if to_leave_state then " false " else " true "

(** [generate_counter_le_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_le_max counter_name tmax args to_leave_state =
  "(not "^(generate_counter_gt_max counter_name tmax args to_leave_state) ^")"

(** [generate_counter_ge_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_ge_max counter_name tmax args to_leave_state =
  let open Input_args in
  try
    let t = time_to_string tmax args.clock_t in
    "(>= " ^counter_name ^ " "^ t ^" )"
  with Failure _ -> if to_leave_state then " false " else " true "
  
(** [generate_counter_lt_max counter_name tmax args to_leave_state] generates an smt expression
    that compares the counter [counter_name] to [tmax]. [tmax] is converted to real or integer 
    depending the option in [args]. If tmax is -1, the function returns true or false depending
    if the guards allows to leave the state ([to_leave_state] = true) or not *)
let generate_counter_lt_max counter_name tmax args to_leave_state =
  "(not "^(generate_counter_ge_max counter_name tmax args to_leave_state) ^")"
  
(** [is_t_nul t] returns true if [t] is equal to 0*)
let is_t_nul t =
  (match t with
  | Sup_types.Time(v) 
      -> if v = 0 then true else false
  | Sup_types.GreaterThan(_)
  | Sup_types.LesserThan (_)
    -> false
  ) 

  
(** [generate_SUP_content fmt sup_index req_name tmin tmax lmin lmax amin amax] generates the states machine for the state and counter of the SUP 
    with index [sup_index] and name [req_name] in the formatter [fmt]
    The SUP trigger start event is [tmin]
    The SUP trigger end event is [tmax]
    The SUP delay min is [lmin]
    The SUP delay max is [lmax]
    The SUP action start event is [amin]
    The SUP action end event is [amax]
     *)
let generate_SUP_content fmt sup_index req_name tmin tmax lmin lmax amin amax (args : Input_args.t)=
  let state_name = "state_"^req_name^"_"^(string_of_int sup_index) in
  let counter_name = "c_"^req_name^"_"^(string_of_int sup_index) in
  let vacuity_name = if (List.mem req_name args.check_non_vacuity) then "vacuity_"^req_name^"_"^(string_of_int sup_index) else "" in
  let tse_name = "tse_"^req_name^"_"^(string_of_int sup_index) in
  let tee_name = "tee_"^req_name^"_"^(string_of_int sup_index) in
  let tc_name = "tc_"^req_name^"_"^(string_of_int sup_index) in
  let ase_name = "ase_"^req_name^"_"^(string_of_int sup_index) in
  let aee_name = "aee_"^req_name^"_"^(string_of_int sup_index) in
  let ac_name = "ac_"^req_name^"_"^(string_of_int sup_index) in
  
  let t0 = time_to_string (Time 0) args.clock_t in

  Format.fprintf fmt ";these are the function to access SUP attributes (state, time counter, trigger/delay/action time@\n";
  generate_SUP_status_variables_and_func fmt state_name args;
  Format.fprintf fmt "@\n";

  let clock_type = (match args.clock_t with
  |IntegerClock-> "Int"
  |RealClock -> "Real") in
  (* defines the variables needed for the SUP state machine*)
  Format.fprintf fmt ";these are the function to explicit SUP time counter initial value and transition@\n";
  generate_variable_and_its_next_value fmt counter_name clock_type t0 true args.check_rt_consistency;
  Format.fprintf fmt "%s@\n" ("(define-fun "^ counter_name^"_unchanged () Bool (= "^counter_name^"_n "^ counter_name^ "))");
  Format.fprintf fmt "%s@\n" ("(define-fun "^ counter_name^"_reset () Bool (= "^counter_name^"_n "^ t0 ^ "))");
  Format.fprintf fmt "@\n";

  (*if specified in the command line, check the non vacuity*)
  if (List.mem req_name args.check_non_vacuity) then
    begin
      Format.fprintf fmt "\n;these are the function to explicit SUP non vacuity initial value and transition@\n";
      Format.fprintf fmt "(declare-fun %s () Bool)@\n" vacuity_name ;
      Format.fprintf fmt "(declare-fun %s_n () Bool)@\n" vacuity_name ;
      Format.fprintf fmt "(define-fun .%s_sv0 () %s (!  %s :next %s_n))@\n" vacuity_name "Bool" vacuity_name vacuity_name;
      Format.fprintf fmt "(define-fun .%s_init () Bool (! (= %s true) :init true))@\n" vacuity_name vacuity_name;
      Format.fprintf fmt "(define-fun %s_unchanged () Bool (= %s_n  %s ))" vacuity_name vacuity_name vacuity_name
    end;
    
  let vacuity_constraint = (if (String.length vacuity_name) = 0 then "" else  "(not "^vacuity_name^ "_n )") in
  let vacuity_unchanged = (if (String.length vacuity_name) = 0 then "" else  " "^vacuity_name^ "_unchanged ") in
  
  (*define all guards*)
  Format.fprintf fmt ";these are the functions that explicit the guards of the SUP and  the counter reset/not changed@\n";
  Format.fprintf fmt "(define-fun stay_idle_%s () Bool ( and (not %s) %s_unchanged ))@\n"  state_name tse_name counter_name  ;
  Format.fprintf fmt "(define-fun idle_to_trig_%s () Bool ( and (= %s true)  %s_reset  ))@\n" state_name tse_name  counter_name  ;
  Format.fprintf fmt "(define-fun trig_to_idle_%s () Bool ( and (%s) %s_reset))@\n" state_name (" or ( and (not "^tee_name^") (not "^tc_name^")) (and (not "^tc_name^") "^(generate_counter_lt_min counter_name tmin args true)^" ) (and (not "^tee_name^") "^ (generate_counter_ge_max counter_name tmax args true) ^") "^ (generate_counter_gt_max counter_name tmax args true)) counter_name  ;
  Format.fprintf fmt "(define-fun stay_trig_%s () Bool  ( and ( %s) %s_unchanged )) @\n" state_name (" and "^tc_name^" "^(generate_counter_lt_max counter_name tmax args false)^" ( or ( not "^tee_name^")  "^(generate_counter_lt_min counter_name tmin args false)^")") counter_name ;
  Format.fprintf fmt "(define-fun trig_to_delay_%s () Bool ( and (%s) %s_reset ))@\n" state_name ("and "^tee_name^" "^(generate_counter_ge_min counter_name tmin args true)^" "^ (generate_counter_le_max counter_name tmax args true))  counter_name  ;
  Format.fprintf fmt "(define-fun stay_delay_%s () Bool (  and ( %s ) %s_unchanged ))@\n" state_name ("and "^(generate_counter_lt_max counter_name lmax args false)^ " ( or (not "^ ase_name^") " ^ (generate_counter_lt_min counter_name lmin args false) ^")" ) counter_name;
  Format.fprintf fmt "(define-fun delay_to_err_%s () Bool ( and ( %s ) %s_unchanged ))@\n" state_name ("and (not "^ase_name^") " ^ (generate_counter_ge_max counter_name lmax args true) ) counter_name;
  Format.fprintf fmt "(define-fun delay_to_act_%s () Bool ( and (%s) %s_reset ))@\n" state_name (" and "^ase_name^" " ^ (generate_counter_ge_min counter_name lmin args true) ^" "^(generate_counter_le_max counter_name lmax args true)) counter_name ;
  Format.fprintf fmt "(define-fun stay_act_%s () Bool ( and ( %s ) %s_unchanged ))@\n" state_name ("and "^ac_name^"  "^ (generate_counter_lt_max counter_name amax args false) ^ " (or (not "^aee_name^") "^ (generate_counter_lt_min counter_name amin args  false)^ ")") counter_name;
  Format.fprintf fmt "(define-fun act_to_err_%s () Bool ( and ( %s ) %s_unchanged ))@\n" state_name ("or (and (not "^ac_name^") (not "^aee_name^")) (and (not "^ac_name^") "^ (generate_counter_lt_min counter_name amin args true) ^") (and (not "^aee_name^") "^ (generate_counter_ge_max counter_name amax args true)^") "^ (generate_counter_gt_max counter_name amax args true)) counter_name;
  Format.fprintf fmt "(define-fun act_to_idle_%s () Bool ( and (%s) %s %s_reset ))@\n" state_name ("and "^aee_name^" "^(generate_counter_ge_min counter_name amin args true) ^" "^ (generate_counter_le_max counter_name amax args true)) vacuity_constraint counter_name ;

  (*SUP state transition and initialization*)
  Format.fprintf fmt "\n;this is the function to explicit SUP transition@\n";
  (* variables used for multiple transition in one tick*)
  let tmin_is_nul = is_t_nul tmin in
  let amin_is_nul = is_t_nul amin in
  let lmin_is_nul = is_t_nul lmin in
  let tmax_is_nul = is_t_nul tmax in
  let amax_is_nul = is_t_nul amax in
  let lmax_is_nul = is_t_nul lmax in
  
  
  (*in case of possible multiple transitions in one tick, to model single transition and be deterministic, 
    it is needed in the guard to check that the next transition guard is not valid*)
  let check_stop_after_trig = if not tmin_is_nul then "" else "(not trig_to_delay_" ^state_name^") " in
  let check_stop_after_delay = if not lmin_is_nul then "" else "(not delay_to_act_" ^state_name^") " in
  let check_stop_after_action = if not amin_is_nul then "" else "(not act_to_idle_" ^state_name^") " in

  (*transition function*)
  let state_trans = "(define-fun ."^state_name^"_trans () Bool (!  ( or" ^
    (if (tmin_is_nul && lmin_is_nul && amin_is_nul && tmax_is_nul && amax_is_nul && lmax_is_nul) then
      "\n;all timer are nul so the state machine is more simple
      (and is_" ^ state_name ^ "_IDLE idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^" delay_to_act_" ^state_name^" act_to_idle_" ^state_name^" set_"^state_name^"_IDLE) 
      (and is_" ^ state_name ^ "_IDLE idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^ " (not delay_to_act_" ^state_name^ ") set_"^state_name^"_ERR"^vacuity_unchanged^")
      (and is_" ^ state_name ^ "_IDLE idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^ " delay_to_act_" ^state_name^ " (not act_to_idle_" ^state_name^") set_"^state_name^"_ERR "^vacuity_unchanged^")
      (and is_" ^ state_name ^ "_IDLE idle_to_trig_" ^state_name^" (not trig_to_delay_"^state_name^ ") set_"^state_name^"_IDLE"  ^vacuity_unchanged^ ")"^
      ") :trans true))" 
      else
        begin
        (*four transiations in one tick*)
        (if (tmin_is_nul && lmin_is_nul && amin_is_nul) then    
        "\n;additional transitions because tmin and lmin and amin are equals to 0\n;this is the encoding of a IDLE to IDLE state in one tick 
        (and is_" ^ state_name ^ "_IDLE idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^" delay_to_act_" ^state_name^" act_to_idle_" ^state_name^" set_"^state_name^"_IDLE)"^                 
        "\n(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"  (not act_to_idle_" ^state_name^") set_"^state_name^"_ACTION" ^vacuity_unchanged^")" ^                 
        "\n(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  (not delay_to_act_" ^state_name^" ) set_"^state_name^"_DELAY" ^vacuity_unchanged^")" ^    
        "\n(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")" ^                 
        "\n(and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"  act_to_err_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")" ^                 
        
        "\n(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"   act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  set_"^state_name^"_TRIG) " ^                 
        "\n(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"   act_to_idle_" ^state_name^"   (not idle_to_trig_" ^state_name^") set_"^state_name^"_IDLE) " ^                                  
        "\n(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"   (not act_to_idle_" ^state_name^") set_"^state_name^"_ACTION" ^vacuity_unchanged^")" ^                 
        "\n(and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")" ^       

        "\n(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_idle_" ^state_name^"   idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"   set_"^state_name^"_DELAY)"^                 
        "\n(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_idle_" ^state_name^"   idle_to_trig_" ^state_name^"  (not trig_to_delay_" ^state_name^") set_"^state_name^"_TRIG)"^                 
        "\n(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   act_to_idle_" ^state_name^"   (not idle_to_trig_" ^state_name^") set_"^state_name^"_IDLE)"^                 
        "\n(and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"   (not act_to_idle_" ^state_name^") set_"^state_name^"_ACTION" ^vacuity_unchanged^")"^                 


        "\n(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"  set_"^state_name^"_ACTION)" ^
        "\n(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  (not delay_to_act_" ^state_name^") set_"^state_name^"_DELAY)" ^
        "\n(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  (not trig_to_delay_" ^state_name^") set_"^state_name^"_TRIG)" ^
        "\n(and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"   delay_to_err_" ^state_name^" set_"^state_name^"_ERR)" 
        else "")^
        (*three transiations in one tick*)
        (if (tmin_is_nul && lmin_is_nul && not amin_is_nul) then    
        "\n;additional transitions because tmin and lmin are equals to 0\n;this is the encoding of a IDLE to ACTION state in one tick 
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"  set_"^state_name^"_ACTION" ^vacuity_unchanged^")"^
        "\n;this is the encoding of a IDLE to DELAY in one tick 
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"   trig_to_delay_" ^state_name^"  (not delay_to_act_" ^state_name^") set_"^state_name^"_DELAY" ^vacuity_unchanged^")"^
        "\n;this is the encoding of TRIG TO ACTION in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"  set_"^state_name^"_ACTION" ^vacuity_unchanged^")"^
        "\n;this is the encoding of a IDLE to ERR in one tick 
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"   trig_to_delay_" ^state_name^"  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^")"^
        "\n;this is the encoding of TRIG to ERR in one tick  
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR" ^vacuity_unchanged^") "
        else "")^
        (if (lmin_is_nul && amin_is_nul && not tmin_is_nul) then
        "\n;additional transitions because lmin and amin are equals to 0\n;this is the encoding of a TRIG to IDLE state in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"   act_to_idle_" ^state_name^"   set_"^state_name^"_IDLE)"^
        "\n;this is the encoding of a TRIG to ACTION state in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"   (not act_to_idle_" ^state_name^")  set_"^state_name^"_ACTION" ^vacuity_unchanged^")"^
        "\n;this is the encoding of a DELAY to IDLE state in one tick 
        (and is_" ^ state_name ^ "DELAY  delay_to_act_" ^state_name^"  act_to_idle_" ^state_name^"   set_"^state_name^"_IDLE)" ^
        "\n;this is the encoding of a TRIG to ERR state in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_err_" ^state_name^"   set_"^state_name^"_ERR " ^vacuity_unchanged^") "^
        "\n;this is the encoding of a TRIG to ERR state in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  act_to_idle_" ^state_name^"   act_to_err_" ^state_name^"   set_"^state_name^"_ERR)"
        else "")^  
        (if (amin_is_nul && tmin_is_nul && not lmin_is_nul) then
        "\n;additional transitions because tmin and amin are equals to 0\n;this is the encoding of a ACTION to DELAY state in one tick 
        (and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"   trig_to_delay_" ^state_name^"   set_"^state_name^"_DELAY)" ^
        "\n;this is the encoding of a ACTION to TRIG state in one tick
          (and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^"   (not trig_to_delay_" ^state_name^")  set_"^state_name^"_TRIG)" ^
        "\n;this is the encoding of a IDLE to DELAY state in one tick 
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^" trig_to_delay_" ^state_name^"    set_"^state_name^"_DELAY  " ^vacuity_unchanged^") "^
        "\n;this is the encoding of a DELAY to TRIG state in one tick 
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^" act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^ "  trig_to_delay_" ^state_name^ "  set_"^state_name^"_DELAY) " ^
        "\n;this is the encoding of a DELAY to TRIG state in one tick 
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^" act_to_idle_" ^state_name^"  idle_to_trig_" ^state_name^ " (not trig_to_delay_" ^state_name^ ")  set_"^state_name^"_TRIG) " ^
        "\n;this is the encoding of a DELAY to IDLE state in one tick 
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^" act_to_idle_" ^state_name^"  (not idle_to_trig_" ^state_name^ ")  set_"^state_name^"_IDLE) " 
        else "")^
        (* two transitions in one tick *)
        (if (tmin_is_nul && not lmin_is_nul && not amin_is_nul) then
        "\n;additional transitions because tmin is equal to 0\n;this is the encoding of a IDLE to DELAY state in one tick 
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  trig_to_delay_" ^state_name^"   set_"^state_name^"_TRIG)"
        else "")^
        (if (not tmin_is_nul &&  lmin_is_nul && not amin_is_nul) then
        "\n;additional transitions because lmin is equal to 0\n;this is the encoding of a TRIG to ACTION state in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_act_" ^state_name^"   set_"^state_name^"_ACTION)"^
        "\n;this is the encoding of a TRIG to ACTION state in one tick 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  delay_to_err_" ^state_name^"   set_"^state_name^"_ERR)"
        else "")^
        (if (not tmin_is_nul &&  not lmin_is_nul && amin_is_nul) then
        "\n;additional transitions because amin is equal to 0\n;this is the encoding of a DELAY to ACTION state in one tick 
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"  act_to_idle_" ^state_name^"   set_"^state_name^"_IDLE)" ^
        "\n;this is the encoding of a DELAY to ERR state in one tick 
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"  act_to_err_" ^state_name^"   set_"^state_name^"_ERR)"
        else "")^
        (* single transition in one tick *)
        "\n;this is the encoding of single state change.
        (and is_" ^ state_name ^ "_IDLE  stay_idle_" ^state_name^"  set_"^state_name^"_IDLE " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_IDLE  idle_to_trig_" ^state_name^"  " ^ check_stop_after_trig ^ " set_"^state_name^"_TRIG " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_TRIG  trig_to_idle_" ^state_name^"  set_"^state_name^"_IDLE " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_TRIG  stay_trig_" ^state_name^"  set_"^state_name^"_TRIG " ^vacuity_unchanged^") 
        (and is_" ^ state_name ^ "_TRIG  trig_to_delay_" ^state_name^"  "^ check_stop_after_delay ^"set_"^state_name^"_DELAY " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_DELAY  stay_delay_" ^state_name^"  set_"^state_name^"_DELAY " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_DELAY  delay_to_act_" ^state_name^"  " ^ check_stop_after_action ^ " set_"^state_name^"_ACTION " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_ACTION  stay_act_" ^state_name^"  set_"^state_name^"_ACTION " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_ACTION  act_to_idle_" ^state_name^"  set_"^state_name^"_IDLE)
        (and is_" ^ state_name ^ "_DELAY  delay_to_err_" ^state_name^"  set_"^state_name^"_ERR " ^vacuity_unchanged^")
        (and is_" ^ state_name ^ "_ACTION  act_to_err_" ^state_name^"   set_"^state_name^"_ERR " ^vacuity_unchanged^")
        ) :trans true))" 
      end
      )  in   
  Format.fprintf fmt "%s@\n" state_trans;
  "is_" ^ state_name ^ "_ERR"

(** [generate_var_decl fmt decl] generates the declaration of variable [decl] used in the requirements in the formatter [fmt] *)
let generate_var_decl fmt decl =  
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
let generate_intermediate_var_decl fmt (var_name :string) check_rt_consistency  =
  generate_variable_and_its_next_value fmt var_name "Bool" "false" false check_rt_consistency;
  Format.fprintf fmt "(define-fun %s_unchanged () Bool (= %s_n  %s ))" var_name var_name var_name

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
let generate_sup fmt req_name sup_index (sup : Sup_types.sup_req) (args: Input_args.t) =
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
  generate_SUP_content fmt sup_index req_name t.tmin t.tmax sup.d.lmin sup.d.lmax a.amin a.amax args
  
  
(** [generate_sup_list fmt req_name (_, req_sups_list)] generates the conversion in the formatter [fmt] of the SUP 
    list [req_sups_list] corresponding to a parsed requirement with name [req_name] in SUP format into VMT lib format *)
let generate_sup_list fmt req_name (_, req_sups_list) (args : Input_args.t)=
  (*generate sups*)
  Format.fprintf fmt "@\n\n\n;generation of the SUP state machine for requirement %s @\n" req_name;
  List.fold_left (fun acc sup -> (generate_sup fmt req_name (List.length acc) sup args)::acc) [] req_sups_list
  
(** [generate_requirements fmt t] generates the conversion of the parsed requirements [t] in SUP format into VMT lib format
    in the formatter [fmt] *)
let generate_requirements fmt (t:Parse.t) (args : Input_args.t) = 
  (*print original variables*)
  Format.fprintf fmt "@\n;these are the variables used in triggers and actions@\n";
  let vars = t.vars in
  let vars_decl = List.of_seq ( Hashtbl.to_seq_values vars ) in
  List.iter (fun decl -> generate_var_decl fmt decl) vars_decl;
  let ( generated_variables,(intermediate_variables:string list), sup_map) =  Sup.of_req t args in 

  (*print generated variables used to convert non boolean expressions into boolean*)
  if (List.length generated_variables) > 0 then(    
    Format.fprintf fmt "@\n;these are generated variables to replace non boolean expressions @\n";
    List.iter (fun var_name -> (generate_generated_var_decl fmt var_name)) generated_variables
  );

  (*print generated variables used to convert requirements to SUP*)
  if (List.length intermediate_variables) > 0 then (
    Format.fprintf fmt "@\n;these are generated variables to model Before, After ... requirements @\n";
    List.iter (fun var_name -> (generate_intermediate_var_decl fmt var_name args.check_rt_consistency)) intermediate_variables
  );

  (*print the SUPs and get the list of functions that detects error status*)
  Format.fprintf fmt "@\n;these are generated SUPs @\n";
  let list_error_func = Sup.SMap.fold (fun key value acc -> (generate_sup_list fmt key value args)@acc) sup_map [] in
 
  (*creates an invariant property with all error status funtions*)
  let all_func_error = List.fold_left (fun acc s -> (" (not "^s ^") ")^acc) "" list_error_func in
  let invar_prop = "(define-fun .all_sup_status () Bool (! (and true true "^all_func_error^")" in 
  generate_invariant fmt invar_prop "))" ;
  Format.fprintf fmt "@\n"

(** [generate_vmt_file fmt t] generates a file in the vmt-lib format containing the parsed requirements [t] in the formatter [fmt]*)
let generate_vmt_file fmt t (args : Input_args.t)=
  generate_state fmt args;
  generate_requirements fmt t args;
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "(assert true)@\n"

 