open Src.Input_args

let get_args file =
  { output_fmt = VMT;
  state_enc = BooleanEncoding;
  clock_t = IntegerClock;
  clock_mult = 10;
  only_bool_predicates = false ;
  input_file = Some file;
  input_dir= None;
  keep_simple = false;
  check_non_vacuity = [];
  check_rt_consistency = false
  } 

let convert_to_absolute_path f= 
  let dir = Filename.dirname (Sys.getcwd()) in
  let rec go_up d =
    let l = List.rev (String.split_on_char '/' d) in 
      match l with
      | _::[]
      | [] -> assert false
      | h::t -> (let new_dir  =String.concat "/" (List.rev t) in if String.equal (h) "_build" then new_dir else go_up new_dir)
  in (go_up dir)^"/"^f

let exec args =
  try 
  let t =  (Src.Parse.of_file (Option.get args.input_file)) in 
  let fmt = Format.get_std_formatter() in
  (match args.output_fmt with
    | NuSMV  -> Src.Sup.generate_sup_file fmt t args
    | VMT  -> Src.Vmt.generate_vmt_file fmt t args);
  with Src.Parse.ParseException msg -> Format.printf "%s@." msg 


let%expect_test "1.req" =
  let f = convert_to_absolute_path "tests/inputs/1.req" in
  let args =get_args f in 
  exec args;
  [%expect {|
    ;this boolean variables are used to define the state of the SUP;  bool err, loc0, loc1;
             ;
             ;  (err, loc0, loc1)
             ;  000 IDLE
             ;  001 TRIG
             ;  010 DELAY
             ;  011 ACTION
             ;  1__ ERR
            ;some helper functions for SUP status
    (define-fun is_IDLE ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err) (not loc0) (not loc1)))
    (define-fun is_TRIG ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err) (not loc0) loc1))
    (define-fun is_DELAY ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err) loc0 (not loc1)))
    (define-fun is_ACTION ( (err Bool) (loc0 Bool) (loc1 Bool) ) Bool (and (not err)  loc0  loc1 ))
    (define-fun is_ERR ( (err Bool) ) Bool (= true err) )

    ;these are the variables used in triggers and actions
    (declare-fun x0004 () Bool)
    (declare-fun x0006 () Bool)
    (declare-fun x0005 () Bool)



    ;generation of the SUP state machine for requirement ID004
    ;these are the function to access SUP attributes (state, time counter, trigger/delay/action time
    (declare-fun state_ID004_0_err () Bool)
    (declare-fun state_ID004_0_err_n () Bool)
    (define-fun .state_ID004_0_err_sv0 () Bool (!  state_ID004_0_err :next state_ID004_0_err_n))
    (define-fun .state_ID004_0_err_init () Bool (! (= state_ID004_0_err false) :init true))
    (declare-fun state_ID004_0_loc0 () Bool)
    (declare-fun state_ID004_0_loc0_n () Bool)
    (define-fun .state_ID004_0_loc0_sv0 () Bool (!  state_ID004_0_loc0 :next state_ID004_0_loc0_n))
    (define-fun .state_ID004_0_loc0_init () Bool (! (= state_ID004_0_loc0 false) :init true))
    (declare-fun state_ID004_0_loc1 () Bool)
    (declare-fun state_ID004_0_loc1_n () Bool)
    (define-fun .state_ID004_0_loc1_sv0 () Bool (!  state_ID004_0_loc1 :next state_ID004_0_loc1_n))
    (define-fun .state_ID004_0_loc1_init () Bool (! (= state_ID004_0_loc1 false) :init true))
    (define-fun is_state_ID004_0_IDLE () Bool (is_IDLE state_ID004_0_err state_ID004_0_loc0 state_ID004_0_loc1) )
    (define-fun is_state_ID004_0_TRIG () Bool (is_TRIG state_ID004_0_err state_ID004_0_loc0 state_ID004_0_loc1) )
    (define-fun is_state_ID004_0_DELAY () Bool (is_DELAY state_ID004_0_err state_ID004_0_loc0 state_ID004_0_loc1) )
    (define-fun is_state_ID004_0_ACTION () Bool (is_ACTION state_ID004_0_err state_ID004_0_loc0 state_ID004_0_loc1) )
    (define-fun is_state_ID004_0_ERR () Bool (is_ERR state_ID004_0_err) )
    (define-fun set_state_ID004_0_IDLE () Bool (is_IDLE state_ID004_0_err_n state_ID004_0_loc0_n state_ID004_0_loc1_n) )
    (define-fun set_state_ID004_0_TRIG () Bool (is_TRIG state_ID004_0_err_n state_ID004_0_loc0_n state_ID004_0_loc1_n) )
    (define-fun set_state_ID004_0_DELAY () Bool (is_DELAY state_ID004_0_err_n state_ID004_0_loc0_n state_ID004_0_loc1_n) )
    (define-fun set_state_ID004_0_ACTION () Bool (is_ACTION state_ID004_0_err_n state_ID004_0_loc0_n state_ID004_0_loc1_n) )
    (define-fun set_state_ID004_0_ERR () Bool (is_ERR state_ID004_0_err_n) )

    ;these are the function to explicit SUP time counter initial value and transition
    (declare-fun c_ID004_0 () Int)
    (declare-fun c_ID004_0_n () Int)
    (define-fun .c_ID004_0_sv0 () Int (!  c_ID004_0 :nextclock c_ID004_0_n))
    (define-fun .c_ID004_0_init () Bool (! (= c_ID004_0 0) :init true))
    (define-fun c_ID004_0_unchanged () Bool (= c_ID004_0_n c_ID004_0))
    (define-fun c_ID004_0_reset () Bool (= c_ID004_0_n 0))

    ;these are the function to access SUP attributes (state, time counter, trigger/delay/action time
    (declare-fun state_ID004_1_err () Bool)
    (declare-fun state_ID004_1_err_n () Bool)
    (define-fun .state_ID004_1_err_sv0 () Bool (!  state_ID004_1_err :next state_ID004_1_err_n))
    (define-fun .state_ID004_1_err_init () Bool (! (= state_ID004_1_err false) :init true))
    (declare-fun state_ID004_1_loc0 () Bool)
    (declare-fun state_ID004_1_loc0_n () Bool)
    (define-fun .state_ID004_1_loc0_sv0 () Bool (!  state_ID004_1_loc0 :next state_ID004_1_loc0_n))
    (define-fun .state_ID004_1_loc0_init () Bool (! (= state_ID004_1_loc0 false) :init true))
    (declare-fun state_ID004_1_loc1 () Bool)
    (declare-fun state_ID004_1_loc1_n () Bool)
    (define-fun .state_ID004_1_loc1_sv0 () Bool (!  state_ID004_1_loc1 :next state_ID004_1_loc1_n))
    (define-fun .state_ID004_1_loc1_init () Bool (! (= state_ID004_1_loc1 false) :init true))
    (define-fun is_state_ID004_1_IDLE () Bool (is_IDLE state_ID004_1_err state_ID004_1_loc0 state_ID004_1_loc1) )
    (define-fun is_state_ID004_1_TRIG () Bool (is_TRIG state_ID004_1_err state_ID004_1_loc0 state_ID004_1_loc1) )
    (define-fun is_state_ID004_1_DELAY () Bool (is_DELAY state_ID004_1_err state_ID004_1_loc0 state_ID004_1_loc1) )
    (define-fun is_state_ID004_1_ACTION () Bool (is_ACTION state_ID004_1_err state_ID004_1_loc0 state_ID004_1_loc1) )
    (define-fun is_state_ID004_1_ERR () Bool (is_ERR state_ID004_1_err) )
    (define-fun set_state_ID004_1_IDLE () Bool (is_IDLE state_ID004_1_err_n state_ID004_1_loc0_n state_ID004_1_loc1_n) )
    (define-fun set_state_ID004_1_TRIG () Bool (is_TRIG state_ID004_1_err_n state_ID004_1_loc0_n state_ID004_1_loc1_n) )
    (define-fun set_state_ID004_1_DELAY () Bool (is_DELAY state_ID004_1_err_n state_ID004_1_loc0_n state_ID004_1_loc1_n) )
    (define-fun set_state_ID004_1_ACTION () Bool (is_ACTION state_ID004_1_err_n state_ID004_1_loc0_n state_ID004_1_loc1_n) )
    (define-fun set_state_ID004_1_ERR () Bool (is_ERR state_ID004_1_err_n) )

    ;these are the function to explicit SUP time counter initial value and transition
    (declare-fun c_ID004_1 () Int)
    (declare-fun c_ID004_1_n () Int)
    (define-fun .c_ID004_1_sv0 () Int (!  c_ID004_1 :nextclock c_ID004_1_n))
    (define-fun .c_ID004_1_init () Bool (! (= c_ID004_1 0) :init true))
    (define-fun c_ID004_1_unchanged () Bool (= c_ID004_1_n c_ID004_1))
    (define-fun c_ID004_1_reset () Bool (= c_ID004_1_n 0))

    ;these are the function to access SUP attributes (state, time counter, trigger/delay/action time
    (declare-fun state_ID004_2_err () Bool)
    (declare-fun state_ID004_2_err_n () Bool)
    (define-fun .state_ID004_2_err_sv0 () Bool (!  state_ID004_2_err :next state_ID004_2_err_n))
    (define-fun .state_ID004_2_err_init () Bool (! (= state_ID004_2_err false) :init true))
    (declare-fun state_ID004_2_loc0 () Bool)
    (declare-fun state_ID004_2_loc0_n () Bool)
    (define-fun .state_ID004_2_loc0_sv0 () Bool (!  state_ID004_2_loc0 :next state_ID004_2_loc0_n))
    (define-fun .state_ID004_2_loc0_init () Bool (! (= state_ID004_2_loc0 false) :init true))
    (declare-fun state_ID004_2_loc1 () Bool)
    (declare-fun state_ID004_2_loc1_n () Bool)
    (define-fun .state_ID004_2_loc1_sv0 () Bool (!  state_ID004_2_loc1 :next state_ID004_2_loc1_n))
    (define-fun .state_ID004_2_loc1_init () Bool (! (= state_ID004_2_loc1 false) :init true))
    (define-fun is_state_ID004_2_IDLE () Bool (is_IDLE state_ID004_2_err state_ID004_2_loc0 state_ID004_2_loc1) )
    (define-fun is_state_ID004_2_TRIG () Bool (is_TRIG state_ID004_2_err state_ID004_2_loc0 state_ID004_2_loc1) )
    (define-fun is_state_ID004_2_DELAY () Bool (is_DELAY state_ID004_2_err state_ID004_2_loc0 state_ID004_2_loc1) )
    (define-fun is_state_ID004_2_ACTION () Bool (is_ACTION state_ID004_2_err state_ID004_2_loc0 state_ID004_2_loc1) )
    (define-fun is_state_ID004_2_ERR () Bool (is_ERR state_ID004_2_err) )
    (define-fun set_state_ID004_2_IDLE () Bool (is_IDLE state_ID004_2_err_n state_ID004_2_loc0_n state_ID004_2_loc1_n) )
    (define-fun set_state_ID004_2_TRIG () Bool (is_TRIG state_ID004_2_err_n state_ID004_2_loc0_n state_ID004_2_loc1_n) )
    (define-fun set_state_ID004_2_DELAY () Bool (is_DELAY state_ID004_2_err_n state_ID004_2_loc0_n state_ID004_2_loc1_n) )
    (define-fun set_state_ID004_2_ACTION () Bool (is_ACTION state_ID004_2_err_n state_ID004_2_loc0_n state_ID004_2_loc1_n) )
    (define-fun set_state_ID004_2_ERR () Bool (is_ERR state_ID004_2_err_n) )

    ;these are the function to explicit SUP time counter initial value and transition
    (declare-fun c_ID004_2 () Int)
    (declare-fun c_ID004_2_n () Int)
    (define-fun .c_ID004_2_sv0 () Int (!  c_ID004_2 :nextclock c_ID004_2_n))
    (define-fun .c_ID004_2_init () Bool (! (= c_ID004_2 0) :init true))
    (define-fun c_ID004_2_unchanged () Bool (= c_ID004_2_n c_ID004_2))
    (define-fun c_ID004_2_reset () Bool (= c_ID004_2_n 0))

    ;these are the function to access SUP attributes (state, time counter, trigger/delay/action time
    (declare-fun state_ID004_3_err () Bool)
    (declare-fun state_ID004_3_err_n () Bool)
    (define-fun .state_ID004_3_err_sv0 () Bool (!  state_ID004_3_err :next state_ID004_3_err_n))
    (define-fun .state_ID004_3_err_init () Bool (! (= state_ID004_3_err false) :init true))
    (declare-fun state_ID004_3_loc0 () Bool)
    (declare-fun state_ID004_3_loc0_n () Bool)
    (define-fun .state_ID004_3_loc0_sv0 () Bool (!  state_ID004_3_loc0 :next state_ID004_3_loc0_n))
    (define-fun .state_ID004_3_loc0_init () Bool (! (= state_ID004_3_loc0 false) :init true))
    (declare-fun state_ID004_3_loc1 () Bool)
    (declare-fun state_ID004_3_loc1_n () Bool)
    (define-fun .state_ID004_3_loc1_sv0 () Bool (!  state_ID004_3_loc1 :next state_ID004_3_loc1_n))
    (define-fun .state_ID004_3_loc1_init () Bool (! (= state_ID004_3_loc1 false) :init true))
    (define-fun is_state_ID004_3_IDLE () Bool (is_IDLE state_ID004_3_err state_ID004_3_loc0 state_ID004_3_loc1) )
    (define-fun is_state_ID004_3_TRIG () Bool (is_TRIG state_ID004_3_err state_ID004_3_loc0 state_ID004_3_loc1) )
    (define-fun is_state_ID004_3_DELAY () Bool (is_DELAY state_ID004_3_err state_ID004_3_loc0 state_ID004_3_loc1) )
    (define-fun is_state_ID004_3_ACTION () Bool (is_ACTION state_ID004_3_err state_ID004_3_loc0 state_ID004_3_loc1) )
    (define-fun is_state_ID004_3_ERR () Bool (is_ERR state_ID004_3_err) )
    (define-fun set_state_ID004_3_IDLE () Bool (is_IDLE state_ID004_3_err_n state_ID004_3_loc0_n state_ID004_3_loc1_n) )
    (define-fun set_state_ID004_3_TRIG () Bool (is_TRIG state_ID004_3_err_n state_ID004_3_loc0_n state_ID004_3_loc1_n) )
    (define-fun set_state_ID004_3_DELAY () Bool (is_DELAY state_ID004_3_err_n state_ID004_3_loc0_n state_ID004_3_loc1_n) )
    (define-fun set_state_ID004_3_ACTION () Bool (is_ACTION state_ID004_3_err_n state_ID004_3_loc0_n state_ID004_3_loc1_n) )
    (define-fun set_state_ID004_3_ERR () Bool (is_ERR state_ID004_3_err_n) )

    ;these are the function to explicit SUP time counter initial value and transition
    (declare-fun c_ID004_3 () Int)
    (declare-fun c_ID004_3_n () Int)
    (define-fun .c_ID004_3_sv0 () Int (!  c_ID004_3 :nextclock c_ID004_3_n))
    (define-fun .c_ID004_3_init () Bool (! (= c_ID004_3 0) :init true))
    (define-fun c_ID004_3_unchanged () Bool (= c_ID004_3_n c_ID004_3))
    (define-fun c_ID004_3_reset () Bool (= c_ID004_3_n 0))

    ;these are the function to access SUP attributes (state, time counter, trigger/delay/action time
    (declare-fun state_ID004_4_err () Bool)
    (declare-fun state_ID004_4_err_n () Bool)
    (define-fun .state_ID004_4_err_sv0 () Bool (!  state_ID004_4_err :next state_ID004_4_err_n))
    (define-fun .state_ID004_4_err_init () Bool (! (= state_ID004_4_err false) :init true))
    (declare-fun state_ID004_4_loc0 () Bool)
    (declare-fun state_ID004_4_loc0_n () Bool)
    (define-fun .state_ID004_4_loc0_sv0 () Bool (!  state_ID004_4_loc0 :next state_ID004_4_loc0_n))
    (define-fun .state_ID004_4_loc0_init () Bool (! (= state_ID004_4_loc0 false) :init true))
    (declare-fun state_ID004_4_loc1 () Bool)
    (declare-fun state_ID004_4_loc1_n () Bool)
    (define-fun .state_ID004_4_loc1_sv0 () Bool (!  state_ID004_4_loc1 :next state_ID004_4_loc1_n))
    (define-fun .state_ID004_4_loc1_init () Bool (! (= state_ID004_4_loc1 false) :init true))
    (define-fun is_state_ID004_4_IDLE () Bool (is_IDLE state_ID004_4_err state_ID004_4_loc0 state_ID004_4_loc1) )
    (define-fun is_state_ID004_4_TRIG () Bool (is_TRIG state_ID004_4_err state_ID004_4_loc0 state_ID004_4_loc1) )
    (define-fun is_state_ID004_4_DELAY () Bool (is_DELAY state_ID004_4_err state_ID004_4_loc0 state_ID004_4_loc1) )
    (define-fun is_state_ID004_4_ACTION () Bool (is_ACTION state_ID004_4_err state_ID004_4_loc0 state_ID004_4_loc1) )
    (define-fun is_state_ID004_4_ERR () Bool (is_ERR state_ID004_4_err) )
    (define-fun set_state_ID004_4_IDLE () Bool (is_IDLE state_ID004_4_err_n state_ID004_4_loc0_n state_ID004_4_loc1_n) )
    (define-fun set_state_ID004_4_TRIG () Bool (is_TRIG state_ID004_4_err_n state_ID004_4_loc0_n state_ID004_4_loc1_n) )
    (define-fun set_state_ID004_4_DELAY () Bool (is_DELAY state_ID004_4_err_n state_ID004_4_loc0_n state_ID004_4_loc1_n) )
    (define-fun set_state_ID004_4_ACTION () Bool (is_ACTION state_ID004_4_err_n state_ID004_4_loc0_n state_ID004_4_loc1_n) )
    (define-fun set_state_ID004_4_ERR () Bool (is_ERR state_ID004_4_err_n) )

    ;these are the function to explicit SUP time counter initial value and transition
    (declare-fun c_ID004_4 () Int)
    (declare-fun c_ID004_4_n () Int)
    (define-fun .c_ID004_4_sv0 () Int (!  c_ID004_4 :nextclock c_ID004_4_n))
    (define-fun .c_ID004_4_init () Bool (! (= c_ID004_4 0) :init true))
    (define-fun c_ID004_4_unchanged () Bool (= c_ID004_4_n c_ID004_4))
    (define-fun c_ID004_4_reset () Bool (= c_ID004_4_n 0))


    ;these are error functions
    (define-fun .all_sup_status () Bool (! (and true true  (not is_state_ID004_0_ERR)  (not is_state_ID004_1_ERR)  (not is_state_ID004_2_ERR)  (not is_state_ID004_3_ERR)  (not is_state_ID004_4_ERR) ) :invar-property 0))


    ;these are generated variables to model Before, After ... requirements
    (declare-fun intermediate0 () Bool)
    (declare-fun intermediate0_n () Bool)
    (define-fun .intermediate0_sv0 () Bool (!  intermediate0 :next intermediate0_n))
    (define-fun .intermediate0_init () Bool (! (= intermediate0 false) :init true))
    (define-fun intermediate0_unchanged () Bool (= intermediate0_n  intermediate0 ))
    (declare-fun intermediate1 () Bool)
    (declare-fun intermediate1_n () Bool)
    (define-fun .intermediate1_sv0 () Bool (!  intermediate1 :next intermediate1_n))
    (define-fun .intermediate1_init () Bool (! (= intermediate1 false) :init true))
    (define-fun intermediate1_unchanged () Bool (= intermediate1_n  intermediate1 ))

    ;these are generated SUPs



    ;generation of the SUP state machine for requirement ID004
    ; sup  [ x0004 , x0004 , x0004 , 0, 0, 0, 0, True, intermediate1 , True, -1, -1 ]
    (define-fun tse_ID004_0 () Bool x0004 )
    (define-fun tc_ID004_0 () Bool x0004 )
    (define-fun tee_ID004_0 () Bool x0004 )
    (define-fun ase_ID004_0 () Bool true)
    (define-fun ac_ID004_0 () Bool intermediate1_n )
    (define-fun aee_ID004_0 () Bool true)
    ; guards of the SUP transitions + clock reset/not changed
    (define-fun stay_idle_state_ID004_0 () Bool ( and (not tse_ID004_0) c_ID004_0_unchanged ))
    (define-fun idle_to_trig_state_ID004_0 () Bool ( and tse_ID004_0 c_ID004_0_reset  ))
    (define-fun trig_to_idle_state_ID004_0 () Bool ( and ( or ( and (not tee_ID004_0) (not tc_ID004_0)) (and (not tc_ID004_0) (< c_ID004_0 0 ) ) (and (not tee_ID004_0) (>= c_ID004_0 0 )) (> c_ID004_0 0 )) c_ID004_0_reset))
    (define-fun stay_trig_state_ID004_0 () Bool  ( and (  and tc_ID004_0 (< c_ID004_0 0 ) ( or ( not tee_ID004_0)  (< c_ID004_0 0 ))) c_ID004_0_unchanged ))
    (define-fun trig_to_delay_state_ID004_0 () Bool ( and (and tee_ID004_0 (>= c_ID004_0 0 ) (<= c_ID004_0 0 )) c_ID004_0_reset ))
    (define-fun stay_delay_state_ID004_0 () Bool (  and (and (< c_ID004_0 0 ) ( or (not ase_ID004_0) (< c_ID004_0 0 ))) c_ID004_0_unchanged ))
    (define-fun delay_to_err_state_ID004_0 () Bool ( and (and (not ase_ID004_0) (>= c_ID004_0 0 )) c_ID004_0_unchanged ))
    (define-fun stay_act_state_ID004_0 () Bool ( and (and ac_ID004_0   true  (or (not aee_ID004_0)  true )) c_ID004_0_unchanged ))
    (define-fun act_to_err_state_ID004_0 () Bool ( and (or (and (not ac_ID004_0) (not aee_ID004_0)) (and (not ac_ID004_0)  true ) (and (not aee_ID004_0)  false )  false ) c_ID004_0_unchanged ))
    (define-fun act_to_idle_state_ID004_0 () Bool ( and (and aee_ID004_0  false   true )  c_ID004_0_reset ))
    (define-fun delay_to_act_state_ID004_0 () Bool ( and ( and ase_ID004_0 (>= c_ID004_0 0 ) (<= c_ID004_0 0 )) c_ID004_0_reset ))
    ; guards of the SUP transitions without clock delay
    (define-fun trig_to_delay_no_clock_state_ID004_0 () Bool tee_ID004_0 )
    (define-fun delay_to_err_no_clock_state_ID004_0 () Bool (not ase_ID004_0)  )
    (define-fun idle_to_trig_no_clock_state_ID004_0 () Bool tse_ID004_0)
    (define-fun delay_to_act_no_clock_state_ID004_0 () Bool ase_ID004_0 )
    (define-fun act_to_err_no_clock_state_ID004_0 () Bool (not ac_ID004_0) )
    (define-fun act_to_idle_no_clock_state_ID004_0 () Bool false)

    ; SUP transitions
    (define-fun .state_ID004_0_trans () Bool (!  (or

    ; Combined transitions
    ; IDLE -> TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_0_IDLE idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0 delay_to_act_no_clock_state_ID004_0 act_to_idle_no_clock_state_ID004_0 set_state_ID004_0_IDLE)
    ; IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_0_IDLE  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0  delay_to_err_state_ID004_0  set_state_ID004_0_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT -> ERR
    (and is_state_ID004_0_IDLE  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0  delay_to_act_no_clock_state_ID004_0  act_to_err_no_clock_state_ID004_0  set_state_ID004_0_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_0_IDLE  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0  delay_to_act_no_clock_state_ID004_0 ac_ID004_0 set_state_ID004_0_ACTION)
    ; IDLE -> TRIG -> DELAY
    (and is_state_ID004_0_IDLE  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0  false set_state_ID004_0_DELAY)
    ; IDLE -> TRIG
    (and is_state_ID004_0_IDLE  idle_to_trig_state_ID004_0 (and tee_ID004_0 (not trig_to_delay_no_clock_state_ID004_0)) set_state_ID004_0_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_0_TRIG  trig_to_delay_state_ID004_0  delay_to_act_no_clock_state_ID004_0   act_to_idle_no_clock_state_ID004_0  idle_to_trig_state_ID004_0  set_state_ID004_0_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_0_TRIG  trig_to_delay_state_ID004_0  delay_to_act_no_clock_state_ID004_0   act_to_idle_no_clock_state_ID004_0 (or (not tse_ID004_0) (not tee_ID004_0)) set_state_ID004_0_IDLE)
    ; TRIG -> DELAY -> ACT
    (and is_state_ID004_0_TRIG  trig_to_delay_state_ID004_0  delay_to_act_no_clock_state_ID004_0   ac_ID004_0 set_state_ID004_0_ACTION)
    ; TRIG -> DELAY -> ERR
    (and is_state_ID004_0_TRIG  trig_to_delay_state_ID004_0  delay_to_err_no_clock_state_ID004_0  set_state_ID004_0_ERR)
    ; TRIG -> DELAY
    (and is_state_ID004_0_TRIG  trig_to_delay_state_ID004_0  false  set_state_ID004_0_ERR)
    ; DELAY -> ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_0_DELAY  delay_to_act_state_ID004_0   act_to_idle_no_clock_state_ID004_0   idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0   set_state_ID004_0_DELAY )
    ; DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_0_DELAY  delay_to_act_state_ID004_0   act_to_idle_no_clock_state_ID004_0   idle_to_trig_state_ID004_0 (and tee_ID004_0 (not trig_to_delay_no_clock_state_ID004_0)) set_state_ID004_0_TRIG)
    ; DELAY -> ACT -> IDLE
    (and is_state_ID004_0_DELAY  delay_to_act_state_ID004_0   act_to_idle_no_clock_state_ID004_0   (or (not tse_ID004_0) (not tee_ID004_0)) set_state_ID004_0_IDLE)
    ; DELAY -> ACT
    (and is_state_ID004_0_DELAY  delay_to_act_state_ID004_0   ac_ID004_0 set_state_ID004_0_ACTION)
    ; DELAY -> ACT -> ERR
    (and is_state_ID004_0_DELAY  delay_to_act_state_ID004_0   act_to_err_no_clock_state_ID004_0 set_state_ID004_0_ERR)
    ; ACT -> IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_0_ACTION  act_to_idle_state_ID004_0  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0  delay_to_act_no_clock_state_ID004_0  set_state_ID004_0_ACTION)
    ; ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_0_ACTION  act_to_idle_state_ID004_0  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0  false set_state_ID004_0_DELAY)
    ; ACT -> IDLE -> TRIG
    (and is_state_ID004_0_ACTION  act_to_idle_state_ID004_0  idle_to_trig_state_ID004_0  (and tee_ID004_0 (not trig_to_delay_no_clock_state_ID004_0)) set_state_ID004_0_TRIG)
    ; ACT -> IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_0_ACTION  act_to_idle_state_ID004_0  idle_to_trig_state_ID004_0  trig_to_delay_no_clock_state_ID004_0   delay_to_err_no_clock_state_ID004_0 set_state_ID004_0_ERR)

    ; Single transitions:
            (and is_state_ID004_0_IDLE  stay_idle_state_ID004_0  set_state_ID004_0_IDLE )
            (and is_state_ID004_0_IDLE  idle_to_trig_state_ID004_0  (and tee_ID004_0 (not trig_to_delay_no_clock_state_ID004_0)) set_state_ID004_0_TRIG )
            (and is_state_ID004_0_TRIG  trig_to_idle_state_ID004_0 (not idle_to_trig_no_clock_state_ID004_0) set_state_ID004_0_IDLE )
            (and is_state_ID004_0_TRIG  stay_trig_state_ID004_0  set_state_ID004_0_TRIG )
            (and is_state_ID004_0_TRIG  trig_to_delay_state_ID004_0  false set_state_ID004_0_DELAY )
            (and is_state_ID004_0_DELAY  stay_delay_state_ID004_0  set_state_ID004_0_DELAY )
            (and is_state_ID004_0_DELAY  delay_to_act_state_ID004_0  ac_ID004_0 set_state_ID004_0_ACTION )
            (and is_state_ID004_0_ACTION  stay_act_state_ID004_0  set_state_ID004_0_ACTION )
            (and is_state_ID004_0_ACTION  act_to_idle_state_ID004_0  set_state_ID004_0_IDLE)
            (and is_state_ID004_0_DELAY  delay_to_err_state_ID004_0  set_state_ID004_0_ERR )
            (and is_state_ID004_0_ACTION  act_to_err_state_ID004_0   set_state_ID004_0_ERR )
            ) :trans true))
    ; sup  [ And(Not(x0004 ) , Not(intermediate1 )), And(Not(x0004 ) , Not(intermediate1 )), And(Not(x0004 ) , Not(intermediate1 )), 0, 0, 0, 0, True, True, Not(intermediate1 ), 1, -1 ]
    (define-fun tse_ID004_1 () Bool (and (not x0004 )  (not intermediate1 )))
    (define-fun tc_ID004_1 () Bool (and (not x0004 )  (not intermediate1 )))
    (define-fun tee_ID004_1 () Bool (and (not x0004 )  (not intermediate1 )))
    (define-fun ase_ID004_1 () Bool true)
    (define-fun ac_ID004_1 () Bool true)
    (define-fun aee_ID004_1 () Bool (not intermediate1_n ))
    ; guards of the SUP transitions + clock reset/not changed
    (define-fun stay_idle_state_ID004_1 () Bool ( and (not tse_ID004_1) c_ID004_1_unchanged ))
    (define-fun idle_to_trig_state_ID004_1 () Bool ( and tse_ID004_1 c_ID004_1_reset  ))
    (define-fun trig_to_idle_state_ID004_1 () Bool ( and ( or ( and (not tee_ID004_1) (not tc_ID004_1)) (and (not tc_ID004_1) (< c_ID004_1 0 ) ) (and (not tee_ID004_1) (>= c_ID004_1 0 )) (> c_ID004_1 0 )) c_ID004_1_reset))
    (define-fun stay_trig_state_ID004_1 () Bool  ( and (  and tc_ID004_1 (< c_ID004_1 0 ) ( or ( not tee_ID004_1)  (< c_ID004_1 0 ))) c_ID004_1_unchanged ))
    (define-fun trig_to_delay_state_ID004_1 () Bool ( and (and tee_ID004_1 (>= c_ID004_1 0 ) (<= c_ID004_1 0 )) c_ID004_1_reset ))
    (define-fun stay_delay_state_ID004_1 () Bool (  and (and (< c_ID004_1 0 ) ( or (not ase_ID004_1) (< c_ID004_1 0 ))) c_ID004_1_unchanged ))
    (define-fun delay_to_err_state_ID004_1 () Bool ( and (and (not ase_ID004_1) (>= c_ID004_1 0 )) c_ID004_1_unchanged ))
    (define-fun stay_act_state_ID004_1 () Bool ( and (and ac_ID004_1   true  (or (not aee_ID004_1) (<= c_ID004_1 0 ))) c_ID004_1_unchanged ))
    (define-fun act_to_err_state_ID004_1 () Bool ( and (or (and (not ac_ID004_1) (not aee_ID004_1)) (and (not ac_ID004_1) (<= c_ID004_1 0 )) (and (not aee_ID004_1)  false )  false ) c_ID004_1_unchanged ))
    (define-fun act_to_idle_state_ID004_1 () Bool ( and (and aee_ID004_1 (> c_ID004_1 0 )  true )  c_ID004_1_reset ))
    (define-fun delay_to_act_state_ID004_1 () Bool ( and ( and ase_ID004_1 (>= c_ID004_1 0 ) (<= c_ID004_1 0 )) c_ID004_1_reset ))
    ; guards of the SUP transitions without clock delay
    (define-fun trig_to_delay_no_clock_state_ID004_1 () Bool tee_ID004_1 )
    (define-fun delay_to_err_no_clock_state_ID004_1 () Bool (not ase_ID004_1)  )
    (define-fun idle_to_trig_no_clock_state_ID004_1 () Bool tse_ID004_1)
    (define-fun delay_to_act_no_clock_state_ID004_1 () Bool ase_ID004_1 )
    (define-fun act_to_err_no_clock_state_ID004_1 () Bool (not ac_ID004_1) )
    (define-fun act_to_idle_no_clock_state_ID004_1 () Bool false)

    ; SUP transitions
    (define-fun .state_ID004_1_trans () Bool (!  (or

    ; Combined transitions
    ; IDLE -> TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_1_IDLE idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1 delay_to_act_no_clock_state_ID004_1 act_to_idle_no_clock_state_ID004_1 set_state_ID004_1_IDLE)
    ; IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_1_IDLE  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1  delay_to_err_state_ID004_1  set_state_ID004_1_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT -> ERR
    (and is_state_ID004_1_IDLE  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1  delay_to_act_no_clock_state_ID004_1  act_to_err_no_clock_state_ID004_1  set_state_ID004_1_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_1_IDLE  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1  delay_to_act_no_clock_state_ID004_1 ac_ID004_1 set_state_ID004_1_ACTION)
    ; IDLE -> TRIG -> DELAY
    (and is_state_ID004_1_IDLE  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1  false set_state_ID004_1_DELAY)
    ; IDLE -> TRIG
    (and is_state_ID004_1_IDLE  idle_to_trig_state_ID004_1 (and tee_ID004_1 (not trig_to_delay_no_clock_state_ID004_1)) set_state_ID004_1_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_1_TRIG  trig_to_delay_state_ID004_1  delay_to_act_no_clock_state_ID004_1   act_to_idle_no_clock_state_ID004_1  idle_to_trig_state_ID004_1  set_state_ID004_1_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_1_TRIG  trig_to_delay_state_ID004_1  delay_to_act_no_clock_state_ID004_1   act_to_idle_no_clock_state_ID004_1 (or (not tse_ID004_1) (not tee_ID004_1)) set_state_ID004_1_IDLE)
    ; TRIG -> DELAY -> ACT
    (and is_state_ID004_1_TRIG  trig_to_delay_state_ID004_1  delay_to_act_no_clock_state_ID004_1   ac_ID004_1 set_state_ID004_1_ACTION)
    ; TRIG -> DELAY -> ERR
    (and is_state_ID004_1_TRIG  trig_to_delay_state_ID004_1  delay_to_err_no_clock_state_ID004_1  set_state_ID004_1_ERR)
    ; TRIG -> DELAY
    (and is_state_ID004_1_TRIG  trig_to_delay_state_ID004_1  false  set_state_ID004_1_ERR)
    ; DELAY -> ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_1_DELAY  delay_to_act_state_ID004_1   act_to_idle_no_clock_state_ID004_1   idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1   set_state_ID004_1_DELAY )
    ; DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_1_DELAY  delay_to_act_state_ID004_1   act_to_idle_no_clock_state_ID004_1   idle_to_trig_state_ID004_1 (and tee_ID004_1 (not trig_to_delay_no_clock_state_ID004_1)) set_state_ID004_1_TRIG)
    ; DELAY -> ACT -> IDLE
    (and is_state_ID004_1_DELAY  delay_to_act_state_ID004_1   act_to_idle_no_clock_state_ID004_1   (or (not tse_ID004_1) (not tee_ID004_1)) set_state_ID004_1_IDLE)
    ; DELAY -> ACT
    (and is_state_ID004_1_DELAY  delay_to_act_state_ID004_1   ac_ID004_1 set_state_ID004_1_ACTION)
    ; DELAY -> ACT -> ERR
    (and is_state_ID004_1_DELAY  delay_to_act_state_ID004_1   act_to_err_no_clock_state_ID004_1 set_state_ID004_1_ERR)
    ; ACT -> IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_1_ACTION  act_to_idle_state_ID004_1  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1  delay_to_act_no_clock_state_ID004_1  set_state_ID004_1_ACTION)
    ; ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_1_ACTION  act_to_idle_state_ID004_1  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1  false set_state_ID004_1_DELAY)
    ; ACT -> IDLE -> TRIG
    (and is_state_ID004_1_ACTION  act_to_idle_state_ID004_1  idle_to_trig_state_ID004_1  (and tee_ID004_1 (not trig_to_delay_no_clock_state_ID004_1)) set_state_ID004_1_TRIG)
    ; ACT -> IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_1_ACTION  act_to_idle_state_ID004_1  idle_to_trig_state_ID004_1  trig_to_delay_no_clock_state_ID004_1   delay_to_err_no_clock_state_ID004_1 set_state_ID004_1_ERR)

    ; Single transitions:
            (and is_state_ID004_1_IDLE  stay_idle_state_ID004_1  set_state_ID004_1_IDLE )
            (and is_state_ID004_1_IDLE  idle_to_trig_state_ID004_1  (and tee_ID004_1 (not trig_to_delay_no_clock_state_ID004_1)) set_state_ID004_1_TRIG )
            (and is_state_ID004_1_TRIG  trig_to_idle_state_ID004_1 (not idle_to_trig_no_clock_state_ID004_1) set_state_ID004_1_IDLE )
            (and is_state_ID004_1_TRIG  stay_trig_state_ID004_1  set_state_ID004_1_TRIG )
            (and is_state_ID004_1_TRIG  trig_to_delay_state_ID004_1  false set_state_ID004_1_DELAY )
            (and is_state_ID004_1_DELAY  stay_delay_state_ID004_1  set_state_ID004_1_DELAY )
            (and is_state_ID004_1_DELAY  delay_to_act_state_ID004_1  ac_ID004_1 set_state_ID004_1_ACTION )
            (and is_state_ID004_1_ACTION  stay_act_state_ID004_1  set_state_ID004_1_ACTION )
            (and is_state_ID004_1_ACTION  act_to_idle_state_ID004_1  set_state_ID004_1_IDLE)
            (and is_state_ID004_1_DELAY  delay_to_err_state_ID004_1  set_state_ID004_1_ERR )
            (and is_state_ID004_1_ACTION  act_to_err_state_ID004_1   set_state_ID004_1_ERR )
            ) :trans true))
    ; sup  [ x0006 , x0006 , x0006 , 0, 0, 0, 0, True, intermediate0 , True, -1, -1 ]
    (define-fun tse_ID004_2 () Bool x0006 )
    (define-fun tc_ID004_2 () Bool x0006 )
    (define-fun tee_ID004_2 () Bool x0006 )
    (define-fun ase_ID004_2 () Bool true)
    (define-fun ac_ID004_2 () Bool intermediate0_n )
    (define-fun aee_ID004_2 () Bool true)
    ; guards of the SUP transitions + clock reset/not changed
    (define-fun stay_idle_state_ID004_2 () Bool ( and (not tse_ID004_2) c_ID004_2_unchanged ))
    (define-fun idle_to_trig_state_ID004_2 () Bool ( and tse_ID004_2 c_ID004_2_reset  ))
    (define-fun trig_to_idle_state_ID004_2 () Bool ( and ( or ( and (not tee_ID004_2) (not tc_ID004_2)) (and (not tc_ID004_2) (< c_ID004_2 0 ) ) (and (not tee_ID004_2) (>= c_ID004_2 0 )) (> c_ID004_2 0 )) c_ID004_2_reset))
    (define-fun stay_trig_state_ID004_2 () Bool  ( and (  and tc_ID004_2 (< c_ID004_2 0 ) ( or ( not tee_ID004_2)  (< c_ID004_2 0 ))) c_ID004_2_unchanged ))
    (define-fun trig_to_delay_state_ID004_2 () Bool ( and (and tee_ID004_2 (>= c_ID004_2 0 ) (<= c_ID004_2 0 )) c_ID004_2_reset ))
    (define-fun stay_delay_state_ID004_2 () Bool (  and (and (< c_ID004_2 0 ) ( or (not ase_ID004_2) (< c_ID004_2 0 ))) c_ID004_2_unchanged ))
    (define-fun delay_to_err_state_ID004_2 () Bool ( and (and (not ase_ID004_2) (>= c_ID004_2 0 )) c_ID004_2_unchanged ))
    (define-fun stay_act_state_ID004_2 () Bool ( and (and ac_ID004_2   true  (or (not aee_ID004_2)  true )) c_ID004_2_unchanged ))
    (define-fun act_to_err_state_ID004_2 () Bool ( and (or (and (not ac_ID004_2) (not aee_ID004_2)) (and (not ac_ID004_2)  true ) (and (not aee_ID004_2)  false )  false ) c_ID004_2_unchanged ))
    (define-fun act_to_idle_state_ID004_2 () Bool ( and (and aee_ID004_2  false   true )  c_ID004_2_reset ))
    (define-fun delay_to_act_state_ID004_2 () Bool ( and ( and ase_ID004_2 (>= c_ID004_2 0 ) (<= c_ID004_2 0 )) c_ID004_2_reset ))
    ; guards of the SUP transitions without clock delay
    (define-fun trig_to_delay_no_clock_state_ID004_2 () Bool tee_ID004_2 )
    (define-fun delay_to_err_no_clock_state_ID004_2 () Bool (not ase_ID004_2)  )
    (define-fun idle_to_trig_no_clock_state_ID004_2 () Bool tse_ID004_2)
    (define-fun delay_to_act_no_clock_state_ID004_2 () Bool ase_ID004_2 )
    (define-fun act_to_err_no_clock_state_ID004_2 () Bool (not ac_ID004_2) )
    (define-fun act_to_idle_no_clock_state_ID004_2 () Bool false)

    ; SUP transitions
    (define-fun .state_ID004_2_trans () Bool (!  (or

    ; Combined transitions
    ; IDLE -> TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_2_IDLE idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2 delay_to_act_no_clock_state_ID004_2 act_to_idle_no_clock_state_ID004_2 set_state_ID004_2_IDLE)
    ; IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_2_IDLE  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2  delay_to_err_state_ID004_2  set_state_ID004_2_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT -> ERR
    (and is_state_ID004_2_IDLE  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2  delay_to_act_no_clock_state_ID004_2  act_to_err_no_clock_state_ID004_2  set_state_ID004_2_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_2_IDLE  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2  delay_to_act_no_clock_state_ID004_2 ac_ID004_2 set_state_ID004_2_ACTION)
    ; IDLE -> TRIG -> DELAY
    (and is_state_ID004_2_IDLE  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2  false set_state_ID004_2_DELAY)
    ; IDLE -> TRIG
    (and is_state_ID004_2_IDLE  idle_to_trig_state_ID004_2 (and tee_ID004_2 (not trig_to_delay_no_clock_state_ID004_2)) set_state_ID004_2_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_2_TRIG  trig_to_delay_state_ID004_2  delay_to_act_no_clock_state_ID004_2   act_to_idle_no_clock_state_ID004_2  idle_to_trig_state_ID004_2  set_state_ID004_2_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_2_TRIG  trig_to_delay_state_ID004_2  delay_to_act_no_clock_state_ID004_2   act_to_idle_no_clock_state_ID004_2 (or (not tse_ID004_2) (not tee_ID004_2)) set_state_ID004_2_IDLE)
    ; TRIG -> DELAY -> ACT
    (and is_state_ID004_2_TRIG  trig_to_delay_state_ID004_2  delay_to_act_no_clock_state_ID004_2   ac_ID004_2 set_state_ID004_2_ACTION)
    ; TRIG -> DELAY -> ERR
    (and is_state_ID004_2_TRIG  trig_to_delay_state_ID004_2  delay_to_err_no_clock_state_ID004_2  set_state_ID004_2_ERR)
    ; TRIG -> DELAY
    (and is_state_ID004_2_TRIG  trig_to_delay_state_ID004_2  false  set_state_ID004_2_ERR)
    ; DELAY -> ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_2_DELAY  delay_to_act_state_ID004_2   act_to_idle_no_clock_state_ID004_2   idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2   set_state_ID004_2_DELAY )
    ; DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_2_DELAY  delay_to_act_state_ID004_2   act_to_idle_no_clock_state_ID004_2   idle_to_trig_state_ID004_2 (and tee_ID004_2 (not trig_to_delay_no_clock_state_ID004_2)) set_state_ID004_2_TRIG)
    ; DELAY -> ACT -> IDLE
    (and is_state_ID004_2_DELAY  delay_to_act_state_ID004_2   act_to_idle_no_clock_state_ID004_2   (or (not tse_ID004_2) (not tee_ID004_2)) set_state_ID004_2_IDLE)
    ; DELAY -> ACT
    (and is_state_ID004_2_DELAY  delay_to_act_state_ID004_2   ac_ID004_2 set_state_ID004_2_ACTION)
    ; DELAY -> ACT -> ERR
    (and is_state_ID004_2_DELAY  delay_to_act_state_ID004_2   act_to_err_no_clock_state_ID004_2 set_state_ID004_2_ERR)
    ; ACT -> IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_2_ACTION  act_to_idle_state_ID004_2  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2  delay_to_act_no_clock_state_ID004_2  set_state_ID004_2_ACTION)
    ; ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_2_ACTION  act_to_idle_state_ID004_2  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2  false set_state_ID004_2_DELAY)
    ; ACT -> IDLE -> TRIG
    (and is_state_ID004_2_ACTION  act_to_idle_state_ID004_2  idle_to_trig_state_ID004_2  (and tee_ID004_2 (not trig_to_delay_no_clock_state_ID004_2)) set_state_ID004_2_TRIG)
    ; ACT -> IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_2_ACTION  act_to_idle_state_ID004_2  idle_to_trig_state_ID004_2  trig_to_delay_no_clock_state_ID004_2   delay_to_err_no_clock_state_ID004_2 set_state_ID004_2_ERR)

    ; Single transitions:
            (and is_state_ID004_2_IDLE  stay_idle_state_ID004_2  set_state_ID004_2_IDLE )
            (and is_state_ID004_2_IDLE  idle_to_trig_state_ID004_2  (and tee_ID004_2 (not trig_to_delay_no_clock_state_ID004_2)) set_state_ID004_2_TRIG )
            (and is_state_ID004_2_TRIG  trig_to_idle_state_ID004_2 (not idle_to_trig_no_clock_state_ID004_2) set_state_ID004_2_IDLE )
            (and is_state_ID004_2_TRIG  stay_trig_state_ID004_2  set_state_ID004_2_TRIG )
            (and is_state_ID004_2_TRIG  trig_to_delay_state_ID004_2  false set_state_ID004_2_DELAY )
            (and is_state_ID004_2_DELAY  stay_delay_state_ID004_2  set_state_ID004_2_DELAY )
            (and is_state_ID004_2_DELAY  delay_to_act_state_ID004_2  ac_ID004_2 set_state_ID004_2_ACTION )
            (and is_state_ID004_2_ACTION  stay_act_state_ID004_2  set_state_ID004_2_ACTION )
            (and is_state_ID004_2_ACTION  act_to_idle_state_ID004_2  set_state_ID004_2_IDLE)
            (and is_state_ID004_2_DELAY  delay_to_err_state_ID004_2  set_state_ID004_2_ERR )
            (and is_state_ID004_2_ACTION  act_to_err_state_ID004_2   set_state_ID004_2_ERR )
            ) :trans true))
    ; sup  [ And(Not(x0006 ) , Not(intermediate0 )), And(Not(x0006 ) , Not(intermediate0 )), And(Not(x0006 ) , Not(intermediate0 )), 0, 0, 0, 0, True, True, Not(intermediate0 ), 1, -1 ]
    (define-fun tse_ID004_3 () Bool (and (not x0006 )  (not intermediate0 )))
    (define-fun tc_ID004_3 () Bool (and (not x0006 )  (not intermediate0 )))
    (define-fun tee_ID004_3 () Bool (and (not x0006 )  (not intermediate0 )))
    (define-fun ase_ID004_3 () Bool true)
    (define-fun ac_ID004_3 () Bool true)
    (define-fun aee_ID004_3 () Bool (not intermediate0_n ))
    ; guards of the SUP transitions + clock reset/not changed
    (define-fun stay_idle_state_ID004_3 () Bool ( and (not tse_ID004_3) c_ID004_3_unchanged ))
    (define-fun idle_to_trig_state_ID004_3 () Bool ( and tse_ID004_3 c_ID004_3_reset  ))
    (define-fun trig_to_idle_state_ID004_3 () Bool ( and ( or ( and (not tee_ID004_3) (not tc_ID004_3)) (and (not tc_ID004_3) (< c_ID004_3 0 ) ) (and (not tee_ID004_3) (>= c_ID004_3 0 )) (> c_ID004_3 0 )) c_ID004_3_reset))
    (define-fun stay_trig_state_ID004_3 () Bool  ( and (  and tc_ID004_3 (< c_ID004_3 0 ) ( or ( not tee_ID004_3)  (< c_ID004_3 0 ))) c_ID004_3_unchanged ))
    (define-fun trig_to_delay_state_ID004_3 () Bool ( and (and tee_ID004_3 (>= c_ID004_3 0 ) (<= c_ID004_3 0 )) c_ID004_3_reset ))
    (define-fun stay_delay_state_ID004_3 () Bool (  and (and (< c_ID004_3 0 ) ( or (not ase_ID004_3) (< c_ID004_3 0 ))) c_ID004_3_unchanged ))
    (define-fun delay_to_err_state_ID004_3 () Bool ( and (and (not ase_ID004_3) (>= c_ID004_3 0 )) c_ID004_3_unchanged ))
    (define-fun stay_act_state_ID004_3 () Bool ( and (and ac_ID004_3   true  (or (not aee_ID004_3) (<= c_ID004_3 0 ))) c_ID004_3_unchanged ))
    (define-fun act_to_err_state_ID004_3 () Bool ( and (or (and (not ac_ID004_3) (not aee_ID004_3)) (and (not ac_ID004_3) (<= c_ID004_3 0 )) (and (not aee_ID004_3)  false )  false ) c_ID004_3_unchanged ))
    (define-fun act_to_idle_state_ID004_3 () Bool ( and (and aee_ID004_3 (> c_ID004_3 0 )  true )  c_ID004_3_reset ))
    (define-fun delay_to_act_state_ID004_3 () Bool ( and ( and ase_ID004_3 (>= c_ID004_3 0 ) (<= c_ID004_3 0 )) c_ID004_3_reset ))
    ; guards of the SUP transitions without clock delay
    (define-fun trig_to_delay_no_clock_state_ID004_3 () Bool tee_ID004_3 )
    (define-fun delay_to_err_no_clock_state_ID004_3 () Bool (not ase_ID004_3)  )
    (define-fun idle_to_trig_no_clock_state_ID004_3 () Bool tse_ID004_3)
    (define-fun delay_to_act_no_clock_state_ID004_3 () Bool ase_ID004_3 )
    (define-fun act_to_err_no_clock_state_ID004_3 () Bool (not ac_ID004_3) )
    (define-fun act_to_idle_no_clock_state_ID004_3 () Bool false)

    ; SUP transitions
    (define-fun .state_ID004_3_trans () Bool (!  (or

    ; Combined transitions
    ; IDLE -> TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_3_IDLE idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3 delay_to_act_no_clock_state_ID004_3 act_to_idle_no_clock_state_ID004_3 set_state_ID004_3_IDLE)
    ; IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_3_IDLE  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3  delay_to_err_state_ID004_3  set_state_ID004_3_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT -> ERR
    (and is_state_ID004_3_IDLE  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3  delay_to_act_no_clock_state_ID004_3  act_to_err_no_clock_state_ID004_3  set_state_ID004_3_ERR)
    ; IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_3_IDLE  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3  delay_to_act_no_clock_state_ID004_3 ac_ID004_3 set_state_ID004_3_ACTION)
    ; IDLE -> TRIG -> DELAY
    (and is_state_ID004_3_IDLE  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3  false set_state_ID004_3_DELAY)
    ; IDLE -> TRIG
    (and is_state_ID004_3_IDLE  idle_to_trig_state_ID004_3 (and tee_ID004_3 (not trig_to_delay_no_clock_state_ID004_3)) set_state_ID004_3_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_3_TRIG  trig_to_delay_state_ID004_3  delay_to_act_no_clock_state_ID004_3   act_to_idle_no_clock_state_ID004_3  idle_to_trig_state_ID004_3  set_state_ID004_3_TRIG)
    ; TRIG -> DELAY -> ACT -> IDLE
    (and is_state_ID004_3_TRIG  trig_to_delay_state_ID004_3  delay_to_act_no_clock_state_ID004_3   act_to_idle_no_clock_state_ID004_3 (or (not tse_ID004_3) (not tee_ID004_3)) set_state_ID004_3_IDLE)
    ; TRIG -> DELAY -> ACT
    (and is_state_ID004_3_TRIG  trig_to_delay_state_ID004_3  delay_to_act_no_clock_state_ID004_3   ac_ID004_3 set_state_ID004_3_ACTION)
    ; TRIG -> DELAY -> ERR
    (and is_state_ID004_3_TRIG  trig_to_delay_state_ID004_3  delay_to_err_no_clock_state_ID004_3  set_state_ID004_3_ERR)
    ; TRIG -> DELAY
    (and is_state_ID004_3_TRIG  trig_to_delay_state_ID004_3  false  set_state_ID004_3_ERR)
    ; DELAY -> ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_3_DELAY  delay_to_act_state_ID004_3   act_to_idle_no_clock_state_ID004_3   idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3   set_state_ID004_3_DELAY )
    ; DELAY -> ACT -> IDLE -> TRIG
    (and is_state_ID004_3_DELAY  delay_to_act_state_ID004_3   act_to_idle_no_clock_state_ID004_3   idle_to_trig_state_ID004_3 (and tee_ID004_3 (not trig_to_delay_no_clock_state_ID004_3)) set_state_ID004_3_TRIG)
    ; DELAY -> ACT -> IDLE
    (and is_state_ID004_3_DELAY  delay_to_act_state_ID004_3   act_to_idle_no_clock_state_ID004_3   (or (not tse_ID004_3) (not tee_ID004_3)) set_state_ID004_3_IDLE)
    ; DELAY -> ACT
    (and is_state_ID004_3_DELAY  delay_to_act_state_ID004_3   ac_ID004_3 set_state_ID004_3_ACTION)
    ; DELAY -> ACT -> ERR
    (and is_state_ID004_3_DELAY  delay_to_act_state_ID004_3   act_to_err_no_clock_state_ID004_3 set_state_ID004_3_ERR)
    ; ACT -> IDLE -> TRIG -> DELAY -> ACT
    (and is_state_ID004_3_ACTION  act_to_idle_state_ID004_3  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3  delay_to_act_no_clock_state_ID004_3  set_state_ID004_3_ACTION)
    ; ACT -> IDLE -> TRIG -> DELAY
    (and is_state_ID004_3_ACTION  act_to_idle_state_ID004_3  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3  false set_state_ID004_3_DELAY)
    ; ACT -> IDLE -> TRIG
    (and is_state_ID004_3_ACTION  act_to_idle_state_ID004_3  idle_to_trig_state_ID004_3  (and tee_ID004_3 (not trig_to_delay_no_clock_state_ID004_3)) set_state_ID004_3_TRIG)
    ; ACT -> IDLE -> TRIG -> DELAY -> ERR
    (and is_state_ID004_3_ACTION  act_to_idle_state_ID004_3  idle_to_trig_state_ID004_3  trig_to_delay_no_clock_state_ID004_3   delay_to_err_no_clock_state_ID004_3 set_state_ID004_3_ERR)

    ; Single transitions:
            (and is_state_ID004_3_IDLE  stay_idle_state_ID004_3  set_state_ID004_3_IDLE )
            (and is_state_ID004_3_IDLE  idle_to_trig_state_ID004_3  (and tee_ID004_3 (not trig_to_delay_no_clock_state_ID004_3)) set_state_ID004_3_TRIG )
            (and is_state_ID004_3_TRIG  trig_to_idle_state_ID004_3 (not idle_to_trig_no_clock_state_ID004_3) set_state_ID004_3_IDLE )
            (and is_state_ID004_3_TRIG  stay_trig_state_ID004_3  set_state_ID004_3_TRIG )
            (and is_state_ID004_3_TRIG  trig_to_delay_state_ID004_3  false set_state_ID004_3_DELAY )
            (and is_state_ID004_3_DELAY  stay_delay_state_ID004_3  set_state_ID004_3_DELAY )
            (and is_state_ID004_3_DELAY  delay_to_act_state_ID004_3  ac_ID004_3 set_state_ID004_3_ACTION )
            (and is_state_ID004_3_ACTION  stay_act_state_ID004_3  set_state_ID004_3_ACTION )
            (and is_state_ID004_3_ACTION  act_to_idle_state_ID004_3  set_state_ID004_3_IDLE)
            (and is_state_ID004_3_DELAY  delay_to_err_state_ID004_3  set_state_ID004_3_ERR )
            (and is_state_ID004_3_ACTION  act_to_err_state_ID004_3   set_state_ID004_3_ERR )
            ) :trans true))
    ; sup  [ And(x0005  , intermediate1 ), And(x0005  , intermediate1 ), And(x0005  , intermediate1 ), 0, 0, 0, 0, True, True, intermediate0 , 0, 0 ]
    (define-fun tse_ID004_4 () Bool (and x0005   intermediate1 ))
    (define-fun tc_ID004_4 () Bool (and x0005   intermediate1 ))
    (define-fun tee_ID004_4 () Bool (and x0005   intermediate1 ))
    (define-fun ase_ID004_4 () Bool true)
    (define-fun ac_ID004_4 () Bool true)
    (define-fun aee_ID004_4 () Bool intermediate0_n )
    ; guards of the SUP transitions + clock reset/not changed
    (define-fun stay_idle_state_ID004_4 () Bool ( and (not tse_ID004_4) c_ID004_4_unchanged ))
    (define-fun idle_to_trig_state_ID004_4 () Bool ( and tse_ID004_4 c_ID004_4_reset  ))
    (define-fun trig_to_idle_state_ID004_4 () Bool ( and ( or ( and (not tee_ID004_4) (not tc_ID004_4)) (and (not tc_ID004_4) (< c_ID004_4 0 ) ) (and (not tee_ID004_4) (>= c_ID004_4 0 )) (> c_ID004_4 0 )) c_ID004_4_reset))
    (define-fun stay_trig_state_ID004_4 () Bool  ( and (  and tc_ID004_4 (< c_ID004_4 0 ) ( or ( not tee_ID004_4)  (< c_ID004_4 0 ))) c_ID004_4_unchanged ))
    (define-fun trig_to_delay_state_ID004_4 () Bool ( and (and tee_ID004_4 (>= c_ID004_4 0 ) (<= c_ID004_4 0 )) c_ID004_4_reset ))
    (define-fun stay_delay_state_ID004_4 () Bool (  and (and (< c_ID004_4 0 ) ( or (not ase_ID004_4) (< c_ID004_4 0 ))) c_ID004_4_unchanged ))
    (define-fun delay_to_err_state_ID004_4 () Bool ( and (and (not ase_ID004_4) (>= c_ID004_4 0 )) c_ID004_4_unchanged ))
    (define-fun stay_act_state_ID004_4 () Bool ( and (and ac_ID004_4  (< c_ID004_4 0 ) (or (not aee_ID004_4) (< c_ID004_4 0 ))) c_ID004_4_unchanged ))
    (define-fun act_to_err_state_ID004_4 () Bool ( and (or (and (not ac_ID004_4) (not aee_ID004_4)) (and (not ac_ID004_4) (< c_ID004_4 0 )) (and (not aee_ID004_4) (>= c_ID004_4 0 )) (> c_ID004_4 0 )) c_ID004_4_unchanged ))
    (define-fun act_to_idle_state_ID004_4 () Bool ( and (and aee_ID004_4 (>= c_ID004_4 0 ) (<= c_ID004_4 0 ))  c_ID004_4_reset ))
    (define-fun delay_to_act_state_ID004_4 () Bool ( and ( and ase_ID004_4 (>= c_ID004_4 0 ) (<= c_ID004_4 0 )) c_ID004_4_reset ))
    ; guards of the SUP transitions without clock delay
    (define-fun trig_to_delay_no_clock_state_ID004_4 () Bool tee_ID004_4 )
    (define-fun delay_to_err_no_clock_state_ID004_4 () Bool (not ase_ID004_4)  )
    (define-fun idle_to_trig_no_clock_state_ID004_4 () Bool tse_ID004_4)
    (define-fun delay_to_act_no_clock_state_ID004_4 () Bool ase_ID004_4 )
    (define-fun act_to_err_no_clock_state_ID004_4 () Bool  (not aee_ID004_4)  )
    (define-fun act_to_idle_no_clock_state_ID004_4 () Bool ( and true aee_ID004_4  ))

    ; SUP transitions
    (define-fun .state_ID004_4_trans () Bool (!  ( and
    ; all constants are 0 so the state machine is simpler
    ; TODO in this case, all clock and state variables can be removed altogether, except for the variable err
      (=> is_state_ID004_4_ERR set_state_ID004_4_ERR) ; make the err state absorbing
      (or
        (and tse_ID004_4 tee_ID004_4 (not (and ase_ID004_4 aee_ID004_4)) set_state_ID004_4_ERR )
        (and (not is_state_ID004_4_ERR) (not (and tse_ID004_4 tee_ID004_4)) set_state_ID004_4_IDLE )
        (and (not is_state_ID004_4_ERR)tse_ID004_4 tee_ID004_4 ase_ID004_4 aee_ID004_4 set_state_ID004_4_IDLE )
      )
    ) :trans true))

    (assert true)
    |}]