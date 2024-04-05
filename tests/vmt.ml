open Src.Input_args

let get_args file =
  { output_fmt = VMT;
  state_enc = BooleanEncoding;
  clock_t = IntegerClock;
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

    ;these are generated variables to model Before, After ... requirements
    (declare-fun intermediate0 () Bool)
    (declare-fun intermediate0_n () Bool)
    (define-fun .intermediate0_sv0 () Bool (!  intermediate0 :next intermediate0_n))
    (define-fun .intermediate0_init () Bool (! (= intermediate0 false) :init true))
    (declare-fun intermediate1 () Bool)
    (declare-fun intermediate1_n () Bool)
    (define-fun .intermediate1_sv0 () Bool (!  intermediate1 :next intermediate1_n))
    (define-fun .intermediate1_init () Bool (! (= intermediate1 false) :init true))

    ;these are generated SUPs



    ;generation of the SUP state machine for requirement ID004
    ; sup [ x0004 , x0004 , x0004 , 0, 0, 1, 1, intermediate1 , intermediate1 , intermediate1 , 0, 0 ]
    (define-fun tse_ID004_0 () Bool x0004 )
    (define-fun tc_ID004_0 () Bool x0004 )
    (define-fun tee_ID004_0 () Bool x0004 )
    (define-fun ase_ID004_0 () Bool intermediate1_n )
    (define-fun ac_ID004_0 () Bool intermediate1_n )
    (define-fun aee_ID004_0 () Bool intermediate1_n )
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

    ;these are the functions that explicit the guards of the SUP and  the counter reset/not changed
    (define-fun stay_idle_state_ID004_0 () Bool ( and (not tse_ID004_0) c_ID004_0_unchanged ))
    (define-fun idle_to_trig_state_ID004_0 () Bool ( and (= tse_ID004_0 true)  c_ID004_0_reset  ))
    (define-fun trig_to_idle_state_ID004_0 () Bool ( and ( or ( and (not tee_ID004_0) (not tc_ID004_0)) (and (not tc_ID004_0) (< c_ID004_0 0 ) ) (and (not tee_ID004_0) (>= c_ID004_0 0 )) (> c_ID004_0 0 )) c_ID004_0_reset))
    (define-fun stay_trig_state_ID004_0 () Bool  ( and (  and tc_ID004_0 (not (>= c_ID004_0 0 )) ( or ( not tee_ID004_0)  (< c_ID004_0 0 ))) c_ID004_0_unchanged ))
    (define-fun trig_to_delay_state_ID004_0 () Bool ( and (and tee_ID004_0 (not (< c_ID004_0 0 )) (not (> c_ID004_0 0 ))) c_ID004_0_reset ))
    (define-fun stay_delay_state_ID004_0 () Bool (  and ( and (not (>= c_ID004_0 1 )) ( or (not ase_ID004_0) (< c_ID004_0 1 )) ) c_ID004_0_unchanged ))
    (define-fun delay_to_err_state_ID004_0 () Bool ( and ( and (= ase_ID004_0 false) (>= c_ID004_0 1 ) ) c_ID004_0_unchanged ))
    (define-fun delay_to_act_state_ID004_0 () Bool ( and ( and ase_ID004_0 (not (< c_ID004_0 1 )) (not (> c_ID004_0 1 ))) c_ID004_0_reset ))
    (define-fun stay_act_state_ID004_0 () Bool ( and ( and ac_ID004_0  (not (>= c_ID004_0 0 )) (or (not aee_ID004_0) (< c_ID004_0 0 )) ) c_ID004_0_unchanged ))
    (define-fun act_to_err_state_ID004_0 () Bool ( and ( or (and (not ac_ID004_0) (not aee_ID004_0)) (and (not ac_ID004_0) (< c_ID004_0 0 )) (and (not aee_ID004_0) (>= c_ID004_0 0 )) (> c_ID004_0 0 ) ) c_ID004_0_unchanged ))
    (define-fun act_to_idle_state_ID004_0 () Bool ( and (and aee_ID004_0 (not (< c_ID004_0 0 )) (not (> c_ID004_0 0 )))  c_ID004_0_reset ))

    ;this is the function to explicit SUP transition
    (define-fun .state_ID004_0_trans () Bool (!  ( or
    ;additional transitions because tmin and amin are equals to 0
    ;this is the encoding of a ACTION to DELAY state in one tick
          (and (=  is_state_ID004_0_ACTION true) (= act_to_idle_state_ID004_0 true) (= idle_to_trig_state_ID004_0 true)  (= trig_to_delay_state_ID004_0 true)  (= set_state_ID004_0_DELAY true)  )
    ;this is the encoding of a ACTION to TRIG state in one tick
            (and (=  is_state_ID004_0_ACTION true) (= act_to_idle_state_ID004_0 true) (= idle_to_trig_state_ID004_0 true)  (= trig_to_delay_state_ID004_0 false)  (= set_state_ID004_0_TRIG true)  )
    ;this is the encoding of a IDLE to DELAY state in one tick
          (and (=  is_state_ID004_0_IDLE true) (= idle_to_trig_state_ID004_0 true)  (= trig_to_delay_state_ID004_0 false)  (= trig_to_delay_state_ID004_0 true)   (= set_state_ID004_0_DELAY true) )
    ;this is the encoding of single state change.
          (and (=  is_state_ID004_0_IDLE true) (= stay_idle_state_ID004_0 true) (= set_state_ID004_0_IDLE true) )
          (and (=  is_state_ID004_0_IDLE true) (= idle_to_trig_state_ID004_0 true) (= trig_to_delay_state_ID004_0 false) (= set_state_ID004_0_TRIG true)  )
          (and (=  is_state_ID004_0_TRIG true) (= trig_to_idle_state_ID004_0 true) (= set_state_ID004_0_IDLE true) )
          (and (=  is_state_ID004_0_TRIG true) (= stay_trig_state_ID004_0 true) (= set_state_ID004_0_TRIG true))
          (and (=  is_state_ID004_0_TRIG true) (= trig_to_delay_state_ID004_0 true) (= set_state_ID004_0_DELAY true)  )
          (and (=  is_state_ID004_0_DELAY true) (= stay_delay_state_ID004_0 true) (= set_state_ID004_0_DELAY true) )
          (and (=  is_state_ID004_0_DELAY true) (= delay_to_act_state_ID004_0 true) (= act_to_idle_state_ID004_0 false) (= set_state_ID004_0_ACTION true)     )
          (and (=  is_state_ID004_0_ACTION true) (= stay_act_state_ID004_0 true) (= set_state_ID004_0_ACTION true) )
          (and (=  is_state_ID004_0_ACTION true) (= act_to_idle_state_ID004_0 true) (= set_state_ID004_0_IDLE true) )
          (and (=  is_state_ID004_0_DELAY true) (= delay_to_err_state_ID004_0 true) (= set_state_ID004_0_ERR true) )
          (and (=  is_state_ID004_0_ACTION true) (= act_to_err_state_ID004_0 true)  (= set_state_ID004_0_ERR true) )
          ) :trans true))
    ; sup [ And(Not(x0004 ) , Not(intermediate1 )), And(Not(x0004 ) , Not(intermediate1 )), And(Not(x0004 ) , Not(intermediate1 )), 0, 0, 1, 1, True, True, Not(intermediate1 ), 0, 0 ]
    (define-fun tse_ID004_1 () Bool (and (not x0004 )  (not intermediate1 )))
    (define-fun tc_ID004_1 () Bool (and (not x0004 )  (not intermediate1 )))
    (define-fun tee_ID004_1 () Bool (and (not x0004 )  (not intermediate1 )))
    (define-fun ase_ID004_1 () Bool true)
    (define-fun ac_ID004_1 () Bool true)
    (define-fun aee_ID004_1 () Bool (not intermediate1_n ))
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

    ;these are the functions that explicit the guards of the SUP and  the counter reset/not changed
    (define-fun stay_idle_state_ID004_1 () Bool ( and (not tse_ID004_1) c_ID004_1_unchanged ))
    (define-fun idle_to_trig_state_ID004_1 () Bool ( and (= tse_ID004_1 true)  c_ID004_1_reset  ))
    (define-fun trig_to_idle_state_ID004_1 () Bool ( and ( or ( and (not tee_ID004_1) (not tc_ID004_1)) (and (not tc_ID004_1) (< c_ID004_1 0 ) ) (and (not tee_ID004_1) (>= c_ID004_1 0 )) (> c_ID004_1 0 )) c_ID004_1_reset))
    (define-fun stay_trig_state_ID004_1 () Bool  ( and (  and tc_ID004_1 (not (>= c_ID004_1 0 )) ( or ( not tee_ID004_1)  (< c_ID004_1 0 ))) c_ID004_1_unchanged ))
    (define-fun trig_to_delay_state_ID004_1 () Bool ( and (and tee_ID004_1 (not (< c_ID004_1 0 )) (not (> c_ID004_1 0 ))) c_ID004_1_reset ))
    (define-fun stay_delay_state_ID004_1 () Bool (  and ( and (not (>= c_ID004_1 1 )) ( or (not ase_ID004_1) (< c_ID004_1 1 )) ) c_ID004_1_unchanged ))
    (define-fun delay_to_err_state_ID004_1 () Bool ( and ( and (= ase_ID004_1 false) (>= c_ID004_1 1 ) ) c_ID004_1_unchanged ))
    (define-fun delay_to_act_state_ID004_1 () Bool ( and ( and ase_ID004_1 (not (< c_ID004_1 1 )) (not (> c_ID004_1 1 ))) c_ID004_1_reset ))
    (define-fun stay_act_state_ID004_1 () Bool ( and ( and ac_ID004_1  (not (>= c_ID004_1 0 )) (or (not aee_ID004_1) (< c_ID004_1 0 )) ) c_ID004_1_unchanged ))
    (define-fun act_to_err_state_ID004_1 () Bool ( and ( or (and (not ac_ID004_1) (not aee_ID004_1)) (and (not ac_ID004_1) (< c_ID004_1 0 )) (and (not aee_ID004_1) (>= c_ID004_1 0 )) (> c_ID004_1 0 ) ) c_ID004_1_unchanged ))
    (define-fun act_to_idle_state_ID004_1 () Bool ( and (and aee_ID004_1 (not (< c_ID004_1 0 )) (not (> c_ID004_1 0 )))  c_ID004_1_reset ))

    ;this is the function to explicit SUP transition
    (define-fun .state_ID004_1_trans () Bool (!  ( or
    ;additional transitions because tmin and amin are equals to 0
    ;this is the encoding of a ACTION to DELAY state in one tick
          (and (=  is_state_ID004_1_ACTION true) (= act_to_idle_state_ID004_1 true) (= idle_to_trig_state_ID004_1 true)  (= trig_to_delay_state_ID004_1 true)  (= set_state_ID004_1_DELAY true)  )
    ;this is the encoding of a ACTION to TRIG state in one tick
            (and (=  is_state_ID004_1_ACTION true) (= act_to_idle_state_ID004_1 true) (= idle_to_trig_state_ID004_1 true)  (= trig_to_delay_state_ID004_1 false)  (= set_state_ID004_1_TRIG true)  )
    ;this is the encoding of a IDLE to DELAY state in one tick
          (and (=  is_state_ID004_1_IDLE true) (= idle_to_trig_state_ID004_1 true)  (= trig_to_delay_state_ID004_1 false)  (= trig_to_delay_state_ID004_1 true)   (= set_state_ID004_1_DELAY true) )
    ;this is the encoding of single state change.
          (and (=  is_state_ID004_1_IDLE true) (= stay_idle_state_ID004_1 true) (= set_state_ID004_1_IDLE true) )
          (and (=  is_state_ID004_1_IDLE true) (= idle_to_trig_state_ID004_1 true) (= trig_to_delay_state_ID004_1 false) (= set_state_ID004_1_TRIG true)  )
          (and (=  is_state_ID004_1_TRIG true) (= trig_to_idle_state_ID004_1 true) (= set_state_ID004_1_IDLE true) )
          (and (=  is_state_ID004_1_TRIG true) (= stay_trig_state_ID004_1 true) (= set_state_ID004_1_TRIG true))
          (and (=  is_state_ID004_1_TRIG true) (= trig_to_delay_state_ID004_1 true) (= set_state_ID004_1_DELAY true)  )
          (and (=  is_state_ID004_1_DELAY true) (= stay_delay_state_ID004_1 true) (= set_state_ID004_1_DELAY true) )
          (and (=  is_state_ID004_1_DELAY true) (= delay_to_act_state_ID004_1 true) (= act_to_idle_state_ID004_1 false) (= set_state_ID004_1_ACTION true)     )
          (and (=  is_state_ID004_1_ACTION true) (= stay_act_state_ID004_1 true) (= set_state_ID004_1_ACTION true) )
          (and (=  is_state_ID004_1_ACTION true) (= act_to_idle_state_ID004_1 true) (= set_state_ID004_1_IDLE true) )
          (and (=  is_state_ID004_1_DELAY true) (= delay_to_err_state_ID004_1 true) (= set_state_ID004_1_ERR true) )
          (and (=  is_state_ID004_1_ACTION true) (= act_to_err_state_ID004_1 true)  (= set_state_ID004_1_ERR true) )
          ) :trans true))
    ; sup [ x0006 , x0006 , x0006 , 0, 0, 1, 1, intermediate0 , intermediate0 , intermediate0 , 0, 0 ]
    (define-fun tse_ID004_2 () Bool x0006 )
    (define-fun tc_ID004_2 () Bool x0006 )
    (define-fun tee_ID004_2 () Bool x0006 )
    (define-fun ase_ID004_2 () Bool intermediate0_n )
    (define-fun ac_ID004_2 () Bool intermediate0_n )
    (define-fun aee_ID004_2 () Bool intermediate0_n )
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

    ;these are the functions that explicit the guards of the SUP and  the counter reset/not changed
    (define-fun stay_idle_state_ID004_2 () Bool ( and (not tse_ID004_2) c_ID004_2_unchanged ))
    (define-fun idle_to_trig_state_ID004_2 () Bool ( and (= tse_ID004_2 true)  c_ID004_2_reset  ))
    (define-fun trig_to_idle_state_ID004_2 () Bool ( and ( or ( and (not tee_ID004_2) (not tc_ID004_2)) (and (not tc_ID004_2) (< c_ID004_2 0 ) ) (and (not tee_ID004_2) (>= c_ID004_2 0 )) (> c_ID004_2 0 )) c_ID004_2_reset))
    (define-fun stay_trig_state_ID004_2 () Bool  ( and (  and tc_ID004_2 (not (>= c_ID004_2 0 )) ( or ( not tee_ID004_2)  (< c_ID004_2 0 ))) c_ID004_2_unchanged ))
    (define-fun trig_to_delay_state_ID004_2 () Bool ( and (and tee_ID004_2 (not (< c_ID004_2 0 )) (not (> c_ID004_2 0 ))) c_ID004_2_reset ))
    (define-fun stay_delay_state_ID004_2 () Bool (  and ( and (not (>= c_ID004_2 1 )) ( or (not ase_ID004_2) (< c_ID004_2 1 )) ) c_ID004_2_unchanged ))
    (define-fun delay_to_err_state_ID004_2 () Bool ( and ( and (= ase_ID004_2 false) (>= c_ID004_2 1 ) ) c_ID004_2_unchanged ))
    (define-fun delay_to_act_state_ID004_2 () Bool ( and ( and ase_ID004_2 (not (< c_ID004_2 1 )) (not (> c_ID004_2 1 ))) c_ID004_2_reset ))
    (define-fun stay_act_state_ID004_2 () Bool ( and ( and ac_ID004_2  (not (>= c_ID004_2 0 )) (or (not aee_ID004_2) (< c_ID004_2 0 )) ) c_ID004_2_unchanged ))
    (define-fun act_to_err_state_ID004_2 () Bool ( and ( or (and (not ac_ID004_2) (not aee_ID004_2)) (and (not ac_ID004_2) (< c_ID004_2 0 )) (and (not aee_ID004_2) (>= c_ID004_2 0 )) (> c_ID004_2 0 ) ) c_ID004_2_unchanged ))
    (define-fun act_to_idle_state_ID004_2 () Bool ( and (and aee_ID004_2 (not (< c_ID004_2 0 )) (not (> c_ID004_2 0 )))  c_ID004_2_reset ))

    ;this is the function to explicit SUP transition
    (define-fun .state_ID004_2_trans () Bool (!  ( or
    ;additional transitions because tmin and amin are equals to 0
    ;this is the encoding of a ACTION to DELAY state in one tick
          (and (=  is_state_ID004_2_ACTION true) (= act_to_idle_state_ID004_2 true) (= idle_to_trig_state_ID004_2 true)  (= trig_to_delay_state_ID004_2 true)  (= set_state_ID004_2_DELAY true)  )
    ;this is the encoding of a ACTION to TRIG state in one tick
            (and (=  is_state_ID004_2_ACTION true) (= act_to_idle_state_ID004_2 true) (= idle_to_trig_state_ID004_2 true)  (= trig_to_delay_state_ID004_2 false)  (= set_state_ID004_2_TRIG true)  )
    ;this is the encoding of a IDLE to DELAY state in one tick
          (and (=  is_state_ID004_2_IDLE true) (= idle_to_trig_state_ID004_2 true)  (= trig_to_delay_state_ID004_2 false)  (= trig_to_delay_state_ID004_2 true)   (= set_state_ID004_2_DELAY true) )
    ;this is the encoding of single state change.
          (and (=  is_state_ID004_2_IDLE true) (= stay_idle_state_ID004_2 true) (= set_state_ID004_2_IDLE true) )
          (and (=  is_state_ID004_2_IDLE true) (= idle_to_trig_state_ID004_2 true) (= trig_to_delay_state_ID004_2 false) (= set_state_ID004_2_TRIG true)  )
          (and (=  is_state_ID004_2_TRIG true) (= trig_to_idle_state_ID004_2 true) (= set_state_ID004_2_IDLE true) )
          (and (=  is_state_ID004_2_TRIG true) (= stay_trig_state_ID004_2 true) (= set_state_ID004_2_TRIG true))
          (and (=  is_state_ID004_2_TRIG true) (= trig_to_delay_state_ID004_2 true) (= set_state_ID004_2_DELAY true)  )
          (and (=  is_state_ID004_2_DELAY true) (= stay_delay_state_ID004_2 true) (= set_state_ID004_2_DELAY true) )
          (and (=  is_state_ID004_2_DELAY true) (= delay_to_act_state_ID004_2 true) (= act_to_idle_state_ID004_2 false) (= set_state_ID004_2_ACTION true)     )
          (and (=  is_state_ID004_2_ACTION true) (= stay_act_state_ID004_2 true) (= set_state_ID004_2_ACTION true) )
          (and (=  is_state_ID004_2_ACTION true) (= act_to_idle_state_ID004_2 true) (= set_state_ID004_2_IDLE true) )
          (and (=  is_state_ID004_2_DELAY true) (= delay_to_err_state_ID004_2 true) (= set_state_ID004_2_ERR true) )
          (and (=  is_state_ID004_2_ACTION true) (= act_to_err_state_ID004_2 true)  (= set_state_ID004_2_ERR true) )
          ) :trans true))
    ; sup [ And(Not(x0006 ) , Not(intermediate0 )), And(Not(x0006 ) , Not(intermediate0 )), And(Not(x0006 ) , Not(intermediate0 )), 0, 0, 1, 1, True, True, Not(intermediate0 ), 0, 0 ]
    (define-fun tse_ID004_3 () Bool (and (not x0006 )  (not intermediate0 )))
    (define-fun tc_ID004_3 () Bool (and (not x0006 )  (not intermediate0 )))
    (define-fun tee_ID004_3 () Bool (and (not x0006 )  (not intermediate0 )))
    (define-fun ase_ID004_3 () Bool true)
    (define-fun ac_ID004_3 () Bool true)
    (define-fun aee_ID004_3 () Bool (not intermediate0_n ))
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

    ;these are the functions that explicit the guards of the SUP and  the counter reset/not changed
    (define-fun stay_idle_state_ID004_3 () Bool ( and (not tse_ID004_3) c_ID004_3_unchanged ))
    (define-fun idle_to_trig_state_ID004_3 () Bool ( and (= tse_ID004_3 true)  c_ID004_3_reset  ))
    (define-fun trig_to_idle_state_ID004_3 () Bool ( and ( or ( and (not tee_ID004_3) (not tc_ID004_3)) (and (not tc_ID004_3) (< c_ID004_3 0 ) ) (and (not tee_ID004_3) (>= c_ID004_3 0 )) (> c_ID004_3 0 )) c_ID004_3_reset))
    (define-fun stay_trig_state_ID004_3 () Bool  ( and (  and tc_ID004_3 (not (>= c_ID004_3 0 )) ( or ( not tee_ID004_3)  (< c_ID004_3 0 ))) c_ID004_3_unchanged ))
    (define-fun trig_to_delay_state_ID004_3 () Bool ( and (and tee_ID004_3 (not (< c_ID004_3 0 )) (not (> c_ID004_3 0 ))) c_ID004_3_reset ))
    (define-fun stay_delay_state_ID004_3 () Bool (  and ( and (not (>= c_ID004_3 1 )) ( or (not ase_ID004_3) (< c_ID004_3 1 )) ) c_ID004_3_unchanged ))
    (define-fun delay_to_err_state_ID004_3 () Bool ( and ( and (= ase_ID004_3 false) (>= c_ID004_3 1 ) ) c_ID004_3_unchanged ))
    (define-fun delay_to_act_state_ID004_3 () Bool ( and ( and ase_ID004_3 (not (< c_ID004_3 1 )) (not (> c_ID004_3 1 ))) c_ID004_3_reset ))
    (define-fun stay_act_state_ID004_3 () Bool ( and ( and ac_ID004_3  (not (>= c_ID004_3 0 )) (or (not aee_ID004_3) (< c_ID004_3 0 )) ) c_ID004_3_unchanged ))
    (define-fun act_to_err_state_ID004_3 () Bool ( and ( or (and (not ac_ID004_3) (not aee_ID004_3)) (and (not ac_ID004_3) (< c_ID004_3 0 )) (and (not aee_ID004_3) (>= c_ID004_3 0 )) (> c_ID004_3 0 ) ) c_ID004_3_unchanged ))
    (define-fun act_to_idle_state_ID004_3 () Bool ( and (and aee_ID004_3 (not (< c_ID004_3 0 )) (not (> c_ID004_3 0 )))  c_ID004_3_reset ))

    ;this is the function to explicit SUP transition
    (define-fun .state_ID004_3_trans () Bool (!  ( or
    ;additional transitions because tmin and amin are equals to 0
    ;this is the encoding of a ACTION to DELAY state in one tick
          (and (=  is_state_ID004_3_ACTION true) (= act_to_idle_state_ID004_3 true) (= idle_to_trig_state_ID004_3 true)  (= trig_to_delay_state_ID004_3 true)  (= set_state_ID004_3_DELAY true)  )
    ;this is the encoding of a ACTION to TRIG state in one tick
            (and (=  is_state_ID004_3_ACTION true) (= act_to_idle_state_ID004_3 true) (= idle_to_trig_state_ID004_3 true)  (= trig_to_delay_state_ID004_3 false)  (= set_state_ID004_3_TRIG true)  )
    ;this is the encoding of a IDLE to DELAY state in one tick
          (and (=  is_state_ID004_3_IDLE true) (= idle_to_trig_state_ID004_3 true)  (= trig_to_delay_state_ID004_3 false)  (= trig_to_delay_state_ID004_3 true)   (= set_state_ID004_3_DELAY true) )
    ;this is the encoding of single state change.
          (and (=  is_state_ID004_3_IDLE true) (= stay_idle_state_ID004_3 true) (= set_state_ID004_3_IDLE true) )
          (and (=  is_state_ID004_3_IDLE true) (= idle_to_trig_state_ID004_3 true) (= trig_to_delay_state_ID004_3 false) (= set_state_ID004_3_TRIG true)  )
          (and (=  is_state_ID004_3_TRIG true) (= trig_to_idle_state_ID004_3 true) (= set_state_ID004_3_IDLE true) )
          (and (=  is_state_ID004_3_TRIG true) (= stay_trig_state_ID004_3 true) (= set_state_ID004_3_TRIG true))
          (and (=  is_state_ID004_3_TRIG true) (= trig_to_delay_state_ID004_3 true) (= set_state_ID004_3_DELAY true)  )
          (and (=  is_state_ID004_3_DELAY true) (= stay_delay_state_ID004_3 true) (= set_state_ID004_3_DELAY true) )
          (and (=  is_state_ID004_3_DELAY true) (= delay_to_act_state_ID004_3 true) (= act_to_idle_state_ID004_3 false) (= set_state_ID004_3_ACTION true)     )
          (and (=  is_state_ID004_3_ACTION true) (= stay_act_state_ID004_3 true) (= set_state_ID004_3_ACTION true) )
          (and (=  is_state_ID004_3_ACTION true) (= act_to_idle_state_ID004_3 true) (= set_state_ID004_3_IDLE true) )
          (and (=  is_state_ID004_3_DELAY true) (= delay_to_err_state_ID004_3 true) (= set_state_ID004_3_ERR true) )
          (and (=  is_state_ID004_3_ACTION true) (= act_to_err_state_ID004_3 true)  (= set_state_ID004_3_ERR true) )
          ) :trans true))
    ; sup [ And(x0005  , intermediate1 ), And(x0005  , intermediate1 ), And(x0005  , intermediate1 ), 0, 0, 0, 0, intermediate0 , intermediate0 , intermediate0 , 0, 0 ]
    (define-fun tse_ID004_4 () Bool (and x0005   intermediate1 ))
    (define-fun tc_ID004_4 () Bool (and x0005   intermediate1 ))
    (define-fun tee_ID004_4 () Bool (and x0005   intermediate1 ))
    (define-fun ase_ID004_4 () Bool intermediate0_n )
    (define-fun ac_ID004_4 () Bool intermediate0_n )
    (define-fun aee_ID004_4 () Bool intermediate0_n )
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

    ;these are the functions that explicit the guards of the SUP and  the counter reset/not changed
    (define-fun stay_idle_state_ID004_4 () Bool ( and (not tse_ID004_4) c_ID004_4_unchanged ))
    (define-fun idle_to_trig_state_ID004_4 () Bool ( and (= tse_ID004_4 true)  c_ID004_4_reset  ))
    (define-fun trig_to_idle_state_ID004_4 () Bool ( and ( or ( and (not tee_ID004_4) (not tc_ID004_4)) (and (not tc_ID004_4) (< c_ID004_4 0 ) ) (and (not tee_ID004_4) (>= c_ID004_4 0 )) (> c_ID004_4 0 )) c_ID004_4_reset))
    (define-fun stay_trig_state_ID004_4 () Bool  ( and (  and tc_ID004_4 (not (>= c_ID004_4 0 )) ( or ( not tee_ID004_4)  (< c_ID004_4 0 ))) c_ID004_4_unchanged ))
    (define-fun trig_to_delay_state_ID004_4 () Bool ( and (and tee_ID004_4 (not (< c_ID004_4 0 )) (not (> c_ID004_4 0 ))) c_ID004_4_reset ))
    (define-fun stay_delay_state_ID004_4 () Bool (  and ( and (not (>= c_ID004_4 0 )) ( or (not ase_ID004_4) (< c_ID004_4 0 )) ) c_ID004_4_unchanged ))
    (define-fun delay_to_err_state_ID004_4 () Bool ( and ( and (= ase_ID004_4 false) (>= c_ID004_4 0 ) ) c_ID004_4_unchanged ))
    (define-fun delay_to_act_state_ID004_4 () Bool ( and ( and ase_ID004_4 (not (< c_ID004_4 0 )) (not (> c_ID004_4 0 ))) c_ID004_4_reset ))
    (define-fun stay_act_state_ID004_4 () Bool ( and ( and ac_ID004_4  (not (>= c_ID004_4 0 )) (or (not aee_ID004_4) (< c_ID004_4 0 )) ) c_ID004_4_unchanged ))
    (define-fun act_to_err_state_ID004_4 () Bool ( and ( or (and (not ac_ID004_4) (not aee_ID004_4)) (and (not ac_ID004_4) (< c_ID004_4 0 )) (and (not aee_ID004_4) (>= c_ID004_4 0 )) (> c_ID004_4 0 ) ) c_ID004_4_unchanged ))
    (define-fun act_to_idle_state_ID004_4 () Bool ( and (and aee_ID004_4 (not (< c_ID004_4 0 )) (not (> c_ID004_4 0 )))  c_ID004_4_reset ))

    ;this is the function to explicit SUP transition
    (define-fun .state_ID004_4_trans () Bool (!  ( or
    ;additional transitions because tmin and lmin and amin are equals to 0
    ;this is the encoding of a IDLE to IDLE state in one tick
           (and (= is_state_ID004_4_IDLE true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true) (= act_to_idle_state_ID004_4 true)  (= set_state_ID004_4_IDLE true)  )
    (and (= is_state_ID004_4_IDLE true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 false) (= set_state_ID004_4_ACTION true) )
    (and (= is_state_ID004_4_IDLE true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 false) (= set_state_ID004_4_DELAY true) )
    (and (= is_state_ID004_4_IDLE true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_err_state_ID004_4 true) (= set_state_ID004_4_ERR true)  )
    (and (= is_state_ID004_4_IDLE true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true) (= act_to_err_state_ID004_4 true) (= set_state_ID004_4_ERR true)  )
    (and (= is_state_ID004_4_TRIG true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 true) (= idle_to_trig_state_ID004_4 true) (= set_state_ID004_4_TRIG true)  )
    (and (= is_state_ID004_4_TRIG true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 true)  (= idle_to_trig_state_ID004_4 false) (= set_state_ID004_4_IDLE true))
    (and (= is_state_ID004_4_TRIG true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 false) (= set_state_ID004_4_ACTION true))
    (and (= is_state_ID004_4_TRIG true) (= trig_to_delay_state_ID004_4 true) (= delay_to_err_state_ID004_4 true) (= set_state_ID004_4_ERR true)  )
    (and (= is_state_ID004_4_DELAY true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 true)  (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true)  (= set_state_ID004_4_DELAY true) )
    (and (= is_state_ID004_4_DELAY true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 true)  (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 false) (= set_state_ID004_4_TRIG true) )
    (and (= is_state_ID004_4_DELAY true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 true)  (= idle_to_trig_state_ID004_4 false) (= set_state_ID004_4_IDLE true) )
    (and (= is_state_ID004_4_DELAY true) (= delay_to_act_state_ID004_4 true)  (= act_to_idle_state_ID004_4 false) (= set_state_ID004_4_ACTION true) )
    (and (= is_state_ID004_4_ACTION true) (= act_to_idle_state_ID004_4 true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 true) (= set_state_ID004_4_ACTION true) )
    (and (= is_state_ID004_4_ACTION true) (= act_to_idle_state_ID004_4 true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 false) (= set_state_ID004_4_DELAY true) )
    (and (= is_state_ID004_4_ACTION true) (= act_to_idle_state_ID004_4 true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 false) (= set_state_ID004_4_TRIG true) )
    (and (= is_state_ID004_4_ACTION true) (= act_to_idle_state_ID004_4 true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 true) (= delay_to_err_state_ID004_4 false) (= set_state_ID004_4_ERR true) )
    ;this is the encoding of single state change.
          (and (=  is_state_ID004_4_IDLE true) (= stay_idle_state_ID004_4 true) (= set_state_ID004_4_IDLE true) )
          (and (=  is_state_ID004_4_IDLE true) (= idle_to_trig_state_ID004_4 true) (= trig_to_delay_state_ID004_4 false) (= set_state_ID004_4_TRIG true)  )
          (and (=  is_state_ID004_4_TRIG true) (= trig_to_idle_state_ID004_4 true) (= set_state_ID004_4_IDLE true) )
          (and (=  is_state_ID004_4_TRIG true) (= stay_trig_state_ID004_4 true) (= set_state_ID004_4_TRIG true))
          (and (=  is_state_ID004_4_TRIG true) (= trig_to_delay_state_ID004_4 true) (= delay_to_act_state_ID004_4 false)(= set_state_ID004_4_DELAY true)  )
          (and (=  is_state_ID004_4_DELAY true) (= stay_delay_state_ID004_4 true) (= set_state_ID004_4_DELAY true) )
          (and (=  is_state_ID004_4_DELAY true) (= delay_to_act_state_ID004_4 true) (= act_to_idle_state_ID004_4 false) (= set_state_ID004_4_ACTION true)     )
          (and (=  is_state_ID004_4_ACTION true) (= stay_act_state_ID004_4 true) (= set_state_ID004_4_ACTION true) )
          (and (=  is_state_ID004_4_ACTION true) (= act_to_idle_state_ID004_4 true) (= set_state_ID004_4_IDLE true) )
          (and (=  is_state_ID004_4_DELAY true) (= delay_to_err_state_ID004_4 true) (= set_state_ID004_4_ERR true) )
          (and (=  is_state_ID004_4_ACTION true) (= act_to_err_state_ID004_4 true)  (= set_state_ID004_4_ERR true) )
          ) :trans true))
    (define-fun .all_sup_status () Bool (! (and true true  (not is_state_ID004_0_ERR)  (not is_state_ID004_1_ERR)  (not is_state_ID004_2_ERR)  (not is_state_ID004_3_ERR)  (not is_state_ID004_4_ERR) ) :invar-property 1))


    (assert true)|}]