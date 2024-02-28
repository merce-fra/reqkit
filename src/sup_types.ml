
type event=  | Var of string (* xNNNNN *)
                | Not of event (* "!" exp *)
                | And of event * event (* exp "&&" exp *)
                | Or of event * event (* exp "||" exp *)         
                | Constant of bool

type time = | Time of int (*used for tmin, tmax, lmin, lmax, amin and amax*)
             
type trigger = { tse: event ; tc : event ; tee : event;  tmin : time;  tmax : time } (* trigger of a SUP *)
type action = { ase: event ; ac : event ; aee : event;  amin : time;  amax : time }  (* action of a SUP *)
type delay =  { lmin : time; lmax : time }                                           (* delay between end of the trigger and beginning of the action*)
type sup_req = {t : trigger; d : delay;  a : action}                                 (* SUP requirement *)
type sup_req_list = sup_req list                                                     (* list of SUP requirements *)
