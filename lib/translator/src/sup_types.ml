
type event=  | Var of string (* xNNNNN *)
                | Not of event (* "!" exp *)
                | And of event * event (* exp "&&" exp *)
                | Or of event * event (* exp "||" exp *)         
                | Constant of bool
                | IntConstant of int*int
                | RealConstant of float*int
                | Plus of event * event (* exp + exp*)
                | Minus of event * event (* exp - exp*)
                | Multiply of event * event (* exp * exp*)
                | Divide of event * event (* exp / exp*)
                | Eq of event * event (* exp == exp*)
                | Geq of event * event (* exp >= exp*)
                | Leq of event * event (* exp <= exp*)
                | Gt of event * event (* exp > exp*)
                | Lt of event * event (* exp < exp*)
                | NotEq of event * event (* exp != exp*)

type time = | Time of int (*used for tmin, tmax, lmin, lmax, amin and amax*)            
            | GreaterThan of int (*used for real clock encoding*)
            | LesserThan of int (*used for real clock encoding*)

type trigger = { tse: event ; tc : event ; tee : event;  tmin : time;  tmax : time } (* trigger of a SUP *)
type action = { ase: event ; ac : event ; aee : event;  amin : time;  amax : time }  (* action of a SUP *)
type delay =  { lmin : time; lmax : time }                                           (* delay between end of the trigger and beginning of the action*)
type sup_req = {t : trigger; d : delay;  a : action;  vacuity : bool}                                 (* SUP requirement *)
type sup_req_list = sup_req list                                                     (* list of SUP requirements *)
