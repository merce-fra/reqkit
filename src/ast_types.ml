
type var_type =
  | Bool (* "Input" ... "IS bool" *)
  | Int (* "Input" ... "IS int" *)
  | Real (* "Input" ... "IS real" *)

type const_value =
  | Const_bool of bool (* "CONST" ... "IS" ...*)
  | Const_int  of int (* "CONST" ... "IS" ...*)
  | Const_real of float (* "CONST" ... "IS" ...*)

type declaration =
  | Constant of string * const_value (* "CONST" ... "IS" ...*)
  | Input of string * var_type (* "Input" ... "IS" ...*)
  | Output of string * var_type (* "Output" ... "IS" ...*)
  | Internal of string * var_type (* "Internal" ... "IS" ...*)

(* Note: all [exp] are surrounded by double quotes *)
type exp = | Var of string (* xNNNNN *)
         | Bool_const of bool (* true *)
         | Int_const of int (* 10 *)
         | Real_const of float (*10.0*)
         | Not of exp (* "!" exp *)
         | And of exp * exp (* exp "&&" exp *)
         | Or of exp * exp (* exp "||" exp *)         
         | Eq of exp * exp (* exp "==" exp *)
         | NotEq of exp * exp (* exp "!=" exp *)
         | Geq of exp * exp (* exp ">=" exp *)
         | Leq of exp * exp (* exp "<=" exp *)
         | Gt of exp * exp (* exp ">" exp *)
         | Lt of exp * exp (* exp "<" exp *)
         | Implies of exp * exp (* exp ==> exp *)
         | Plus of exp * exp (* exp + exp *)
         | Minus of exp * exp (* exp - exp *)
         | Divide of exp * exp (* exp / exp *)
         | Multiply of exp * exp (* exp * exp *)

type hold=
  | Empty (* no info *)
  | Holds (* holds or holds as well *)
  | Holds_afterward (* holds afterwards *)
  | Previously_held (* previously held *)
  | Holds_for_at_least of exp   (*holds for at least [exp] time units*) 
  | Holds_after_at_most of exp   (*holds after at most [exp] time units*) 
  | Holds_afterward_for_at_least of exp (*holds afterwards for at least [exp] time units*) 
  | Holds_for_less_than of exp  (*holds for less [exp] time units*) 
  | Holds_at_list_every of exp  (*holds at least every [exp] time units*) 
  | Holds_end_succeded_by of exp  (*holds and is succeeded by [exp]*) 
  | Toggles_at_most of exp * exp (*toggles [exp] at most [exp] time units*) 
         
type req =
  | Prop of exp * hold (* [exp] "holds" or [exp] "holds as well" *)
  | Globally of req (* "Globally" "," [req] *)
  | After of exp * req (* "After" [exp] "," [req] *)
  | After_until of exp * exp * req (* "After" [exp] "until" [exp] "," [req] *)
  | Before of exp *req (* "Before" [req] "," *)
  | Always of req (* "it is always the case that" [req] *)
  | Never of req (* "it is never the case that" [req] *)
  | If of req * req (* "if" [req], "then" [req] *)
  | After_at_most of req * exp (* [req] "after at most" [exp] "time units" *)
  | Between of exp *exp*req (* Between [exp] and [exp], [req]*)

type req_with_id = | Req of string * req

type prog = | Prog of declaration list option * req_with_id list option

