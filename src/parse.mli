
type var_or_const_type =
  | Bool (* "Input" ... "IS bool" *)
  | Int (* "Input" ... "IS int" *)
  | Real (* "Input" ... "IS real" *)
  | Const_int of int (* "CONST" ... "IS" [int] *)
  | Const_real of float (* "CONST" ... "IS" [float] *)

(* Note: all [exp] are surrounded by double quotes *)
type exp = Var of string (* xNNNNN *)
         | Int_const of int (* 10 *)
         | Not of exp (* "!" exp *)
         | And of exp * exp (* exp "&&" exp *)
         | Eq of exp * exp (* exp "==" exp *)
         | Geq of exp * exp (* exp ">=" exp *)
         | Leq of exp * exp (* exp "<=" exp *)

type req =
  | Prop of exp (* [exp] "holds" or [exp] "holds as well" *)
  | Globally of req (* "Globally" "," [req] *)
  | After of req (* "After" [req] "," *)
  | After_until of req * req (* "After" [req] "until" [req] "," *)
  | Before of req (* "Before" [req] "," *)
  | Between of req * req (* "Between" [req] "and" [req] "," *)
  | Always of req (* "it is always the case that" [req] *)
  | If of req * req (* "if" [req], "then" [req] *)
  | After_at_most of req * exp (* [req] "after at most" [exp] "time units" *)

type t = {
    vars: (string, var_or_const_type) Hashtbl.t;
      (* "CONST" [string] "IS" [var_or_const_type]
         or "Input" [string] "IS" [var_or_const_type] *)
    reqs: (string, req) Hashtbl.t (* [string] ":" [req] *)
  }

val of_file: string -> t (* parse a file of name [string] and return
                            its content as [t] *)

val print: t -> unit (* print [t] to stdout *)
