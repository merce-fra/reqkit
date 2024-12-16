module SMap : Map.S with type key = string 

(* Takes a parsed requirement [Parse.t] and convert it into 
   - a list of generated variables that replaces the non boolen expressions
   - a list of intermediates variables that allows to model the After, Before etc... requirements 
   - a list of requirements in SUP format
  *)
val of_req :  Reqs.Parse.t ->  Input_args.t -> (string list * string list * (Reqs.Ast_types.req * Sups.Sup_types.sup_req_list) SMap.t) 

(* Takes a parsed requirement [Parse.t] and convert it into 
   - a list of intermediates variables that allows to model the After, Before etc... requirements 
   - a list of requirements in SUP format
 
val of_req_with_non_bool : Parse.t -> (string list * (Ast_types.req * Sup_types.sup_req_list) SMap.t)
 *)

(* Takes a list of requirements and convert it into the SUP format in [fmt] *)
val generate_sup_file : Format.formatter -> Reqs.Parse.t -> Input_args.t -> unit


(** [print fmt sup first] prints a [sup] requirement in the formatter [fmt]*)
val print_sup : Format.formatter -> Sups.Sup_types.sup_req -> Input_args.t -> unit