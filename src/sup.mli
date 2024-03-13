module SMap : Map.S with type key = string 

(* Takes a parsed requirement [Parse.t] and convert it into 
   - a list of generated variables that replaces the statements myVar == cst
   - a list of intermediates variables that allows to model the After, Before etc... requirements 
   - a list of requirements in SUP format
  *)
val of_req :  Parse.t ->  (string list * string list * (Ast_types.req * Sup_types.sup_req_list) SMap.t)

val of_req_with_non_bool : Parse.t -> (string list * (Ast_types.req * Sup_types.sup_req_list) SMap.t)

(* Takes a list of requirements and convert it into the SUP format in [fmt] *)
val generate_sup_file : Format.formatter -> Parse.t -> unit