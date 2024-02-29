module SMap : Map.S with type key = string 

(* Takes a parsed requirement [Parse.t] and convert it into SUP format
  *)
val of_req :  Parse.t ->  (string list * (Ast_types.req * Sup_types.sup_req_list) SMap.t)


(* Takes a list of requirements and convert it into the SUP format in [fmt] *)
val generate_sup_file : Format.formatter -> Parse.t -> unit