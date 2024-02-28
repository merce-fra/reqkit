module SMap : Map.S with type key = string 

(* Takes a parsed requirement [Parse.t] and convert it into SUP format
  *)
val of_req :  Parse.t ->  (string list * (Ast_types.req * Sup_types.sup_req_list) SMap.t)


(* Takes a list of SUP and convert it into the selected format [fmt] *)
val print : Format.formatter ->  Sup_types.sup_req_list -> bool  -> unit