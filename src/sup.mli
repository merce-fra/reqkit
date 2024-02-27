
(* Takes a parsed requirement [Parse.t] and convert it into SUP format
  *)
val of_req :  Parse.t ->  (Ast_types.req * Sup_types.sup_req_list) list  


(* Takes a list of SUP and convert it into the selected format [fmt] *)
val print : Format.formatter ->  Sup_types.sup_req_list   -> unit 