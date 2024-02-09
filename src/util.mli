

exception Unknown_variable of string

(* From a list of [Parse.t] requirement sets, return a list of minimal
   [Parse.t] requirements. Each element of the list contains only 1
   requirement (in Parse.req sense) such that it has a different
   structure from all the other requirements of the returned list. All
   structures of requirement in the input list have at least one
   representative of the same structure on the returned
   list. Comparison between requirements of the list does not consider
   expressions or constants. Each returned requirement in the list
   have only needed variables or constants used in the requirement *)
val typical_reqs: Parse.t list -> Parse.t list
