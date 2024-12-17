open Reqs.Ast_types



(** module to compare list of int*)
module Key = struct
  type t = int list
  let compare = List.compare Int.compare
end

module ILMap = Map.Make(Key)

module SMap = Map.Make(String)

exception Unknown_variable of string

(** [req_to_int r] converts a requirement [r] into an integer in order to distinguish identical requirements construction *)
let req_to_int r =
  match r with
  | Prop (_, _) -> 1
  | Globally(_) -> 2 
  | Always (_) -> 3
  | Never (_) -> 4 
  | Before (_, _) -> 5
  | After_at_most (_, _) -> 6
  | After (_, _) -> 7
  | Between (_ ,_, _)-> 8
  | After_until (_, _, _) -> 9
  | If (_, _) -> 10
  | Toggles(_,_,_) -> 11
 
(** [hold_to_int h] converts a hold [h] into an integer in order to distinguish identical requirements construction *)
let hold_to_int h =
  match h with
  | Empty -> 20
  | Holds -> 21
  | Holds_afterward -> 22
  | Previously_held -> 23
  | Holds_for_at_least (_) -> 24
  | Holds_after_at_most (_) -> 25
  | Holds_afterward_for_at_least (_) -> 26
  | Holds_for_less_than (_) -> 27
  | Holds_at_least_every (_) -> 28
  | Holds_and_succeeded_by (_) -> 29
  | At_most (_) -> 30
  | Holds_indefinitely -> 31


(** [extract_vars_from_req req list_vars] for a requirement [req] this function 
    - extracts all variable that are used in this requirement
    - construct an integer list that represents the different requirements/holds occuring in the input requirement 
    - count the number of node used (to evaluate complexity of the requirement)*)
let extract_vars_from_req req list_vars= 
  let rec aux_e (vars, nb_expr, list_op) expr =
    match expr with
    (* if a variable add it to the list of used variable*)
    | Var (s) -> (
      try
        let v = Hashtbl.find list_vars s in
        ( SMap.add s v vars, nb_expr+1, list_op)
      with Not_found -> raise (Unknown_variable s)
    )
    | Bool_const (_) 
    | Int_const (_)
    | Real_const (_) -> (vars, nb_expr+1, list_op)
    | Not (e) ->  aux_e (vars, nb_expr+1, list_op) e
    | And (e1, e2) 
    | Or (e1, e2) 
    | Eq (e1, e2) 
    | NotEq (e1, e2) 
    | Geq (e1, e2) 
    | Leq (e1, e2) 
    | Gt (e1, e2) 
    | Lt (e1, e2) 
    | Implies (e1, e2) 
    | Plus (e1, e2) 
    | Minus (e1, e2)
    | Divide (e1, e2)
    | Multiply (e1, e2) -> aux_e (aux_e (vars, nb_expr+2, list_op) e2) e1

  and aux_h (vars, nb_expr, list_op) h =
    match h with
    | Empty
    | Holds
    | Holds_afterward
    | Holds_indefinitely
    | Previously_held -> (vars, nb_expr, (hold_to_int h)::list_op)
    | Holds_for_at_least (e)   
    | Holds_after_at_most (e)
    | Holds_afterward_for_at_least (e)
    | Holds_for_less_than (e)
    | Holds_at_least_every (e)
    | Holds_and_succeeded_by (e) -> aux_e (vars, nb_expr +1, (hold_to_int h)::list_op) e
    (*| Toggles_at_most (e1, e2) -> aux_e (aux_e (vars, nb_expr + 2, (hold_to_int h)::list_op) e2 ) e1*)
    | At_most (e) -> aux_e  (vars, nb_expr +1, (hold_to_int h)::list_op) e    
    

  in
  let rec aux_r (vars, nb_expr, list_op) req = 
    match req with
    | Prop (e, h) -> aux_e (aux_h (vars, nb_expr+2, (req_to_int req)::list_op) h ) e
    | Globally( r) 
    | Always (r)
    | Never (r) -> aux_r (vars, nb_expr +1, (req_to_int req)::list_op) r
    | Before (e, r)
    | After_at_most (r, e)
    | After (e, r) -> aux_r (aux_e (vars, nb_expr + 2, (req_to_int req)::list_op) e ) r
    | Between (e1, e2, r)
    | After_until (e1, e2, r) -> aux_r (aux_e ( aux_e (vars, nb_expr + 3, (req_to_int req)::list_op) e2) e1 ) r
    | If (r1, r2) -> aux_r (aux_r (vars, nb_expr + 2, (req_to_int req)::list_op) r2 ) r1
    | Toggles(e1, e2, h) -> aux_e( aux_e (aux_h (vars, nb_expr+3, (req_to_int req)::list_op) h ) e2) e1 
  in 
  aux_r ( SMap.empty, 0, []) req 


(** [split_reqs_from_parse_t input_parse] for a Parse.t [input_parse], split all requirements in a tuple 
    (name of the requirements, requirement, variables involved in the requirement, 
    nb of nodes, list of integer representing the requirements/holds) *)
let split_reqs_from_parse_t (input_parse : Reqs.Parse.t) = 
  let req_as_list = List.of_seq (Hashtbl.to_seq (input_parse.reqs)) in
  List.fold_left ( fun acc (name,req) -> begin
                                        let (vars, nb_expr, list_op) = extract_vars_from_req req input_parse.vars in
                                        (*let vars_as_string = List.fold_left ( fun acc (k,v) -> acc ^ k ^ " " ) "" (List.of_seq (SMap.to_seq vars)) in
                                        let list_op_as_string = List.fold_left (fun acc i -> acc ^ (string_of_int  i) ^ " ") "" list_op in
                                        Format.printf "name : %s => ops :[%s] => vars :[%s] => nb_expr:%d@\n" name vars_as_string list_op_as_string nb_expr;*)
                                        (name, req, vars, nb_expr, list_op)::acc
                                        end
                 ) [] req_as_list 


(** [typical_reqs_ input_parse_list keep_simple] for a list of Parse.t [input_parse_list], it calls split_reqs_from_parse_t 
     on each and then filter them according the list of integer representing the requirements/holds 
    If two requirements have the same list, the one with the highest number of nodes is kept if [keep_simple] is false, otherwise
    the one with lowest number of nodes is kept *)
let typical_reqs_ input_parse_list keep_simple = 
  let all_reqs = List.fold_left (fun acc input_parse -> List.append (split_reqs_from_parse_t input_parse) acc) [] input_parse_list in
  let filted_reqs_ = List.fold_left (fun acc (name, req, vars, nb_expr, list_op) -> begin
                                                                  try 
                                                                    let (_,_,_,nb,_) = ILMap.find list_op acc  in
                                                                    if ( (not keep_simple && (nb_expr > nb)) || (keep_simple && (nb_expr < nb))) then (ILMap.add list_op (name, req, vars, nb_expr, list_op) acc) else acc
                                                                  with Not_found -> ILMap.add list_op (name, req, vars, nb_expr, list_op) acc
                                                                  end) ILMap.empty all_reqs in
  (*Format.printf "Typical requirements @\n";                                                                
  ILMap.iter ( fun  key (name, req, vars, nb_expr, list_op) -> begin 
                                                          let vars_as_string = List.fold_left ( fun acc (k,v) -> acc ^ k ^ " " ) "" (List.of_seq (SMap.to_seq vars)) in
                                                          let list_op_as_string = List.fold_left (fun acc i -> acc ^ (string_of_int  i) ^ " ") "" list_op in
                                                          Format.printf "name : %s => ops :[%s] => vars :[%s] => nb_expr:%d@\n" name vars_as_string list_op_as_string nb_expr;
                                                          end) filted_reqs_;*)
                                                          filted_reqs_

(** [convert_to_parse_t name vars req] convert a requirement [name], a map of used variables [vars] and a requirement [req] to a Parse.t*)                                                                   
let convert_to_parse_t name vars req  =
  let open Reqs.Parse in                                                          
  let h1 = Hashtbl.create 1 in 
  SMap.iter ( fun k v -> Hashtbl.add h1 k v ) vars;
  let h2 = Hashtbl.create 1 in 
  Hashtbl.add h2 name req;
  {vars=h1; reqs=h2}

(** [typical_reqs input_parse_list simple_exp] extract all unique requirements constructions. 
    The requirements are distinguished using their ast considering only req/hold nodes (not the expressions) *)
let typical_reqs (input_parse_list: Reqs.Parse.t list) simple_exp : Reqs.Parse.t list =
  let reqs_ = typical_reqs_ input_parse_list simple_exp in 
  (* convert the map into a list of Parse.t *)
  let filted_reqs_list_ = List.of_seq (ILMap.to_seq reqs_) in
  List.fold_left (fun acc (_ ,(name, req, vars, _, _)) -> (convert_to_parse_t name vars req) :: acc ) [] filted_reqs_list_
