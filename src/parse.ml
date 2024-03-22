(** need the --table option : usefull for parsing error messages *)
module I = Parser.MenhirInterpreter
open Ast_types

exception ParseException of string

type t = {
    vars: (string, declaration) Hashtbl.t;
      (* "CONST" [string] "IS" [var_or_const_type]
         or "Input" [string] "IS" [var_or_const_type] *)
    reqs: (string, req) Hashtbl.t (* [string] ":" [req] *)
  }

(** syntax error description *)
exception Syntax_error of ((int * int) option * string)

(** [get_lexing_position lexbuf] gets the position of the [lexbuf] *)
let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

(** [message] gets the code error related to the parser.messages file *)
let message =
  fun s ->
    match s with
    | i when i > 0 && i< 200->
        Format.sprintf "Error %d \n" i
    | _ ->
        raise Not_found

(** [get_parse_error env] gets the parse error code error from the [env] state machine*)
let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (message (I.number state)) with
        | Not_found -> "invalid syntax (no specific message for this eror)"

(** [parse_ lexbuf checkpoint ] is the recursive method that parse the [lexbuf] *)
let rec parse_ lexbuf (checkpoint : prog I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse_ lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse_ lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
       raise (Syntax_error (None, "invalid syntax (parser rejected the input)"))

(** [parse lexbuf] is the entry point method to parse a [lexbuf]*)
let parse lexbuf =
  try
    let ast = parse_ lexbuf (Parser.Incremental.prog lexbuf.lex_curr_p) in
    Ok ast
  with
  | Syntax_error (pos, err) ->
    begin
      match pos with
      | Some (line, pos) ->
        Error (Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err)
      | None -> Error (Printf.sprintf "Syntax error: %s" err)
    end
    (* here not so much info but this means that there are some unknown tokens*)
  | _ -> Error (Printf.sprintf "Grammar error: " )

(** [ast_from_file filename] gets the ast from a the file [filename]*)
let ast_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast = parse lexbuf in
  let () = close_in ic in
  ast

(** [ast_from_string s] get the ast from a string [s]*)
let ast_from_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf

(** [ast_to_parse_t ast] converts an [ast] of type Ast_types.Prog to a Parse_t.t *)
let ast_to_parse_t ast =
  match ast with
  | Ok ast_ ->
    begin
      let vars = Hashtbl.create 20 in
      let reqs = Hashtbl.create 20 in
      let aux1 node =
        match node with 
        | Constant (name ,_ ) ->   Hashtbl.add vars name node
        | Input (name, _ ) -> Hashtbl.add vars name node
        | Output (name, _ ) -> Hashtbl.add vars name node
        | Internal (name, _ ) -> Hashtbl.add vars name node
      in
      let aux2 node =
        match node with 
        |  Req (name,req) ->  Hashtbl.add reqs name req
      in
      match ast_ with 
      | Prog (decl_list, req_with_id_list) ->
        begin
          match decl_list with
          | None -> ()
          | Some l ->  List.iter (fun f -> aux1 f) l;
          match req_with_id_list with 
          |None -> ()
          |Some l -> List.iter (fun f -> aux2 f) l;
        end;
      { vars = vars; reqs = reqs}
    end
  | Error msg -> raise  ( ParseException msg)
  

(** [of_file filename] gets the content of [filename] as two hashmaps : one for declaration, other one for requirements *)
let of_file filename =
  ast_to_parse_t ( ast_from_file filename )
  
(** [print_const_value fmt v] prints a constant value [v] into the formatter [fmt] *)
let print_const_value fmt v =
  match v with 
  | Const_bool b -> Format.fprintf fmt "%b " b
  | Const_int i -> Format.fprintf fmt "%i " i
  | Const_real r -> Format.fprintf fmt "%f " r
  
(** [print_type fmt t] prints a type [t] into the formatter [fmt] *)
let print_type fmt t =
  match t with 
  | Bool  -> Format.fprintf fmt "bool"
  | Int  -> Format.fprintf fmt  "int" 
  | Real  -> Format.fprintf fmt "real" 

(** [print_declaration fmt d] prints a declaration [d] into the formatter [fmt] *)
let print_declaration fmt d = 
  match d with 
  |Constant (name, t) -> Format.fprintf fmt "CONST\t%s IS " name; print_const_value fmt t;  Format.fprintf fmt "@."
  |Input (name, t) -> Format.fprintf fmt "Input\t\t%s IS " name ; print_type fmt t;  Format.fprintf fmt "@."
  |Output (name, t) -> Format.fprintf fmt "Output\t\t%s IS " name ; print_type fmt t;  Format.fprintf fmt "@."
  |Internal (name, t) -> Format.fprintf fmt "Internal\t%s IS " name ; print_type fmt t;  Format.fprintf fmt "@."

(** [print_exp fmt e] prints an expression [e] into the formatter [fmt] *)
let rec print_exp fmt e=
  match e with
  | Var (s) -> Format.fprintf fmt "%s" s
  | Bool_const (b) -> Format.fprintf fmt "%b" b
  | Int_const (i) -> Format.fprintf fmt "%d" i
  | Real_const (r) -> Format.fprintf fmt "%f" r
  | Not (e) -> Format.fprintf fmt "!"; print_exp fmt e
  | And  (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " && "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Or (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " || "; print_exp  fmt e2;  Format.fprintf fmt ")"    
  | Eq (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " == "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | NotEq (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " != "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Geq (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " >= "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Leq (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " <= "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Gt (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " > "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Lt (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " < "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Implies (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " ==> "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Plus  (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " + "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Minus (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " - "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Divide (e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " / "; print_exp  fmt e2;  Format.fprintf fmt ")"
  | Multiply(e1,e2) -> Format.fprintf fmt "(" ; print_exp  fmt e1 ; Format.fprintf fmt " * "; print_exp  fmt e2;  Format.fprintf fmt ")"


(** [pretty] if true , the requirements are pretty printed, otherwise they are one a single line *)
let pretty = ref true


(** [print_hold fmt h] prints a hold value [h] into the formatter [fmt] *)
let print_hold fmt h= 
 ( match h with
  | Empty -> ()
  | Holds -> Format.fprintf fmt " holds "
  | Holds_afterward -> Format.fprintf fmt " holds afterward "
  | Previously_held -> Format.fprintf fmt " previously held "
  | Holds_for_at_least (e) -> Format.fprintf fmt " holds for at least "; print_exp  fmt e; Format.fprintf fmt " time units"
  | Holds_after_at_most (e) -> Format.fprintf fmt " holds after at most, "; print_exp  fmt e; Format.fprintf fmt " time units"
  | Holds_afterward_for_at_least (e) -> Format.fprintf fmt " holds afterwards for at least "; print_exp  fmt e; Format.fprintf fmt " time units"
  | Holds_for_less_than (e) -> Format.fprintf fmt " holds for less than "; print_exp  fmt e; Format.fprintf fmt " time units"
  | Holds_at_list_every (e) -> Format.fprintf fmt " holds at least every "; print_exp  fmt e; Format.fprintf fmt " time units"
  | Holds_and_succeded_by (e) -> Format.fprintf fmt " holds and is succeeded by "; print_exp  fmt e
  | At_most (e) -> Format.fprintf fmt " at most "; print_exp  fmt e;  Format.fprintf fmt " time units later" )

(** [print_exp_as_string e] prints an expression [e] as a string *)
let print_exp_as_string e =
  pretty := false;
  let fmt = Format.get_str_formatter() in
  print_exp fmt e;
  Format.flush_str_formatter()

(** [print_hold_as_string h] prints a hold [h] as a string *)
let print_hold_as_string h =
  pretty := false;
  let fmt = Format.get_str_formatter() in
  print_hold fmt h;
  Format.flush_str_formatter()

(** [print_declaration_as_string d] prints a declaration [d] as a string *)
let print_declaration_as_string d =
  pretty := false;
  let fmt = Format.get_str_formatter() in
  print_declaration fmt d;
  Format.flush_str_formatter()

(** [open_box fmt] opens a box in the formatter [fmt]*)
let open_box fmt = 
  if !pretty then Format.fprintf fmt "@["

(** [close_box fmt] closes a box in the formatter [fmt]*)
let close_box fmt = 
  if !pretty then Format.fprintf fmt "@]"

(** [carriage_return fmt] returns to a new line in a box in the formatter [fmt]*)
let carriage_return fmt = 
  if !pretty then Format.fprintf fmt "@\n  " else Format.fprintf fmt " "

(** [print_req fmt r] prints a requirement [r] in the formatter [fmt]  *)
let rec print_req fmt r =
  open_box fmt;
  (match r with 
  | Prop (e, h) -> print_exp  fmt e; print_hold  fmt h;
  | Globally (r) ->  Format.fprintf fmt "Globally,"; carriage_return fmt ; print_req fmt  r
  | After (e, r) ->   Format.fprintf fmt "After "; print_exp  fmt e; Format.fprintf fmt ",";  carriage_return fmt ; print_req fmt  r;
  | After_until (e1, e2 ,r ) ->   Format.fprintf fmt "After "; print_exp  fmt e1; Format.fprintf fmt " until "; print_exp  fmt e2; Format.fprintf fmt "," ; carriage_return fmt ; print_req fmt  r;
  | Before (e,r) ->   Format.fprintf fmt "Before "; print_exp  fmt e; Format.fprintf fmt ",";  carriage_return fmt ; print_req fmt  r;
  | Always (r) ->   Format.fprintf fmt "it is always the case that";  carriage_return fmt ; print_req fmt  r;
  | Never (r) ->   Format.fprintf fmt "it is never the case that";  carriage_return fmt ; print_req fmt  r;
  | If (r1, r2) ->   Format.fprintf fmt "if "; print_req fmt  r1;  carriage_return fmt ; Format.fprintf fmt ", then "; print_req fmt  r2;
  | After_at_most (r, e) -> print_req fmt  r; Format.fprintf fmt " after at most "; print_exp  fmt e; Format.fprintf fmt " time units"
  | Between (e1, e2, r) ->   Format.fprintf fmt "Between "; print_exp  fmt e1; Format.fprintf fmt " and "; print_exp  fmt e2; Format.fprintf fmt ",";  carriage_return fmt ; print_req fmt  r
  | Toggles( e1, e2, h) ->  print_exp  fmt e1; Format.fprintf fmt " toggles "; print_exp  fmt e2; Format.fprintf fmt " "; print_hold fmt h; carriage_return fmt 
   );
  close_box fmt

(** [print_req_as_string h] prints a requirement [r] as a string *)
let print_req_as_string r =
  pretty := false;
  let fmt = Format.get_str_formatter() in
  print_req fmt r;
  Format.flush_str_formatter()

(** [print_requirements fmt r] prints several requirements [r] in the formatter [fmt] *)
let print_requirements fmt r =
  match r with 
  |(name, r_no_id) ->Format.fprintf fmt "%s : " name; print_req fmt  r_no_id; Format.fprintf fmt "@."

(** [print_ fmt r] prints the variables and the requirements in [r] in the formatter [fmt]*)  
let print_ fmt r =
  let h = r.vars in
  let l = List.of_seq (Hashtbl.to_seq_values h) in
  List.iter (print_declaration fmt) l; Format.fprintf fmt "@.";
  let lreq = List.of_seq (Hashtbl.to_seq r.reqs ) in
  List.iter (print_requirements fmt) lreq;  Format.fprintf fmt "@."

(** [print fmt r] prints the variables and the requirements in [r] in the formatter [fmt]*)  
let print fmt r = 
  pretty := false;
  print_ fmt r

(** [pretty_print fmt r] pretty prints the variables and the requirements in [r] in the formatter [fmt]*)  
let pretty_print fmt r =
  pretty := true;
  print_ fmt r

(** [extract_bool_variables vars] extracts the boolean variables of the hashtable of declared variables [vars]*)
let extract_bool_variables vars =
  Hashtbl.fold( fun  _ d acc-> 
    begin
      match d with
      | Ast_types.Constant(n,Const_bool(_)) -> n::acc
      | Ast_types.Input(n,Ast_types.Bool) -> n::acc
      | Ast_types.Output(n,Ast_types.Bool) -> n::acc
      | Ast_types.Internal(n,Ast_types.Bool) -> n::acc
      | _ -> acc
    end)  vars  []
  

(*let print_vars parse_t=
    Hashtbl.iter (fun key value -> Format.printf "key : %s => declaration : %s" key (print_declaration_as_string value)) parse_t.vars*)