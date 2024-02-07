(** need the --table option : usefull for parsing error messages *)
module I = Parser.MenhirInterpreter

exception ParseException of string

type t = {
    vars: (string, Ast_types.declaration) Hashtbl.t;
      (* "CONST" [string] "IS" [var_or_const_type]
         or "Input" [string] "IS" [var_or_const_type] *)
    reqs: (string, Ast_types.req) Hashtbl.t (* [string] ":" [req] *)
  }

(** syntax error description *)
exception Syntax_error of ((int * int) option * string)

(** get the position of the lexbuf *)
let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

(** get the code error related to the parser.messages file *)
let message =
  fun s ->
    match s with
    | i when i > 0 && i< 200->
        Format.sprintf "Error %d \n" i
    | _ ->
        raise Not_found

(** get the parse error code error from the state machine*)
let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (message (I.number state)) with
        | Not_found -> "invalid syntax (no specific message for this eror)"

(** recursive method that parse the lexbuf *)
let rec parse_ lexbuf (checkpoint : Ast_types.prog I.checkpoint) =
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

(** entry point method to parse a lexbuf*)
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
  | e -> Error (Printf.sprintf "Grammar error: " )

(** get the ast from a file*)
let ast_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast = parse lexbuf in
  let () = close_in ic in
  ast

(** get the ast from a string*)
let ast_from_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf



(** get the content of the file as two hashmaps : one for declaration, other one for requirements *)
let of_file filename =
  let ast = ast_from_file filename in 
  match ast with
  | Ok ast_ ->
    begin
      let vars = Hashtbl.create 20 in
      let reqs = Hashtbl.create 20 in
      let aux1 node =
        match node with 
        | Ast_types.Constant (name ,_ ) ->   Hashtbl.add vars name node
        | Ast_types.Input (name, _ ) -> Hashtbl.add vars name node
        | Ast_types.Output (name, _ ) -> Hashtbl.add vars name node
        | Ast_types.Internal (name, _ ) -> Hashtbl.add vars name node
      in
      let aux2 node =
        match node with 
        |  Ast_types.Req (name,req) ->  Hashtbl.add reqs name req
      in
      match ast_ with 
      | Ast_types.Prog (decl_list, req_with_id_list) ->
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
  

