(** need the --table option : usefull for parsing error messages *)
module I = Parser.MenhirInterpreter
open Sup_types

type t = prog

exception ParseException of string

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
  | I.InputNeeded _env ->(
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse_ lexbuf checkpoint)
  | I.Shifting _
  | I.AboutToReduce _ ->(
      let checkpoint = I.resume checkpoint in
      parse_ lexbuf checkpoint)
  | I.HandlingError _env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
      (
       raise (Syntax_error (None, "invalid syntax (parser rejected the input)")))

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


  (** [of_file filename] gets the content of [filename] as two hashmaps : one for declaration, other one for requirements *)
let of_file filename =
  match ast_from_file filename with 
  | Ok ast -> ast
  | Error msg -> raise  ( ParseException msg)