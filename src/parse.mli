open Ast_types

type t = {
    vars: (string, declaration) Hashtbl.t;
      (* "CONST" [string] "IS" [var_or_const_type]
         or "Input" [string] "IS" [var_or_const_type] *)
    reqs: (string, req) Hashtbl.t (* [string] ":" [req] *)
  }

exception ParseException of string

val of_file: string -> t   (*parse a file of name [string] and return
                            its content as [t] *)

val extract_bool_variables : (string, declaration) Hashtbl.t -> string list 

val ast_to_parse_t: (Ast_types.prog, string) result -> t (* convert requirements ast to t*)

val ast_from_string : string -> (Ast_types.prog, string) result

val print : Format.formatter -> t -> unit  (* print each requirements of [t] on a single line *)

val pretty_print : Format.formatter -> t -> unit  (* pretty print each requirements of [t] to returned [string] *)

val print_exp_as_string : Ast_types.exp -> string (* prints an ast expression as a string *)

val print_hold_as_string : Ast_types.hold -> string (* prints an ast hold as a string *)

val print_req_as_string : Ast_types.req -> string (* prints an ast requirement as a string *)
 
val print_declaration_as_string : Ast_types.declaration -> string (* prints an ast declaration as a string *)