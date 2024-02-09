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

val ast_to_parse_t: (Ast_types.prog, string) result -> t

val ast_from_string : string -> (Ast_types.prog, string) result

val print : Format.formatter -> t -> unit  (* print each requirements of [t] on a single line *)

val pretty_print : Format.formatter -> t -> unit  (* pretty print each requirements of [t] to returned [string] *)
