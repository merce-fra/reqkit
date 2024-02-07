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

val ast_from_string : string -> (Ast_types.prog, string) result


(*val print: t -> string  print [t] to returned [string] *)
