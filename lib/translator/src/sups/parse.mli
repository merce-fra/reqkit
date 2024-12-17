open Sup_types

exception ParseException of string

type t = prog

(** [of_file filename] gets the content of [filename] as an ast *)
val of_file: string -> t   
