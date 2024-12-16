open Sup_types

exception ParseException of string

type t = prog

(** [of_file filename] gets the content of [filename] as two hashmaps as [t]: one for declaration, other one for requirements *)
val of_file: string -> t   
