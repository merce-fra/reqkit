


(** [generate_vmt_file fmt t] generates a file in the vmt-lib format containing the parsed requirements [t] in the formatter [fmt] *)
val generate_vmt_file : Format.formatter -> Parse.t -> Input_args.t -> unit