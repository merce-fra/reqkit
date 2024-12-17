


(** [generate_vmt_file fmt t] generates a file in the vmt-lib format containing the parsed requirements [t] in the formatter [fmt] *)
val generate_vmt_file : Format.formatter -> Reqs.Parse.t -> Input_args.t -> unit

(** [generate_vmt_file_from_sup fmt t] generates a file in the vmt-lib format containing the parsed SUP requirements [t] in the formatter [fmt] *)
val generate_vmt_file_from_sup : Format.formatter -> Sups.Parse.t -> Input_args.t -> unit