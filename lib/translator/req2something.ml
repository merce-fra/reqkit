
open Src.Input_args

let () = 
  let exit_value = ref 0 in
  let (args, usage) = Src.Input_args.get () in   
  (match (args.input_file, args.input_dir) with 
  (* case the tool is apply on a whole directory in order to extract constructions *)
    |None, Some d ->    
      begin
          try
            let list_req_files =  Array.to_list (Sys.readdir d) in          
            let input_parse_list = List.fold_left (fun acc f ->  (Reqs.Parse.of_file (d^"/"^f))::acc) [] list_req_files in
            let typical_reqs = Src.Util.typical_reqs input_parse_list args.keep_simple in
            let fmt = Format.get_std_formatter () in 
            List.iteri (fun i p ->  ( Format.printf "##################################################################@.Construction #%d@." (i+1); 
                                      Reqs.Parse.pretty_print fmt p;
                                      (match args.output_fmt with
                                      | NuSMV   -> Src.Sup.generate_sup_file fmt p args
                                      | VMT  -> Src.Vmt.generate_vmt_file fmt p args);
                                    ) ) typical_reqs;

            Format.printf "Success@."
          with Reqs.Parse.ParseException msg -> 
            Format.printf "%s@." msg;
            exit_value := -1
      end
    (* case the tool is apply on a single file *)
    |Some f, None when String.ends_with ~suffix:".req" f -> 
      begin
      (* case of .req file *)  
      try 
        let t =  (Reqs.Parse.of_file f) in 
        let fmt = Format.get_std_formatter() in
        (match args.output_fmt with
        | NuSMV  -> Src.Sup.generate_sup_file fmt t args
        | VMT  -> Src.Vmt.generate_vmt_file fmt t args);
      with Reqs.Parse.ParseException msg -> 
        Printf.fprintf stderr "%s\n" msg;
        exit_value := -1
      end
    |Some f, None when (String.ends_with ~suffix:".py" f) || (String.ends_with ~suffix:".sup" f) -> 
      begin
        try
          let t = (Sups.Parse.of_file f) in
          let fmt = Format.get_std_formatter() in
          (match args.output_fmt with
          | VMT -> Src.Vmt.generate_vmt_file_from_sup fmt t args
          | NuSMV -> Printf.fprintf stderr "Error. Only VMT output format supported for SUP input files.\n");
        with Sups.Parse.ParseException msg ->
          Printf.fprintf stderr "%s\n" msg;
          exit_value := -1
      end

    | _,_ -> 
      Printf.fprintf stderr "%s\n" usage;
      exit_value := -1
  );
  exit !exit_value