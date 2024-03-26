
open Src.Input_args

let () = 
  
  let (args, usage) = Src.Input_args.get () in 

  match (args.input_file, args.input_dir) with 
  (* case the tool is apply on a whole directory in order to extract constructions *)
  |None, Some d ->    
    begin
        try
          let list_req_files =  Array.to_list (Sys.readdir d) in          
          let input_parse_list = List.fold_left (fun acc f ->  (Src.Parse.of_file (d^"/"^f))::acc) [] list_req_files in
          let typical_reqs = Src.Util.typical_reqs input_parse_list args.keep_simple in
          let fmt = Format.get_std_formatter () in 
          List.iteri (fun i p ->  ( Format.printf "##################################################################@.Construction #%d@." (i+1); 
                                    Src.Parse.pretty_print fmt p;
                                    (match args.output_fmt with
                                    | NuSMV   -> Src.Sup.generate_sup_file fmt p args
                                    | VMT  -> Src.Vmt.generate_vmt_file fmt p args);
                                  ) ) typical_reqs;

          Format.printf "Success@."
        with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
    end
  (* case the tool is apply on a single file *)
  |Some f, None -> 
    begin
    try 
      let t =  (Src.Parse.of_file f) in 
      let fmt = Format.get_std_formatter() in
      (match args.output_fmt with
      | NuSMV  -> Src.Sup.generate_sup_file fmt t args
      | VMT  -> Src.Vmt.generate_vmt_file fmt t args);
    with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
  end
  | _,_ -> Format.printf "%s@." usage