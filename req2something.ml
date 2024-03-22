
let () = 
  
  let file = ref None in
  let dir = ref None in 
  let output_fmt = ref "nusmv" in
  let simple_exp = ref false in
  let fill s =
    let l = (15-(String.length s)) in
    if l < 0 then ""
    else String.make l ' '
  in
  let line_length = 73 in
  let dash_line = (String.make line_length '-')^"\n" in
  let usage = "\n"^dash_line^" User options:\n"^dash_line in
  let speclist = [
    ("--input",
      Arg.String (fun s -> file := Some s),
      (fill "input")^"Gives the requirement file to process. This option cannot be used with --input-dir.");  
    ("--input-dir",
      Arg.String (fun s -> dir := Some s),
      (fill "input_valuenput-dir")^"Gives a directory where all requirements file are processed in order to extract a single requirement for each construction kind. This option cannot be used with --input.");  
    ("--simple-exp",
      Arg.Bool (fun b -> simple_exp := b),
      (fill "simple_exp")^"When used with --input-dir keep the most simple (true) or complex (false) expressions for the requirements");  
    ("--output-fmt",
      Arg.String (fun s -> output_fmt := s),
      (fill "output-fmt")^"Specify the generated file format : nusmv or vmtlib");  
  ] in 
  Arg.parse speclist print_endline usage;

  (match !output_fmt with 
  | "nusmv" 
  | "vmtlib"  -> ()
  | _         -> Format.printf "Output format not supported : %s@." !output_fmt);

  match (!file, !dir, !output_fmt) with 
  (* case the tool is apply on a whole directory in order to extract constructions *)
  |None, Some d, out_fmt ->    
    begin
        try
          let list_req_files =  Array.to_list (Sys.readdir d) in          
          let input_parse_list = List.fold_left (fun acc f ->  (Src.Parse.of_file (d^"/"^f))::acc) [] list_req_files in
          let typical_reqs = Src.Util.typical_reqs input_parse_list !simple_exp in
          let fmt = Format.get_std_formatter () in 
          List.iteri (fun i p ->  ( Format.printf "##################################################################@.Construction #%d@." (i+1); 
                                    Src.Parse.pretty_print fmt p;
                                    (match out_fmt with
                                    | "nusmv"   -> Src.Sup.generate_sup_file fmt p
                                    | "vmtlib"  -> Src.Vmt.generate_vmt_file fmt p
                                    | _         -> ());                                    
                                  ) ) typical_reqs;

          Format.printf "Success@."
        with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
    end
  (* case the tool is apply on a single file *)
  |Some f, None, out_fmt -> 
    begin
    try 
      let t =  (Src.Parse.of_file f) in 
      let fmt = Format.get_std_formatter() in
      (match out_fmt with
      | "nusmv"   -> Src.Sup.generate_sup_file fmt t
      | "vmtlib"  -> Src.Vmt.generate_vmt_file fmt t
      | _         -> ());
    with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
  end
  | _,_,_ -> Format.printf "%s@." usage