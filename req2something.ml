
let () = 
  
  let file = ref None in
  let dir = ref None in 
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
      (fill "input")^"Gives the requirement file to process");  
    ("--input-dir",
      Arg.String (fun s -> dir := Some s),
      (fill "input_valuenput-dir")^"Gives a directory where all requirements file are processed");  
    ("--simple-exp",
      Arg.Bool (fun b -> simple_exp := b),
      (fill "simple_exp")^"When used with --input-dir keep the most simple or complex expressions for the requirements");  
  ] in 
  Arg.parse speclist print_endline usage;

  match (!file, !dir) with 
  |None, Some d ->    
    begin
        try
          let list_req_files =  Array.to_list (Sys.readdir d) in          
          let input_parse_list = List.fold_left (fun acc f ->  (Src.Parse.of_file (d^"/"^f))::acc) [] list_req_files in
          let typical_reqs = Src.Util.typical_reqs input_parse_list !simple_exp in
          let fmt = Format.get_std_formatter () in 
          List.iteri (fun i p ->  ( Format.printf "##################################################################@.Construction #%d@." (i+1); 
                                    Src.Parse.pretty_print fmt p;
                                      let (variables,m) =  Src.Sup.of_req p  in
                                      List.iter (fun v -> Format.fprintf fmt "%s = Bool('%s')@\n" v v) variables;
                                      let l = (Src.Sup.SMap.to_list m) in 
                                      if (List.length l) > 0 then(
                                        let (_,(_,sup_list)) = List.hd l in
                                        Src.Sup.print fmt sup_list true)
                                  )) typical_reqs;

          Format.printf "Success@."
        with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
    end
  |Some f, None -> 
    begin
    try 
      let t =  (Src.Parse.of_file f) in 
      let fmt = Format.get_std_formatter() in
      (* gather initial bool variables*)
      let list_initial_bool_variables = Src.Parse.extract_bool_variables t.vars in
      (* and generateed ones + SUP requirements*)
      let (generated_variables, sup_reqs) = Src.Sup.of_req t in
      (* print variables*)
      let all_variables = generated_variables@list_initial_bool_variables in
      Format.fprintf fmt "MAX_PTRACE=20@\n";
      List.iter (fun v -> Format.fprintf fmt "%s = Bool('%s')@\n" v v) all_variables;
      (* print requirements *)
      Format.fprintf fmt "REQ_SET=[";
      List.iteri (fun i  (_ ,(_,sup_list ) ) -> Src.Sup.print fmt sup_list (i=0)) ( Src.Sup.SMap.to_list sup_reqs);
      Format.fprintf fmt "]@\n"
    with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
  end
  |_,_ -> Format.printf "%s@." usage