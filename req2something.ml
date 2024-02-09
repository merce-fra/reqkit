
let () = 
  
  let file = ref None in
  let dir = ref None in 
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
      (fill "input-dir")^"Gives a directory where all requirements file are processed");  
  ] in 
  Arg.parse speclist print_endline usage;

  match (!file, !dir) with 
  |None, Some d ->    
    begin
        try
          let list_req_files =  Array.to_list (Sys.readdir d) in          
          let input_parse_list = List.fold_left (fun acc f ->  (Src.Parse.of_file (d^"/"^f))::acc) [] list_req_files in
          let typical_reqs = Src.Util.typical_reqs input_parse_list in
          let fmt = Format.get_std_formatter () in 
          List.iteri (fun i p ->  (Format.printf "##################################################################@.Construction #%d@." (i+1); Src.Parse.pretty_print fmt p)) typical_reqs;
          Format.printf "Success@."
        with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
    end
  |Some f, None -> 
    begin
    try 
      let t =  (Src.Parse.of_file f) in 
      let fmt = Format.get_std_formatter () in 
      Src.Parse.pretty_print fmt t;
      ignore(Src.Util.typical_reqs [t]);
      Format.printf "Success@."
    with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
  end
  |_,_ -> Format.printf "%s@." usage