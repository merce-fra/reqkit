
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
                                    try 
                                      let (_,sup_list) = List.hd( Src.Sup.of_req p )in
                                      Src.Sup.print fmt sup_list
                                    with Invalid_argument _ -> Format.printf "ERROR : Cannot convert this requirement to SUP.@\n"
                                   )) typical_reqs;

          Format.printf "Success@."
        with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
    end
  |Some f, None -> 
    begin
    try 
      let t =  (Src.Parse.of_file f) in 
      (*fot now works only for
         part1_02.new.req
         part3_07.new.req
         part3_13.new.req
         part3_14.new.req
         part3_17.new.req
         part5_dev03.req*)
      ignore(Src.Sup.of_req t)
    with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
  end
  |_,_ -> Format.printf "%s@." usage