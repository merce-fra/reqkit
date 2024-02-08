
let () = 
  
  let file = ref None in
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
      (fill "input")^"Gives the requirement file file to process");  
  ] in 
  Arg.parse speclist print_endline usage;

  match !file with 
  |None -> Format.printf "%s@." usage
  |Some f -> 
    begin
    try 
      let t =  (Src.Parse.of_file f (*"/home/cellier/req2something/test.req"*) ) in 
      let fmt = Format.get_std_formatter () in 
      Src.Parse.pretty_print fmt t;
      Format.printf "Success@."
    with Src.Parse.ParseException msg -> Format.printf "%s@." msg 
  end