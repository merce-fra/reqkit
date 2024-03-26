
type output_format = NuSMV | VMT 

type state_encoding = IntegerEncoding | BooleanEncoding

type clock_type = IntegerClock | RealClock

type t = { 
  output_fmt : output_format;
  state_enc: state_encoding;
  clock_t : clock_type;
  only_bool_predicates: bool;
  input_file : string option;
  input_dir : string option;
  keep_simple : bool
}

let mk output_format state_encoding clock_type only_bool_predicates input_file input_dir keep_simple =
  {
    output_fmt = (match output_format with 
    |"nusmv" -> if not only_bool_predicates then raise(Invalid_argument ("The NuSMV format only accept boolean predicates.")); NuSMV
    |"vmtlib"-> VMT
    |_ -> raise(Invalid_argument ("The supported format are nusmv and vmtlib.")));
    state_enc = (match state_encoding with
    |"integer"-> IntegerEncoding
    |"boolean"-> BooleanEncoding
    |_ -> raise(Invalid_argument ("The supported state encoding are integer and boolean.")));
    clock_t=(match clock_type with 
    |"integer"-> IntegerClock
    |"real"-> RealClock
    |_ -> raise(Invalid_argument ("The supported clock encoding are integer and real.")));
    only_bool_predicates = only_bool_predicates;
    input_file = input_file;
    input_dir = input_dir;
    keep_simple = keep_simple
  }



  let get () = 
    let file = ref None in
    let dir = ref None in 
    let output_fmt = ref "nusmv" in
    let simple_exp = ref false in
    let which_clock = ref "integer" in
    let state_encode = ref "integer" in
    let bool_only_predicates = ref false in
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
      ("--clock-encoding",
        Arg.String (fun s -> which_clock := s),
        (fill "clock")^"Specify the kind of clock to use : integer or real");  
      ("--state-encoding",
        Arg.String (fun s -> state_encode := s),
        (fill "state-encoding")^"Specify the variable type to encode the SUP state : integer or boolean");  
      ("--bool-only-predicates",
        Arg.Bool (fun b -> bool_only_predicates := b),
        (fill "bool-only-predicates")^"If true, convert predicates that involves not boolean variable into boolean predicates");    
    ] in 
    Arg.parse speclist print_endline usage;

    (mk !output_fmt !state_encode !which_clock !bool_only_predicates !file !dir !simple_exp , usage)