
type output_format = NuSMV | VMT 

type state_encoding = IntegerEncoding | BooleanEncoding

type clock_type = IntegerClock | RealClock

type t = { 
  output_fmt : output_format;
  state_enc: state_encoding;
  clock_t : clock_type;
  clock_mult: int;
  only_bool_predicates: bool;
  input_file : string option;
  input_dir : string option;
  keep_simple : bool;
  check_non_vacuity : string list;
  check_rt_consistency : bool;
  alpha_bound : int
}

let mk output_format state_encoding clock_type clock_mult only_bool_predicates input_file input_dir keep_simple req_vacuity check_rt_consistency alpha =
  {
    output_fmt = (match output_format with 
    |"sup" -> if (((input_file <> None) || (input_dir <> None))&& (not only_bool_predicates)) then raise(Invalid_argument ("The SUP format requires boolean predicates only.")); NuSMV
    |"vmt"-> VMT
    |_ -> raise(Invalid_argument ("The supported formats are sup and vmt.")));
    state_enc = (match state_encoding with
    |"integer"-> IntegerEncoding
    |"boolean"-> BooleanEncoding
    |_ -> raise(Invalid_argument ("The supported state encodings are integer and boolean.")));
    clock_t=(match clock_type with 
    |"integer"-> IntegerClock
    |"real"-> (if output_format = "sup" then raise(Invalid_argument ("SUP only supports integer clocks.")) else  RealClock)
    |_ -> raise(Invalid_argument ("The supported clock encodings are integer and real.")));
    only_bool_predicates = only_bool_predicates;
    input_file = input_file;
    input_dir = input_dir;
    keep_simple = keep_simple;
    check_non_vacuity = req_vacuity;
    check_rt_consistency = check_rt_consistency;
    clock_mult =(match clock_type with 
          |"integer"-> clock_mult
          |"real" -> 1
          |_ -> raise(Invalid_argument ("The supported clock encodings are integer and real."))); 
    alpha_bound = alpha
  }



  let get () = 
    let file = ref None in
    let dir = ref None in 
    let output_fmt = ref "sup" in
    let simple_exp = ref false in
    let which_clock = ref "integer" in
    let state_encode = ref "boolean" in
    let clock_mult = ref 1 in
    let bool_only_predicates = ref true in
    let check_rt_consistency = ref false in
    let vacuity = ref "" in
    let alpha = ref (-1) in
    let fill s =
      let l = (25-(String.length s)) in
      if l < 0 then ""
      else String.make l ' '
    in
    let line_length = 73 in
    let dash_line = (String.make line_length '-')^"\n" in
    let usage = "\n"^dash_line^" User options:\n"^dash_line in
    let speclist = [
      ("--input",
        Arg.String (fun s -> file := Some s),
        (fill "input")^"Requirement file to process. This option cannot be used with --input-dir.");  
      ("--input-dir",
        Arg.String (fun s -> dir := Some s),
        (fill "input-dir")^"Directory where all requirements files are processed in order to extract a single requirement for each construction kind. This option cannot be used with --input.");  
      ("--simple-exp",
        Arg.Bool (fun b -> simple_exp := b),
        (fill "simple_exp")^"When used with --input-dir keep the most simple (true) or complex (false, default) expressions for the requirements.");  
      ("--output-fmt",
        Arg.String (fun s -> output_fmt := s),
        (fill "output-fmt")^"Specify the generated file format : sup (default) or vmt.");  
      ("--clock-encoding",
        Arg.String (fun s -> which_clock := s),
        (fill "clock-encoding")^"Specify the kind of clock to use : integer (default) or real.");  
      ("--clock-multiplier",
        Arg.Int (fun i -> clock_mult := i),
        (fill "clock-multiplier")^"Specify the multiplier to use for integer clocks (default 1).");  
      ("--state-encoding",
        Arg.String (fun s -> state_encode := s),
        (fill "state-encoding")^"Specify the variable type to encode the SUP state : boolean (default) or integer.");  
      ("--bool-only-predicates",
        Arg.Bool (fun b -> bool_only_predicates := b),
        (fill "bool-only-predicates")^"If true, convert predicates that involve non boolean variables into boolean predicates (default is false).");   
      ("--check-non-vacuity",
        Arg.String (fun s -> vacuity := s),
        (fill "check-non-vacuity")^"If a list of requirements ids separated by ; is given, the non vacuity will be checked only on those requirements. Otherwise it is tested on no requirements (default).");
      ("--alpha",
        Arg.Int (fun b -> alpha := b),
        (fill "alpha")^"Time bound for partial rt-consistency checking (all time constants above alpha are transformed to infinirt).");
     ] in 
    Arg.parse speclist print_endline usage;
    let reqs_to_check_for_vacuity = if !vacuity = "" then [] else 
        (String.split_on_char ';' !vacuity)
    in 
    (mk !output_fmt !state_encode !which_clock !clock_mult !bool_only_predicates !file !dir !simple_exp reqs_to_check_for_vacuity !check_rt_consistency !alpha, usage)