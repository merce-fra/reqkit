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

val get : unit -> (t * string)
