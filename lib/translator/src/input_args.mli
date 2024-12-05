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

val get : unit -> (t * string)
