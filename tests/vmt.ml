open Src.Input_args

let get_args file =
  { output_fmt = VMT;
  state_enc = BooleanEncoding;
  clock_t = IntegerClock;
  only_bool_predicates = false ;
  input_file = Some file;
  input_dir= None;
  keep_simple = false;
  check_non_vacuity = []
  } 

let convert_to_absolute_path f= 
  let dir = Filename.dirname (Sys.getcwd()) in
  let rec go_up d =
    let l = List.rev (String.split_on_char '/' d) in 
      match l with
      | _::[]
      | [] -> assert false
      | h::t -> (let new_dir  =String.concat "/" (List.rev t) in if String.equal (h) "_build" then new_dir else go_up new_dir)
  in (go_up dir)^"/"^f

let exec args =
  try 
  let t =  (Src.Parse.of_file (Option.get args.input_file)) in 
  let fmt = Format.get_std_formatter() in
  (match args.output_fmt with
    | NuSMV  -> Src.Sup.generate_sup_file fmt t args
    | VMT  -> Src.Vmt.generate_vmt_file fmt t args);
  with Src.Parse.ParseException msg -> Format.printf "%s@." msg 


let%expect_test "1.req" =
  let f = convert_to_absolute_path "tests/inputs/1.req" in
  let args =get_args f in 
  exec args;
  [%expect {|ok|}]